# Written by Claude, claude-sonnet-4-6
# Guiding prompt: Prototype the rearranged @makeMeasure macro with leading conversion factors:
#   scalar:  @makeMeasure x ExistingUnitType = y NewUnitType "abbrev"
#   affine:  @makeMeasure (toBaseFn) ExistingUnitType = (fromBaseFn) NewUnitType "abbrev"
# The equation x*ExistingUnit = y*NewUnit defines the conversion ratio.
#
# Actual parsing (discovered by running):
#   `@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"` arrives as 4 args:
#     xFactor      = 1e-3           (Float64 — spaces are NOT juxtaposition)
#     relation     = :(Meter = 1)   (Expr — `=` binds Meter and the y-factor together)
#     newType      = :MilliMeter    (Symbol)
#     newAbbreviation = "mm"        (String)
#   Lambda factors must be parenthesized: without parens, `->` consumes the type
#   symbol — `f->(f+459.67)*5/9 Kelvin` parses as `f->((f+459.67)*5/9 * Kelvin)`.

module MakeMeasureParsing

  abstract type AbstractMeasure end

  struct UnitTypeAttributes
    abstract::DataType
    base::DataType
    toBase::Function
    fromBase::Function
    abbreviation::String
    isAffine::Bool
  end
  const allUnitTypes = Dict{DataType, UnitTypeAttributes}()

  macro makeBaseMeasure(quantityName, unitName, abbreviation::String)
    abstractName = Symbol("Abstract" * String(quantityName))
    qts = [quote
      abstract type $abstractName <: AbstractMeasure end
      export $abstractName

      struct $unitName <: $abstractName
        value::Float64
      end
      $unitName(x::T where T <: $abstractName) = $unitName(MakeMeasureParsing.allUnitTypes[typeof(x)].toBase(x.value))
      export $unitName

      MakeMeasureParsing.allUnitTypes[$unitName] = MakeMeasureParsing.UnitTypeAttributes(
        $abstractName, $unitName, x->x, x->x, $abbreviation, false)
    end]
    return esc(Expr(:block, qts...))
  end

  # New @makeMeasure syntax: @makeMeasure x ExistingUnitType = y NewUnitType "abbrev"
  #
  # How Julia parses the call (spaces prevent juxtaposition, `=` has low precedence):
  #   xFactor      ← standalone leading arg  (scalar or parenthesized lambda)
  #   relation     ← Expr(:(=), existingType, yFactor)  ("ExistingType = y")
  #   newType      ← standalone trailing Symbol
  #   newAbbreviation ← trailing String
  #
  # Semantics (xFactor * ExistingUnit = yFactor * NewUnit):
  #   scalar:   toBase(x)   = x * xFactor / yFactor   (NewUnit → ExistingUnit)
  #             fromBase(x) = x * yFactor / xFactor   (ExistingUnit → NewUnit)
  #   function: toBase   = xFactor  (NewUnit → ExistingUnit)
  #             fromBase = yFactor  (ExistingUnit → NewUnit)
  macro makeMeasure(xFactor, relation, newType, newAbbreviation)
    existingType = relation.args[1]  # e.g. :Meter or :Kelvin
    yFactor      = relation.args[2]  # e.g. 1 or (k->k*9/5-459.67)

    isFnFactor = xFactor isa Expr && xFactor.head == :(->)

    toBase   = isFnFactor ? xFactor : :(x -> x * $xFactor / $yFactor)
    fromBase = isFnFactor ? yFactor : :(x -> x * $yFactor / $xFactor)
    isAffine = contains(string(toBase), "+") || contains(string(fromBase), "+")

    qts = [quote
      existingSupertype = supertype($existingType)

      struct $newType <: existingSupertype
        value::Float64
      end
      $newType(x::T where T <: existingSupertype) = $newType(
        MakeMeasureParsing.allUnitTypes[typeof(x)].toBase(x.value))
      export $newType

      MakeMeasureParsing.allUnitTypes[$newType] = MakeMeasureParsing.UnitTypeAttributes(
        existingSupertype, $existingType, $toBase, $fromBase, $newAbbreviation, $isAffine)
    end]
    return esc(Expr(:block, qts...))
  end

  # --- scalar test: 1e-3 Meter = 1 MilliMeter ---
  @makeBaseMeasure Length Meter "m"
  @makeMeasure 1e-3 Meter = 1 MilliMeter "mm"

  @show allUnitTypes[MilliMeter].toBase(1)    # → 0.001
  @show allUnitTypes[MilliMeter].fromBase(1)  # → 1000.0
  @show allUnitTypes[MilliMeter].isAffine     # → false
  @assert isapprox(allUnitTypes[MilliMeter].toBase(1),   1e-3)
  @assert isapprox(allUnitTypes[MilliMeter].fromBase(1), 1000.0)
  @assert !allUnitTypes[MilliMeter].isAffine

  # --- affine test: parenthesized lambdas required ---
  # `(f->(f+459.67)*5/9) Kelvin = (k->k*9/5-459.67) Fahrenheit "°F"` gives:
  #   xFactor  = :(f->(f+459.67)*5/9)
  #   relation = :(Kelvin = (k->k*9/5-459.67))
  #   newType  = :Fahrenheit
  @makeBaseMeasure Temperature Kelvin "K"
  @makeMeasure (f->(f+459.67)*5/9) Kelvin = (k->k*9/5-459.67) Fahrenheit "°F"

  @show allUnitTypes[Fahrenheit].toBase(32)        # → 273.15
  @show allUnitTypes[Fahrenheit].fromBase(273.15)  # → 32.0
  @show allUnitTypes[Fahrenheit].isAffine          # → true
  @assert isapprox(allUnitTypes[Fahrenheit].toBase(32),       273.15, atol=1e-8)
  @assert isapprox(allUnitTypes[Fahrenheit].fromBase(273.15), 32.0,   atol=1e-5)
  @assert allUnitTypes[Fahrenheit].isAffine

  # --- non-affine function factor: Rankine (no + in either direction) ---
  @makeMeasure (x->x*5/9) Kelvin = (x->x*9/5) Rankine "°R"

  @show allUnitTypes[Rankine].toBase(9)  # → 5.0
  @show allUnitTypes[Rankine].isAffine   # → false
  @assert isapprox(allUnitTypes[Rankine].toBase(9), 5.0)
  @assert !allUnitTypes[Rankine].isAffine

  # --- conversion constructor test ---
  mm5 = MilliMeter(5.0)
  @show Meter(mm5)  # → Meter with value ≈ 0.005
  @assert isapprox(Meter(mm5).value, 0.005)

end
