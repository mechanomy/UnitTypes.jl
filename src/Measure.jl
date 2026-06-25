export AbstractMeasure, @makeBaseMeasure, @makeMeasure, @relateMeasures, toBaseFloat, abbreviation, @u_str, displayUnitTypes, getDimensions, mergeBaseDimensions, findNamedType
abstract type AbstractMeasure end

struct UnitTypeAttributes
  abstract::DataType # the abstract type of this type, say AbstractLength for MilliMeter
  base::DataType # Meter
  toBase::Function # function to numerically convert to the base unit; this would be overkill except for handling affine units
  fromBase::Function # function to numerically convert from the base unit
  abbreviation::String # "mm"
  isAffine::Bool
  dimensions::Dict{DataType,Int} # maps abstract dimension types to exponents, e.g. {AbstractLength=>1, AbstractTime=>-1}
end
const allUnitTypes = Dict{DataType, UnitTypeAttributes}() # const means allUnitTypes won't change from a Dict https://discourse.julialang.org/t/mutating-global-variable-during-precompilation/51478/2?u=bcon

# Registry mapping (AbstractType, exponent) → base result type, populated by addRelations.
# Used by registerPower (and makeJointConversions) to emit zero-alloc literal_pow methods.
const powerTypes = Dict{Tuple{DataType,Int}, DataType}()

"""
  `abbreviation(m::AbstractMeasure)::String`

  Returns the unit string for `m`.
"""
function abbreviation(m::AbstractMeasure)::String
  return UnitTypes.allUnitTypes[typeof(m)].abbreviation
end
@testitem "abbreviation()" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @test UnitTypes.abbreviation(MeterT(3.4)) == "mT"
end

function displayUnitTypes()
  println("allUnitTypes=")
  display(sort(collect(allUnitTypes), by=x->string(x[1]))) # alphabetize and display
end

"""
  `macro makeBaseMeasure(quantityName, unitName, abbreviation::String, isAffine=false)`

  Make a new base measure which has no relationship to an existing unit.
  For example, in `@makeBaseMeasure Length Meter "m"`:
  * `quantityName` is the name of the measure, 'Length' above.
  * `unitName` is the name of the unit which will be used to make measures bearing that unit, 'Meter' above.
  * `abbreviation` is the abbreviation of the unit name, used in all string presentations of the measure.
  The macro will introduce `AbstractLength <: AbstractMeasure` and `Meter()` into the current scope.
  
  To get the measure's value in the base unit as a float, see toBaseFloat().
"""
macro makeBaseMeasure(quantityName, unitName, abbreviation::String)
  # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName abbreviation:$abbreviation")
  abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength

  if unitName in keys(UnitTypes.allUnitTypes)
    @warn "$unitName is already defined, skipping"
    return
  end

  qts = [quote
      abstract type $abstractName <: UnitTypes.AbstractMeasure end
      export $abstractName #AbstractLength

      """
        This UnitType represents a basic measure of $($unitName) with units $($abbreviation).
      """
      struct $unitName <: $abstractName
        value::Float64 # the value on creation as measured in the unit
      end
      $unitName(x::T where T<:$abstractName) = $unitName( UnitTypes.allUnitTypes[typeof(x)].toBase(x.value) ) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
      export $unitName

      UnitTypes.allUnitTypes[$unitName] = UnitTypes.UnitTypeAttributes($abstractName, $unitName, x->x, x->x, $abbreviation, false, Dict{DataType,Int}($abstractName => 1)) # A base unit cannot be converted into another base unit, so its to/fromBase functions are =

      UnitTypes.makeSelfConversion($unitName, @__MODULE__)
      # UnitTypes.makeJointConversions($unitName) # define operations only on itself, as there cannot be any child measures yet
    end
  ]
  return esc( Expr(:block, qts...))
end
@testitem "@makeBaseMeasure" begin
  @makeBaseMeasure MeterTest MeterT "metT" 
  # @makeMeasure MeterT(1) = MilliMeterT(1000) "miliT" # u_str needs symbol uniqueness! when this was "mmT", it seemed to conflict with the @makeMeasure test definition of "mmT" and this conflict showed up as LoadError: UndefVarError: `MiliT` not defined in `Main.var"##233"`

  @testset "did the macro create the definitions we expect" begin
    @test isdefined(@__MODULE__, :AbstractMeterTest)
    @test isdefined(@__MODULE__, :MeterT)

    @makeBaseMeasure DensityTest DensityT "dennyT"
    @test MeterT(1.2)*DensityT(3.4) isa Catchall #catch-all returns Catchall when no @relateMeasures is defined
  end
  
  @testset "constructor" begin
    @test MeterT(3.4).value == 3.4
    # @show MeterT(2im).value # should imaginary error?
    @test MeterT(MeterT(3.4)).value == 3.4
  end

  @testset "allUnitTypes is populated" begin
    @test haskey(UnitTypes.allUnitTypes, MeterT)
    @test UnitTypes.allUnitTypes[MeterT].abstract == AbstractMeterTest
    @test UnitTypes.allUnitTypes[MeterT].base == MeterT
    @test UnitTypes.allUnitTypes[MeterT].toBase(10) ≈ 10
    @test UnitTypes.allUnitTypes[MeterT].fromBase(10) ≈ 10
    @test UnitTypes.allUnitTypes[MeterT].abbreviation == "metT"
  end
end

"""
  `macro makeMeasure(xFactor, relation, newType, newAbbreviation)`

  Creates a new Measure from an existing base measure using leading conversion factors.
  The ExistingUnitType must already exist; NewUnitType will be created.
  The equation `xFactor * ExistingUnitType = yFactor * NewUnitType` defines the conversion.
  ```
  @makeMeasure 1e-3 Meter = 1 MilliMeter "mm"
  ```

  Affine units (like Temperature) provide parenthesized anonymous functions as factors.
  xFactor converts NewUnitType values to ExistingUnitType; yFactor is the inverse.
  ```
  @makeMeasure (f->(f+459.67)*5/9) Kelvin = (k->k*9/5-459.67) Fahrenheit "°F"
  ```
"""
macro makeMeasure(xFactor, relation, newType, newAbbreviation)
  # xFactor      : scalar or (parenthesized) lambda — leading conversion factor for ExistingType
  # relation     : Expr(:(=), existingType, yFactor) — parsed from "ExistingType = yFactor"
  # newType      : Symbol for the new unit type
  # newAbbreviation : String abbreviation
  existingType = relation.args[1]
  yFactor      = relation.args[2]

  isFnFactor = xFactor isa Expr && xFactor.head == :(->)
  toBase   = isFnFactor ? xFactor : :(x -> x * $xFactor / $yFactor)
  fromBase = isFnFactor ? yFactor : :(x -> x * $yFactor / $xFactor)
  isAffine = contains(string(toBase), "+") || contains(string(fromBase), "+")

  qts = [quote
    existingSupertype = supertype($existingType)

    """
      This UnitType represents a basic measure of $($newType) with units $($newAbbreviation).
    """
    struct $newType <: existingSupertype
      value::Float64 # the value on creation as measured in the unit
    end
    $newType(x::T where T<:existingSupertype) = $newType( $fromBase(UnitTypes.allUnitTypes[typeof(x)].toBase(x.value) )) # conversion constructor
    export $newType

    UnitTypes.allUnitTypes[$newType] = UnitTypes.UnitTypeAttributes(existingSupertype, $existingType, $toBase, $fromBase, $newAbbreviation, $isAffine, UnitTypes.allUnitTypes[$existingType].dimensions)

    UnitTypes.makeSelfConversion($newType, @__MODULE__)
    UnitTypes.makeJointConversions($newType, @__MODULE__)
  end]
  return esc( Expr(:block, qts...))
end
@testitem "makeMeasure" begin
  @makeBaseMeasure LengthTest MeterT "mT"
  @makeBaseMeasure TemperatureTest KelvinT "KT"

  @makeMeasure (f->(f+459.67)*5/9) KelvinT = (k->k*9/5-459.67) FahrenheitT "FT"
  @makeMeasure (x->x/100) MeterT = (x->x*100) CentiMeterT "cmT"
  @makeMeasure 25.4/1000 MeterT = 1 InchT "inT"
  @makeMeasure 1/1000 MeterT = 1 KiloMeterT "KmT"

  @makeMeasure 12 InchT = 1 FootT "ft"

  @testset "subtyped" begin
    @test KelvinT <: AbstractMeasure
    @test KelvinT <: AbstractTemperatureTest
    @test FahrenheitT <: AbstractTemperatureTest
  end

  @testset "to/fromBase" begin # check that the functions work
    @test UnitTypes.allUnitTypes[FahrenheitT].toBase(32) ≈ 273.15
    @test UnitTypes.allUnitTypes[FahrenheitT].fromBase(273.15) ≈ 32
    @test UnitTypes.allUnitTypes[CentiMeterT].toBase(123) ≈ 1.23
    @test UnitTypes.allUnitTypes[CentiMeterT].fromBase(1.23) ≈ 123
    @test UnitTypes.allUnitTypes[InchT].toBase(1) ≈ 0.0254 
    @test UnitTypes.allUnitTypes[InchT].fromBase(1) ≈ 39.3700787
  end

  @testset "conversion constructor" begin
    @test isapprox(InchT(CentiMeterT(10)), InchT(3.937007), atol=1e-5)
  end

  # @testset "erroneous/re-definition" begin
    # re macroexpand() see https://github.com/JuliaLang/julia/issues/56733
    # @test_warn "MilliMeterT is already defined, cannot re-define" macroexpand(@__MODULE__, :( @makeMeasure2 MeterT = CentiMeterT "cmT" x->x/100 x->x*100  ))

    # @test_warn "mmT is already used by MilliMeterT, suggest choosing a unique unit" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  ))
    # @test_warn "mmT is already used by Main.var\"###236\".MilliMeterT, suggest choosing a unique unit" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  )) # 236 can change, there should be a better reference.. disabling check now
    # @test true # @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  

    # @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterTNot(1) = MilliMeterT3(5000) "mmT3"  )) # error on LHS not existing
    # @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterT(1)*Seconds(3) = MilliMeterTS(5000) "mmTS"  )) # error on compound relations
  # end
end

"""
  `struct UnitStepRange{T<:AbstractMeasure}`

  A lazy, allocation-free stepped range over a consistent unit type.
  Constructed by the `start:step:stop` colon syntax when all three share the same abstract dimension.
  The element type is determined by `start`; `step` and `stop` are converted to match.
"""
struct UnitStepRange{T<:AbstractMeasure} <: AbstractVector{T}
  start::T
  step::T
  stop::T
end
Base.size(r::UnitStepRange) = (max(0, floor(Int, (r.stop.value - r.start.value) / r.step.value) + 1),)
Base.getindex(r::UnitStepRange{T}, i::Int) where T = T(r.start.value + (i - 1) * r.step.value)
export UnitStepRange

Base.Broadcast.broadcastable(x::AbstractMeasure) = Ref(x)
Base.:*(r::AbstractRange{<:Number}, m::T) where T<:AbstractMeasure = T.(r .* m.value)
Base.:*(m::T, r::AbstractRange{<:Number}) where T<:AbstractMeasure = T.(m.value .* r)

"""
  makeSelfConversions(newType)

  Defines function overloads for common operations like `convert`, `isapprox`, `isequal`, `isless`, +-/* between instances of a new UnitType.
"""
function makeSelfConversion(newType, mod=@__MODULE__)
  # something about lowering requires passing the calling module from @makeBaseMeasure into here as just calling @__MODULE__ is undefined.
  uta = allUnitTypes[newType]
  abstractType = uta.abstract
  if uta.isAffine
    # if affine, we can only +- within the type, can't Kelvin+Celsius without messing around with zero points.
    mod.eval( quote 
      if !hasmethod(Base.:+, Tuple{$newType, $newType})
        Base.:+(x::$newType, y::$newType) = $newType(x.value + y.value) 
      end
      if !hasmethod(Base.:-, Tuple{$newType, $newType})
        Base.:-(x::$newType, y::$newType) = $newType(x.value - y.value) 
      end
    end)
  end

  mod.eval( quote 
    if !hasmethod(Base.isapprox, Tuple{$newType, $newType})
      Base.isapprox(x::$newType, y::$newType; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol) 
    end

    if !hasmethod(Base.isless, Tuple{$newType, $newType} )
      Base.isless(x::$newType, y::$newType) = x.value < y.value 
    end

    if !hasmethod(Base.:+, Tuple{Number, $newType})
      Base.:+(x::Number, y::$newType) = $newType(x+y.value) 
    end
    if !hasmethod(Base.:+, Tuple{$newType, Number})
      Base.:+(x::$newType, y::Number) = $newType(x.value+y) 
    end
    if !hasmethod(Base.:-, Tuple{Number, $newType})
      Base.:-(x::Number, y::$newType) = $newType(x-y.value) 
    end
    if !hasmethod(Base.:-, Tuple{$newType, Number})
      Base.:-(x::$newType, y::Number) = $newType(x.value-y) 
    end

    if !hasmethod(Base.:+, Tuple{$newType, $newType})
      Base.:+(x::$newType, y::$newType) = $newType(x.value+y.value) 
    end
    if !hasmethod(Base.:-, Tuple{$newType, $newType})
      Base.:-(x::$newType, y::$newType) = $newType(x.value-y.value) 
    end
    if !hasmethod(Base.:-, Tuple{$newType})
      Base.:-(x::$newType) = x * -1 # leading negation
    end

    if !hasmethod(Base.:*, Tuple{$newType, Number})
      Base.:*(x::$newType, y::U) where U<:Number = $newType(x.value*y) 
      Base.:*(x::T, y::$newType) where T<:Number = $newType(x*y.value)
    end

    if !hasmethod(Base.:/, Tuple{$newType, Number})
      Base.:/(x::$newType, y::U) where U<:Number = $newType(x.value/y)
    end

    if !hasmethod(Base.rem, Tuple{$newType, $newType})
      Base.rem(x::$newType, y::$newType) = $newType(rem(x.value, y.value, RoundNearest)) # required for _colon
      # Base.rem(x::$newType, y::$newType, r::RoundingMode) = $newType(rem(x.value, y.value, r)) ..define others? https://github.com/JuliaLang/julia/blob/d665f8980f2bada7cd87fd79610ab769e44e95f7/base/div.jl#L114
    end

    if !hasmethod(Base.zero, Tuple{$newType})
      Base.zero(x::$newType) = $newType(0) #zero() seems to be required for _colon()
    end

    if !hasmethod(Base.:(:), Tuple{$newType, $abstractType, $abstractType})
      Base.:(:)(start::$newType, step::$abstractType, stop::$abstractType) = UnitTypes.UnitStepRange(start, $newType(step), $newType(stop))
    end
    if !hasmethod(Base.:(:), Tuple{$newType, $abstractType})
      Base.:(:)(start::$newType, stop::$abstractType) = UnitTypes.UnitStepRange(start, $newType(1), $newType(stop))
    end
    (::Type{$newType})(r::AbstractRange{<:Number}) = $newType.(r)

  end)

end
@testitem "makeSelfConversion" begin
  @makeBaseMeasure LengthTest MeterT "mT" 
  @makeBaseMeasure SoundTest GrowlT "gT" 
  @makeBaseMeasure TemperatureTest KelvinT "KT"

  @testset "convert" begin
    @test isa( convert(MeterT, MeterT(1.2)), MeterT)
    @test_throws MethodError convert(GrowlT, MeterT(1.2)) #  MethodError: Cannot `convert` an object of type Main.var"##238".MeterT to an object of type Main.var"##238".GrowlT
  end

  @testset "isapprox" begin
    @test MeterT(1.2) ≈ MeterT(1.2)
  end

  @testset "isequal" begin
    @test MeterT(3.4) == MeterT(3.4)
    @test MeterT(3.4) != MeterT(1.2)
    @test MeterT(3.4) != GrowlT(3.4) 
  end

  @testset "less than" begin
    @test MeterT(1.2) < MeterT(3.4)
    @test MeterT(1.2) <= MeterT(3.4)
    @test MeterT(3.4) > MeterT(1.2)
    @test MeterT(3.4) >= MeterT(1.2)
    @test_throws MethodError MeterT(1.2) < GrowlT(3.4)
  end

  @testset "addition" begin
    @test MeterT(1) + MeterT(10) ≈ MeterT(11)
    @test KelvinT(1) + KelvinT(2) ≈ KelvinT(3)
    @test length(methods(Base.:+, (MeterT, MeterT))) == 1
  end

  @testset "subtraction" begin
    @test MeterT(2) - MeterT(0.1) ≈ MeterT(1.9)
  end

  @testset "multiply divide" begin
    @test MeterT(3)*3 ≈ MeterT(9)
    @test 3*MeterT(3) ≈ MeterT(9)
    @test MeterT(3)/3 ≈ MeterT(1)

    @test -MeterT(3) ≈ MeterT(-3)
    @test MeterT(5) + MeterT(-3) ≈ MeterT(2)
    @test MeterT(5) + -MeterT(3) ≈ MeterT(2)
  end

  @testset "broadcasting" begin
    @test isa([1,2,3] .* MeterT(4), Vector{MeterT})
    for m in MeterT.([1,2,3])
      @test m≈MeterT(1) || m≈MeterT(2) || m≈MeterT(3)
    end
  end
  @testset "range unit multiply" begin
    a = MeterT(1:.1:5)
    @test a[1] ≈ MeterT(1)

    b = MeterT(1:5)
    @test b[1] ≈ MeterT(1)

    c = (1:5)u"mT"
    @test c[1].value ≈ 1.0

    d = (1:.1:5)u"mT"
    @test d[1].value ≈ 1.0
  end
 
  @testset "range" begin
    c = LinRange(MeterT(10), MeterT(20), 4)
    @test c[1] ≈ MeterT(10)
    @test c[2] ≈ MeterT(13+1/3)
    @test last(c) ≈ MeterT(20)
  end

  @testset "range _colon" begin
    b = MeterT(1) : MeterT(0.3) : MeterT(2)
    @test b[1] ≈ MeterT(1)
    @test b[2] ≈ MeterT(1.3)
    @test last(b) ≈ MeterT(1.9)

    @test_throws MethodError GrowlT(1) : MeterT(0.3) : MeterT(2)
    @test_throws MethodError MeterT(1) : GrowlT(0.3) : MeterT(2)
    @test_throws MethodError MeterT(1) : MeterT(0.3) : GrowlT(2)

    b = MeterT(2) : MeterT(-0.3) : MeterT(1)
    @test b[1] ≈ MeterT(2)

    b = MeterT(1) : MeterT(1) : MeterT(5)
    @test b[1] ≈ MeterT(1)
    @test b[5] ≈ MeterT(5)

    c = MeterT(1) : MeterT(3)
    @test c[1] ≈ MeterT(1)
    @test c[2] ≈ MeterT(2)
    @test last(c) ≈ MeterT(3)
    c = MeterT(1) : MeterT(5)
    @test c[1] ≈ MeterT(1)
    @test last(c) ≈ MeterT(5)
  end
end

"""
  makeJointConversions(newType) 
  
  Defines function overloads for common operations like `convert`, `isapprox`, `isequal`, `isless`, +-/* between a new UnitType and all existing UnitTypes.
"""
function makeJointConversions(newType=nothing, mod=@__MODULE__) # optional argument to only run on the newly created type, for makeMeasure()
  for a in Dict(newType=>allUnitTypes[newType]) # match structure of allUnitTypes
    for b in allUnitTypes 
      if supertype(a.first) == supertype(b.first) # can only convert/isapprox/+- within the same measures
        mod.eval( quote
          if !hasmethod(Base.convert, (Type{$(a.first)}, $(b.first)) )
            Base.convert(::Type{$(a.first)}, y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(b.second.toBase)(y.value)) ) # convert b to its base, then a fromBase
          end
          if !hasmethod(Base.convert, (Type{$(b.first)}, $(a.first)) )
            Base.convert(::Type{$(b.first)}, y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(a.second.toBase)(y.value)) )
          end

          # specific outer constructors: more specific than the generic conversion constructor,
          # so Julia dispatches here first — captured functions mean zero runtime dict lookup.
          # Use (::Type{T})(y::S) = ... syntax because $(a.first) interpolates as a qualified
          # name (UnitTypes.Foo) which is not valid as a bare function definition target.
          if !any(m -> m.sig == Tuple{Type{$(a.first)}, $(b.first)}, methods($(a.first)))
            (::Type{$(a.first)})(y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(b.second.toBase)(y.value)) )
          end
          if !any(m -> m.sig == Tuple{Type{$(b.first)}, $(a.first)}, methods($(b.first)))
            (::Type{$(b.first)})(y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(a.second.toBase)(y.value)) )
          end

          if !hasmethod(Base.isapprox, ($(a.first), $(b.first)) )
            Base.isapprox(x::$(a.first), y::$(b.first); atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert($(a.first),y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?
          end
          if !hasmethod(Base.isapprox, ($(b.first), $(a.first)) )
            Base.isapprox(x::$(b.first), y::$(a.first); atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert($(b.first),y).value, atol=atol, rtol=rtol) 
          end

          if !hasmethod(Base.isequal, ($(a.first), $(b.first)) )
            Base.isequal(x::$(a.first), y::$(b.first)) = x.value == convert(($a.first),y).value
          end
          if !hasmethod(Base.isequal, ($(b.first), $(a.first)) )
            Base.isequal(x::$(b.first), y::$(a.first)) = x.value == convert(($b.first),y).value
          end

          if !hasmethod(Base.isless, ($(a.first), $(b.first)) )
            Base.isless(x::$(a.first), y::$(b.first)) = x.value < convert($(a.first),y).value # other <> ops are defined from this
          end
          if !hasmethod(Base.isless, ($(b.first), $(a.first)) )
            Base.isless(x::$(b.first), y::$(a.first)) = x.value < convert($(b.first),y).value # other <> ops are defined from this
          end
        end)
        # Base.:+(<:Number) not implemented to prevent random numbers from assuming UnitTypes, the point is to be explicit

        mod.eval( quote
          if !($(a.second.isAffine) || $(b.second.isAffine))
            if !hasmethod(Base.:+, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(a.second.toBase)(x.value) + $(b.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:+, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(b.second.toBase)(x.value) + $(a.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:-, ($(a.first),$(b.first))) 
              Base.:-(x::$(a.first), y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(a.second.toBase)(x.value) - $(b.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:-, ($(b.first),$(a.first))) 
              Base.:-(x::$(b.first), y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(b.second.toBase)(x.value) - $(a.second.toBase)(y.value))) 
            end
          end

          # if affine, we can only +- within the type, can't Kelvin+Celsius without messing around with zero points
          if $(a.second.isAffine) && $(b.second.isAffine) && $(a.first)==$(b.first) 
            if !hasmethod(Base.:+, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)(x.value + y.value)
            end
            if !hasmethod(Base.:+, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)(x.value + y.value)
            end
            if !hasmethod(Base.:-, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)(x.value - y.value)
            end
            if !hasmethod(Base.:-, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)(x.value - y.value)
            end
          end
        end)
      end

      # # add */ if the resultant type already exists...
      # if hasmethod(Base.:/, (a.first, b.first)) # 
      # # if supertype(a.first) == supertype(b.first) # can only convert/isapprox/+- within the same measures
      #   baseA = getBaseType(a.first)
      #   baseB = getBaseType(b.first)
      #   println("adding math with a:$(a.first)::$baseA, $(b.first)::$baseB")
      #        if !hasmethod(Base.:*, ($(a.first), $(b.first))) 
      #         Base.:*(x::$(a.first), y::$(b.first)) = convert($baseA, x)*convert($baseB, y)  #just convert to base and * since it's too hard to find Foot^2 and handle if it doesn't exist yet
      #       end
      #       if !hasmethod(Base.:*, ($(b.first), $(a.first))) 
      #         Base.:*(x::$(b.first), y::$(a.first)) = convert($baseB, x)*convert($baseA, y)
      #       end
      #       # divide works if we have a/b::baseB
      #       if !hasmethod(Base.:/, ($(baseA),$(baseB)))
      #         Base.:/(x::$(a.first), y::$(b.first)) = convert($baseA, x)/convert($baseB,y)
      #       end     

    end
  end

  # define zero-alloc Base.literal_pow methods for any registered integer power relations
  abstractType = supertype(newType)
  toBase = allUnitTypes[newType].toBase
  for ((absT, n), baseResultType) in powerTypes
    absT == abstractType || continue
    fromBase = allUnitTypes[baseResultType].fromBase
    valN = Val{n}
    mod.eval(quote
      if !UnitTypes.hasExactMethod(Base.literal_pow, (typeof(^), $newType, $valN))
        Base.literal_pow(::typeof(^), x::$newType, ::$valN) = $baseResultType($fromBase($toBase(x.value)^$n))
      end
    end)
  end
end
@testitem "makeJointConversions" begin
  @makeBaseMeasure LengthTest MeterT "mtT" 
  @makeBaseMeasure SoundTest GrowlT "gT" 
  @makeBaseMeasure TemperatureTest KelvinT "KT"

  @makeMeasure (x->(x+459.67)*5/9) KelvinT = (k->k*9/5-459.67) FahrenheitT "FT"
  @makeMeasure 1e-2 MeterT = 1 CentiMeterT "cmT"
  @makeMeasure (x->x*0.0254) MeterT = (x->x/0.0254) InchT "inT"

  @testset "convert" begin
    @test isa( convert(MeterT, MeterT(1.2)), MeterT)
    @test isa( convert(MeterT, CentiMeterT(1.2)), MeterT)
    @test isa( convert(CentiMeterT, MeterT(1.2)), CentiMeterT)

    @test isapprox( convert(CentiMeterT, MeterT(1.234)), CentiMeterT(123.4), atol=1e-3)

    @test convert(InchT, CentiMeterT(3.048)).value ≈ 1.2 
    @test convert(CentiMeterT, InchT(1.2)).value ≈ 3.048 

    @test_throws MethodError convert(GrowlT, MeterT(1.2)) #  MethodError: Cannot `convert` an object of type Main.var"##238".MeterT to an object of type Main.var"##238".GrowlT
  end

  @testset "isapprox" begin
    @test InchT(1.2) ≈ InchT(1.2)
    @test InchT(1.2) ≈ CentiMeterT(3.048)
    @test MeterT(0.03048) ≈ CentiMeterT(3.048)
    @test CentiMeterT(3.048) ≈ MeterT(0.03048)
    @test isapprox( FahrenheitT(32), KelvinT(273.15), atol=1e-3)
    @test isapprox( KelvinT(273.15), FahrenheitT(32), atol=1e-3)
  end

  @testset "isequal" begin
    @test MeterT(3.4) == MeterT(3.4)
    @test MeterT(3.4) != MeterT(1.2)
    @test MeterT(3.4) != GrowlT(3.4) 

    @test MeterT(1.2) ≈ CentiMeterT(120)
    @test MeterT(1).value == convert(MeterT, CentiMeterT(100)).value 
  end

  @testset "less than" begin
    @test MeterT(1.2) < MeterT(3.4)
    @test MeterT(1.2) <= MeterT(3.4)
    @test MeterT(3.4) > MeterT(1.2)
    @test MeterT(3.4) >= MeterT(1.2)

    @test CentiMeterT(1.2) < MeterT(1.2)
    @test CentiMeterT(1200) > MeterT(1.2)

    @test_throws MethodError MeterT(1.2) < GrowlT(3.4)
  end

  @testset "addition" begin
    @test MeterT(1) + CentiMeterT(10) ≈ MeterT(1.1)
    @test CentiMeterT(100) + MeterT(1) ≈ MeterT(2)

    @test KelvinT(1) + KelvinT(2) ≈ KelvinT(3)
    @test isapprox(FahrenheitT(1) + FahrenheitT(2), FahrenheitT(3), atol=1e-3)

    @test length(methods(Base.:+, (FahrenheitT, FahrenheitT))) == 1
    @test length(methods(Base.:+, (FahrenheitT, KelvinT))) == 0
    @test length(methods(Base.:+, (KelvinT, FahrenheitT))) == 0
    @test_throws MethodError KelvinT(100) + FahrenheitT(32) # no method matching +, can't add cross-bases by default
  end

  @testset "subtraction" begin
    @test MeterT(2) - CentiMeterT(100) ≈ MeterT(1)
    @test CentiMeterT(200) - MeterT(1) ≈ MeterT(1)
    @test KelvinT(3) - KelvinT(1) ≈ KelvinT(2)
  end

  @testset "multiply divide" begin
    @test MeterT(3)*3 ≈ MeterT(9)
    @test 3*MeterT(3) ≈ MeterT(9)
    @test MeterT(3)/3 ≈ MeterT(1)

    @test -MeterT(3) ≈ MeterT(-3)
    @test MeterT(5) + MeterT(-3) ≈ MeterT(2)
    @test MeterT(5) + -MeterT(3) ≈ MeterT(2)
  end
  
  @makeBaseMeasure AreaT Meter2T "m^2"
  @relateMeasures MeterT*MeterT=Meter2T
  @testset "multiply between types" begin
    @test MeterT(3)*CentiMeterT(1) ≈ Meter2T(0.03)
    @test MeterT(1)*InchT(1) ≈ Meter2T(0.0254)
  end
  @testset "divide between types" begin
    @test Meter2T(4)/MeterT(2) ≈ MeterT(2)
    @test Meter2T(1)/CentiMeterT(1) ≈ MeterT(100)
    @test isapprox(Meter2T(1)/InchT(1), MeterT(1/0.0254), atol=1e-3)
  end

  @testset "broadcasting" begin
    @test isa([1,2,3] .* MeterT(4), Vector{MeterT})
    for m in MeterT.([1,2,3])
      @test m≈MeterT(1) || m≈MeterT(2) || m≈MeterT(3)
    end
  end

  @testset "range" begin
    c = LinRange(MeterT(10), MeterT(20), 4)
    @test c[1] ≈ MeterT(10)
    @test c[2] ≈ MeterT(13+1/3)
    @test last(c) ≈ MeterT(20)
  end

  @testset "range _colon" begin
    b = MeterT(1) : MeterT(0.3) : MeterT(2)
    @test b[1] ≈ MeterT(1)
    @test b[2] ≈ MeterT(1.3)
    @test last(b) ≈ MeterT(1.9)

    bc = MeterT(1) : CentiMeterT(30) : MeterT(2)
    @test bc[1] ≈ MeterT(1)
    @test bc[2] ≈ MeterT(1.3)
    @test last(bc) ≈ MeterT(1.9)

    @test_throws MethodError GrowlT(1) : MeterT(0.3) : MeterT(2)
    @test_throws MethodError MeterT(1) : GrowlT(0.3) : MeterT(2)
    @test_throws MethodError MeterT(1) : MeterT(0.3) : GrowlT(2)
  end
end

function skipSymbolBlock(eexp)
  if eexp.head == :block
    return eexp.args[2]
  else
    return eexp
  end
end

"""
  `function measure2String(m::AbstractMeasure)::String`

  Returns a string representing measure `m` in the format "1.23mm".
"""
function measure2String(m::AbstractMeasure)::String
  # return @sprintf("%3.3f []", m)
  return "$(m.value)$(abbreviation(m))"
end
@testitem "Measure measure2string()" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @test UnitTypes.measure2String(MeterT(3.4)) == "3.4mT"
  @test string(MeterT(3.4)) == "3.4mT"
end

"""
  `function Base.show(io::IO, m::AbstractMeasure)`

  @show functionality for Measures via `measure2String()`.
"""
function Base.show(io::IO, m::AbstractMeasure)
  print(io, measure2String(m))
end

"""
  `function getBaseType(mtype::DataType) :: DataType`
  Returns the base type of some child type, so getBaseType(MilliMeter) returns Meter.
"""
function getBaseType(mtype::DataType) :: DataType
  ret = UnitTypes.allUnitTypes[mtype].base
  # println("getBase: ", mtype, " -> ", ret)
  return ret
end
@testitem "getBaseType" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @test UnitTypes.getBaseType( MeterT ) == MeterT

  @makeMeasure 1e-2 MeterT = 1 CentiMeterT "cmT"
  @test UnitTypes.getBaseType( CentiMeterT ) == MeterT
end

"""
  `function toBaseFloat(m::AbstractMeasure) :: Float64`

  Returns measure `m` as a float in the base unit.
""" 
function toBaseFloat(m::AbstractMeasure) :: Float64
  return UnitTypes.allUnitTypes[typeof(m)].toBase(m.value)
end
@testitem "toBaseFloat" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeMeasure (x->x/100) MeterT = (x->x*100) CentiMeterT "cmT"
  @makeMeasure (x->x*1000) MeterT = (x->x/1000) KiloMeterT "kmT"

  @test toBaseFloat(CentiMeterT(100)) ≈ 1
  @test toBaseFloat(KiloMeterT(1)) ≈ 1000
end

"""
  `macro u_str(unit::String)`

  Macro to provide the 1.2u"cm" inline unit assignment.
  ```
  a = 1.2u"cm" 
  ```

  This works by looking up the unit string in `allUnitTypes` and returning the corresponding type.
  See https://docs.julialang.org/en/v1/manual/metaprogramming/#meta-non-standard-string-literals
"""
macro u_str(unit::String)
  aut = filter( pairKV -> last(pairKV).abbreviation == unit, UnitTypes.allUnitTypes) # last(pairKV) == value == UnitTypeAttributes).abbreviation ==unit
  if !isempty(aut)
    b = first(first(aut))(1) # MeterT(1)
    return b
  end
  result = UnitTypes.parseCatchall(unit) # defined in Catchall.jl, loaded after this file
  result !== nothing && return result
  @warn "did not find $unit in `allUnitTypes` and could not parse as compound unit expression"
end
@testitem "u_str" begin
  @makeBaseMeasure UStrTest UsT "usT"
  @test isa(1u"usT", UsT) 
end

# hasmethod() returns true for parametric catch-alls (e.g. *(T<:AbstractMeasure, U<:AbstractMeasure)),
# which causes addRelations to skip defining specific abstract-type methods like *(AbstractLength, AbstractLength).
# This checks for an exact non-parametric signature only.
function hasExactMethod(f, types)
  target = Tuple{typeof(f), types...}
  return any(m -> m.sig == target, methods(f))
end

"""
  `mergeBaseDimensions(d1, d2, sign=1) -> Dict{DataType,Int}`

  Combines two dimension maps: result = d1 * d2^sign (sign=1 for multiply, -1 for divide).
  Zero-exponent entries are removed so the map stays canonical.
"""
function mergeBaseDimensions(d1::Dict{DataType,Int}, d2::Dict{DataType,Int}, sign::Int=1)::Dict{DataType,Int}
  result = copy(d1)
  for (k, v) in d2
    result[k] = get(result, k, 0) + sign * v
  end
  filter!(kv -> last(kv) != 0, result)
  return result
end

"""
  `findNamedType(dims) -> Union{DataType, Nothing}`

  Returns the registered type whose dimension signature exactly matches `dims`, preferring
  base types (where `allUnitTypes[T].base == T`) over scaled variants like Inch2 vs Meter2.
  Returns nothing if no match exists.
"""
function findNamedType(dims::Dict{DataType,Int})::Union{DataType, Nothing}
  candidate = nothing
  for (T, uta) in allUnitTypes
    if uta.dimensions == dims
      if uta.base == T          # base types beat scaled variants; return immediately
        return T
      end
      candidate === nothing && (candidate = T)  # keep first non-base match as fallback
    end
  end
  return candidate
end

"""
  `getDimensions(x) -> Dict{DataType,Int}`

  Returns the dimension map for a named measure type.  Catchall overloads this in Catchall.jl.
"""
getDimensions(x::T) where {T<:AbstractMeasure} = allUnitTypes[T].dimensions

"""
  `macro relateMeasures(relation)`

  Adds a multiplicative relationship between the left and right sides of the equation, allowing units to be multiplied and divided with consistent units.
  All types must already be defined and only one * or / is supported on the left side, while the right should be the resultant type.

  ```
    @relateMeasures Meter*Newton = NewtonMeter
    @relateMeasures Newton/Meter2 = Pascal
    @relateMeasures 1/Second = Hertz   # inverse: sets Hertz dims to {AbstractTime=>-1}
  ```

  To add a compound unit like kg*m/s^2, build incrementally:

  ```
    @makeBaseMeasure Frequency Hertz "Hz"
    @relateMeasures 1/Second = Hertz
    @makeMeasure 1 Hertz = 1 PerSecond "s^-1"
    @makeBaseMeasure Velocity MeterPerSecond "m/s"
    @relateMeasures Meter*PerSecond = MeterPerSecond
    @makeBaseMeasure Acceleration MeterPerSecond2 "m/s^2"
    @relateMeasures MeterPerSecond*PerSecond = MeterPerSecond2
    @makeBaseMeasure Force Newton "N"
    @relateMeasures KiloGram*MeterPerSecond2 = Newton
  ```
"""
macro relateMeasures(relation)
  if length(relation.args) == 2
    operator = relation.args[1].args[1]
    lhs1     = relation.args[1].args[2]
    lhs2     = relation.args[1].args[3]
    TNM      = relation.args[2].args[2]

    # Special case: 1/X = Y  (inverse / reciprocal relation)
    if isa(lhs1, Number) && lhs1 == 1 && (operator == :/ || operator == /)
      TX = lhs2
      TY = TNM
      return esc(quote
        UnitTypes.addInverseRelation($TX, $TY, @__MODULE__)
      end)
    end

    TM = lhs1
    TN = lhs2
    operator = Symbol(operator)
    qts = [quote (
      UnitTypes.addRelations($operator, $TM, $TN, $TNM, @__MODULE__)
      ) end]
    return esc(Expr(:block, qts...))
  else
    throw(ArgumentError("@relateMeasures given incorrect format"))
  end
end

"""
  `function addInverseRelation(TX, TY, mod)`

  Establishes that `TY = 1/TX` (and `TX = 1/TY`):
  - defines `Base.:/(n::Number, x::AbstractTX) = baseY(n/toBaseFloat(x))` and the reverse
  - sets TY's dimension map to the element-wise negation of TX's dimension map
"""
function addInverseRelation(TX, TY, mod=@__MODULE__)
  superX = supertype(TX)
  superY = supertype(TY)
  baseX  = getBaseType(TX)
  baseY  = getBaseType(TY)

  mod.eval(quote
    if !hasmethod(Base.:/, (Number, $superX))
      Base.:/(n::Number, x::$superX) = $baseY(n / UnitTypes.toBaseFloat(x))
    end
    if !hasmethod(Base.:/, (Number, $superY))
      Base.:/(n::Number, y::$superY) = $baseX(n / UnitTypes.toBaseFloat(y))
    end
  end)

  # TY's dimensions = inverse of TX's dimensions
  dimsY = mergeBaseDimensions(Dict{DataType,Int}(), allUnitTypes[baseX].dimensions, -1)
  uta = allUnitTypes[baseY]
  allUnitTypes[baseY] = UnitTypeAttributes(uta.abstract, uta.base, uta.toBase, uta.fromBase, uta.abbreviation, uta.isAffine, dimsY)
end

"""
  `registerPower(abstractType, n, baseResultType, mod)`

  Records that values of `abstractType` raised to the integer power `n` resolve to
  `baseResultType`, then evals a zero-alloc `Base.literal_pow` method for every concrete
  subtype currently registered under `abstractType`.  Concrete subtypes defined later get
  their method from the power-conversion block inside `makeJointConversions`.
"""
function registerPower(abstractType::DataType, n::Int, baseResultType::DataType, mod)
  powerTypes[(abstractType, n)] = baseResultType
  fromBase = allUnitTypes[baseResultType].fromBase
  valN = Val{n}
  for (T, uta) in allUnitTypes
    supertype(T) == abstractType || continue
    toBase = uta.toBase
    mod.eval(quote
      if !UnitTypes.hasExactMethod(Base.literal_pow, (typeof(^), $T, $valN))
        Base.literal_pow(::typeof(^), x::$T, ::$valN) = $baseResultType($fromBase($toBase(x.value)^$n))
      end
    end)
  end
end

"""
  function addRelations(operator, TM, TN, TNM, mod=@__MODULE__)

  Adds */ relations between the given UnitTypes by eval()ing in the given module.
  The arguments are such that TM <operator> TN = TNM.
"""
function addRelations(operator, TM, TN, TNM, mod=@__MODULE__)
  superM = supertype(TM) # these are abstract
  superN = supertype(TN)
  superNM = supertype(TNM)
  baseM = getBaseType(TM)
  baseN = getBaseType(TN)
  baseNM = getBaseType(TNM)

  if operator==:* || operator==*
    mod.eval( quote
      if !UnitTypes.hasExactMethod(Base.:*, ($superM, $superN))
        Base.:*(x::$superM, y::$superN) = $baseNM( convert($baseM, x).value * convert($baseN,y).value) # ensure the operation is defined for the base units, in case the relation was not given in base: ft*lbs = Nm
      end
      if !UnitTypes.hasExactMethod(Base.:*, ($superN, $superM))
        Base.:*(x::$superN, y::$superM) = $baseNM( convert($baseN, x).value * convert($baseM,y).value)
      end

      if !UnitTypes.hasExactMethod(Base.:/, ($superNM, $superM))
        Base.:/(x::$superNM, y::$superM) = $baseN( convert($baseNM,x).value / convert($baseM,y).value )
      end
      if !UnitTypes.hasExactMethod(Base.:/, ($superNM, $superN))
        Base.:/(x::$superNM, y::$superN) = $baseM( convert($baseNM,x).value / convert($baseN,y).value )
      end

      if $TM == $TN
        Base.sqrt(x::$TNM) = $TM( sqrt(x.value) ) # I can define sqrt(m^2) -> m, but I cannot define x^0.5 b/c the exponent might not lead to a known or integer unit..
      end
    end)
    # update result type's dimension signature from its factor types
    dimsNM = mergeBaseDimensions(allUnitTypes[baseM].dimensions, allUnitTypes[baseN].dimensions, 1)
    uta = allUnitTypes[baseNM]
    allUnitTypes[baseNM] = UnitTypeAttributes(uta.abstract, uta.base, uta.toBase, uta.fromBase, uta.abbreviation, uta.isAffine, dimsNM)

    # register power relations for zero-alloc literal_pow dispatch
    if TM == TN
      # direct square: TM^2 = TNM
      registerPower(superM, 2, baseNM, mod)
    else
      # chain detection: if TM = TN^k (from powerTypes), then TN^(k+1) = TNM
      for ((absT, k), powerBase) in powerTypes
        if absT == superN && powerBase == baseM
          registerPower(superN, k + 1, baseNM, mod)
          break
        end
      end
    end

  elseif operator==:/ || operator==/ # as in pressure: N/m^2 = Pa
    mod.eval(quote
      if !UnitTypes.hasExactMethod(Base.:/, ($superM, $superN))
        Base.:/(x::$superM, y::$superN) = $baseNM( convert($baseM, x).value / convert($baseN,y).value ) # F/m2 = Pa
      end
      if !UnitTypes.hasExactMethod(Base.:*, ($superM, $superNM))
        Base.:*(x::$superM, y::$superNM) = $baseN( convert($baseM,x).value * convert($baseNM,y).value ) # m2 * Pa = N
      end
      if !UnitTypes.hasExactMethod(Base.:*, ($superNM, $superM))
        Base.:*(x::$superNM, y::$superM) = $baseN( convert($baseNM,x).value * convert($baseM, y)) # Pa * m2 = N
      end
      if !UnitTypes.hasExactMethod(Base.:*, ($superN, $superNM))
        Base.:*(x::$superN, y::$superNM) = $baseM( convert($baseN,x).value * convert($baseNM,y).value ) # N * Pa = m2
      end
      if !UnitTypes.hasExactMethod(Base.:*, ($superNM, $superN))
        Base.:*(x::$superNM, y::$superN) = $baseM( convert($baseNM,x).value * convert($baseN,y).value ) # Pa * N = m2
      end

    end)
    # update result type's dimension signature: TNM = TM / TN
    dimsNM = mergeBaseDimensions(allUnitTypes[baseM].dimensions, allUnitTypes[baseN].dimensions, -1)
    uta = allUnitTypes[baseNM]
    allUnitTypes[baseNM] = UnitTypeAttributes(uta.abstract, uta.base, uta.toBase, uta.fromBase, uta.abbreviation, uta.isAffine, dimsNM)
  else
    throw(ArgumentError("Operator $operator unknown, @relateMeasures accepts only multiplicative measures in the format: @relateMeasures Meter*Newton=NewtonMeter"))
  end
end
@testitem "relateMeasures" begin
  @makeBaseMeasure LengthTest MeterT "mT"
  @makeBaseMeasure AreaTest Meter2T "m2T"
  @relateMeasures MeterT*MeterT = Meter2T
  @makeMeasure 1e-3 MeterT = 1 MilliMeterT "mmT"

  # @show which(Base.:*, (MilliMeterT, MilliMeterT))
  # @show which(Base.:*, (MilliMeterT, MeterT))
  # @show which(Base.:*, (MeterT, MilliMeterT))

  # multiplicative same
  @test MeterT(2)*MeterT(3) ≈ Meter2T(6)
  @test Meter2T(1) / MeterT(1) ≈ MeterT(1)
  @test MeterT(2)*MilliMeterT(3) ≈ Meter2T(6e-3)
  @test MilliMeterT(2)*MeterT(3) ≈ Meter2T(6e-3)

  # multiplicative different
  @makeBaseMeasure TorqueTest NewtonMeterT "NMT"
  @makeBaseMeasure ForceTest NewtonT "NT"
  @relateMeasures NewtonT*MeterT = NewtonMeterT
  @test NewtonT(2) * MeterT(3) ≈ NewtonMeterT(6) # as defined
  @test MeterT(2) * NewtonT(3) ≈ NewtonMeterT(6) # swapped
  @test NewtonMeterT(6) / MeterT(2) ≈ NewtonT(3)
  @test NewtonMeterT(6) / NewtonT(2) ≈ MeterT(3)
  
  # sqrt
  @test sqrt(Meter2T(4)) ≈ MeterT(2)

  # division 
  @makeBaseMeasure PressureTest PascalT "PaT"
  @relateMeasures NewtonT / Meter2T = PascalT
  @test NewtonT(6)/Meter2T(2) ≈ PascalT(3)
  @test PascalT(3)*Meter2T(2) ≈ NewtonT(6)
  @test Meter2T(2)*PascalT(3) ≈ NewtonT(6)

  #non-operator
  # @test_throws ArgumentError macroexpand(@__MODULE__, :( @relateMeasures MeterT Meter2T = NewtonT  )) # missing operator
  # @test_throws ArgumentError macroexpand(@__MODULE__, :( @relateMeasures MeterT & Meter2T = NewtonT  )) # operator is not defined on this

  # coefficient of thermal expansion?, to check with affine unit:
end

