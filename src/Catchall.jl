# Written by Claude (claude-sonnet-4-6)
# Guiding prompts:
#   "Resolve back to named types by adding dimension tracking to UnitTypeAttributes"
#   "We always want to prefer the named, concrete types over Catchall"
#   "Add exponent handling to UnitTypes via Catchall; restrict to integer powers"
#   "Redesign Catchall to store BaseDimensions instead of Dict{DataType,Int}"
# Catch-all unit representation for unanticipated unit combinations arising from arithmetic or from u_str strings that don't match a registered abbreviation. Named concrete types always take priority: resolveOrExpr() attempts a reverse dimension-map lookup first and only falls back to Catchall when no registered type matches.

export Catchall, parseCatchall, getDimensions, mergeBaseDimensions, findNamedType, BaseDimensions, abstractToSI, toSIDimensions, getBaseDims, findNamedTypeFromSI
"""
  `struct BaseDimensions`

  Fixed-field SI base-dimension representation used by Catchall.
  Each field holds the integer exponent of that SI base unit: s (time), m (length), kg (mass), A (current), K (temperature), mol (amount), cd (intensity).
  Using fixed fields instead of a Dict{DataType,Int} guarantees all Catchall values share a canonical representation regardless of which abstract types were used to build them.
"""
struct BaseDimensions
  time::Int8
  length::Int8
  mass::Int8
  current::Int8
  temperature::Int8
  amount::Int8
  intensity::Int8
end
BaseDimensions(; time::Integer=0, length::Integer=0, mass::Integer=0, current::Integer=0, temperature::Integer=0, amount::Integer=0, intensity::Integer=0) = BaseDimensions(time, length, mass, current, temperature, amount, intensity)

Base.:+(a::BaseDimensions, b::BaseDimensions) = BaseDimensions(a.time+b.time, a.length+b.length, a.mass+b.mass, a.current+b.current, a.temperature+b.temperature, a.amount+b.amount, a.intensity+b.intensity)
Base.:-(a::BaseDimensions, b::BaseDimensions) = BaseDimensions(a.time-b.time, a.length-b.length, a.mass-b.mass, a.current-b.current, a.temperature-b.temperature, a.amount-b.amount, a.intensity-b.intensity)
Base.:*(a::BaseDimensions, n::Integer) = BaseDimensions(a.time*n, a.length*n, a.mass*n, a.current*n, a.temperature*n, a.amount*n, a.intensity*n)
Base.:*(n::Integer, a::BaseDimensions) = a * n
Base.:-(a::BaseDimensions) = BaseDimensions(-a.time, -a.length, -a.mass, -a.current, -a.temperature, -a.amount, -a.intensity)
Base.iszero(d::BaseDimensions) = d.time==0 && d.length==0 && d.mass==0 && d.current==0 && d.temperature==0 && d.amount==0 && d.intensity==0

"""
  `abstractToSI::Dict{DataType, BaseDimensions}`

  Registry mapping each abstract dimension type (e.g. AbstractForce) to its SI base dimensions.
  Seeded for the seven SI base abstract types in SI.jl and Temperature.jl; extended by addRelations and addInverseRelation as @relateMeasures chains are processed.
"""
const abstractToSI = Dict{DataType, BaseDimensions}()

"""
  `toSIDimensions(dict; silent=false) -> BaseDimensions`

  Converts a Dict{DataType,Int} dimension map to a BaseDimensions struct by looking up each abstract type key in abstractToSI.
  Unknown types are skipped with a warning (suppressed when silent=true, used internally when searching allUnitTypes).
"""
function toSIDimensions(dict::Dict{DataType,Int}; silent::Bool=false)::BaseDimensions
  result = BaseDimensions()
  for (absT, exp) in dict
    if haskey(abstractToSI, absT)
      result = result + abstractToSI[absT] * exp
    elseif !silent
      @warn "No SI dimension mapping for $absT; Catchall will not track this dimension. Connect it via @relateMeasures to the SI base units."
    end
  end
  return result
end

"""
  `findNamedTypeFromSI(dims) -> Union{DataType, Nothing}`

  Returns the registered base concrete type whose allUnitTypes dimension dict converts to exactly `dims` via toSIDimensions; returns nothing if no match exists.
"""
function findNamedTypeFromSI(dims::BaseDimensions)::Union{DataType, Nothing}
  iszero(dims) && return nothing
  for (T, uta) in allUnitTypes
    uta.base == T || continue
    toSIDimensions(uta.dimensions, silent=true) == dims && return T
  end
  return nothing
end

"""
  `getBaseDims(x) -> BaseDimensions`

  Returns the SI base dimensions for any AbstractMeasure.
  For named types converts the allUnitTypes Dict through the abstractToSI registry; for Catchall returns x.dimensions directly (overloaded below).
"""
getBaseDims(x::T) where {T<:AbstractMeasure} = toSIDimensions(allUnitTypes[T].dimensions)

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

  Returns the registered type whose dimension signature exactly matches `dims`, preferring base types (where `allUnitTypes[T].base == T`) over scaled variants like Inch2 vs Meter2.
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
  `struct Catchall <: AbstractMeasure`

  Catch-all for unit expressions with no defined named type.
  The stored `value` is in SI base units; `dimensions` is a BaseDimensions struct encoding the seven SI base-unit exponents.
  Using BaseDimensions (fixed fields) rather than Dict{DataType,Int} ensures all Catchall values share a canonical representation: e.g. KiloNewton and Newton both convert to BaseDimensions(mass=1,length=1,time=-2) via abstractToSI, preventing the stale-dict bug.
  Prefer registered named types (Meter, Second, Newton …) wherever possible; Catchall is produced only when arithmetic or u_str parsing yields a combination with no matching entry in allUnitTypes.
"""
struct Catchall <: AbstractMeasure
  value::Float64           # in SI base units
  dimensions::BaseDimensions
end

# Catchall-specific overload of getBaseDims (named-type overload is in Measure.jl)
getBaseDims(x::Catchall) = x.dimensions

getDimensions(x::Catchall) = x.dimensions
toBaseFloat(x::Catchall)   = x.value

function _dimAbbreviation(dims::BaseDimensions)::String
  iszero(dims) && return "dimensionless"
  parts = String[]
  for (abbr, exp) in [("s", dims.time), ("m", dims.length), ("kg", dims.mass), ("A", dims.current), ("K", dims.temperature), ("mol", dims.amount), ("cd", dims.intensity)]
    exp == 0 && continue
    push!(parts, exp == 1 ? abbr : "$(abbr)^$(exp)")
  end
  return join(parts, "*")
end

function abbreviation(u::Catchall)::String
  _dimAbbreviation(u.dimensions)
end

function measure2String(u::Catchall)::String
  "$(u.value)$(_dimAbbreviation(u.dimensions))"
end

Base.show(io::IO, u::Catchall) = print(io, measure2String(u))

"""
  `resolveOrExpr(value, dims) -> AbstractMeasure`

  Returns a named type instance if `dims` exactly matches a registered type's SI dimension signature; otherwise returns `Catchall(value, dims)`.
  Two overloads: BaseDimensions uses findNamedTypeFromSI; Dict{DataType,Int} uses findNamedType first then falls back to toSIDimensions.
"""
function resolveOrExpr(value::Float64, dims::BaseDimensions)::AbstractMeasure
  T = findNamedTypeFromSI(dims)
  T !== nothing && return T(allUnitTypes[T].fromBase(value))
  return Catchall(value, dims)
end

function resolveOrExpr(value::Float64, dict::Dict{DataType,Int})::AbstractMeasure
  T = findNamedType(dict)
  T !== nothing && return T(allUnitTypes[T].fromBase(value))
  return Catchall(value, toSIDimensions(dict))
end

Base.:*(x::Catchall, y::Number) = resolveOrExpr(x.value * Float64(y), x.dimensions)
Base.:*(x::Number,   y::Catchall) = resolveOrExpr(Float64(x) * y.value, y.dimensions)
Base.:/(x::Catchall, y::Number) = resolveOrExpr(x.value / Float64(y), x.dimensions)
Base.:-(x::Catchall) = Catchall(-x.value, x.dimensions)

Base.:+(x::Catchall, y::Catchall) = x.dimensions == y.dimensions ? resolveOrExpr(x.value + y.value, x.dimensions) : throw(ArgumentError("Cannot add incompatible Catchall: $(abbreviation(x)) + $(abbreviation(y))"))

Base.:-(x::Catchall, y::Catchall) = x.dimensions == y.dimensions ? resolveOrExpr(x.value - y.value, x.dimensions) : throw(ArgumentError("Cannot subtract incompatible Catchall: $(abbreviation(x)) - $(abbreviation(y))"))

Base.isapprox(x::Catchall, y::Catchall; atol::Real=0, rtol::Real=atol) = x.dimensions == y.dimensions && isapprox(x.value, y.value; atol=atol, rtol=rtol)

Base.isapprox(x::Catchall, y::T; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure} = x.dimensions == getBaseDims(y) && isapprox(x.value, toBaseFloat(y); atol=atol, rtol=rtol)

Base.isapprox(x::T, y::Catchall; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure} = isapprox(y, x; atol=atol, rtol=rtol)

# Catch-all unit × unit arithmetic: fire only when no more-specific method (from @relateMeasures) exists; Julia dispatch gives addRelations methods priority so named-type results are always preferred automatically.
Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = resolveOrExpr(toBaseFloat(x) * toBaseFloat(y), getBaseDims(x) + getBaseDims(y))

Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = resolveOrExpr(toBaseFloat(x) / toBaseFloat(y), getBaseDims(x) - getBaseDims(y))

# Integer exponentiation: (2mm)^3 = 8mm^3; Integer exponents keep dimension maps exact. A single method covers named types and Catchall since both define toBaseFloat/getBaseDims. Base.inv handles literal_pow(^,x,Val{-1}) for compile-time -1 exponents.
Base.:^(x::AbstractMeasure, n::Integer) = resolveOrExpr(toBaseFloat(x)^n, getBaseDims(x) * n)

Base.inv(x::AbstractMeasure) = resolveOrExpr(1.0 / toBaseFloat(x), -getBaseDims(x))

@testitem "AbstractMeasure integer exponentiation" begin
  # Uses real SI types; BaseDimensions lookup resolves named types correctly.
  @testset "design: type resolution" begin
    @test (2u"mm")^3 isa Meter3       # resolves to named base type
    @test (2u"mm")^2 isa Meter2
    @test Meter(3)^2 isa Meter2
    @test Meter(3)^1 isa Meter
    @test Meter(3)^0 isa Catchall     # n=0 → dimensionless, no named type
    @test Meter(2)^(-1) isa Catchall  # no named type for m^-1
  end

  @testset "functional: values" begin
    @test (2u"mm")^3 ≈ 8u"mm^3"      # (2mm)^3 == 8mm^3
    @test (2u"mm")^2 ≈ Meter2(4e-6)
    @test Meter(3)^2 ≈ Meter2(9)
    @test Meter(3)^1 ≈ Meter(3)
    @test (Meter(3)^0).value ≈ 1.0
    @test (Meter(2)^(-1)).value ≈ 0.5
  end
end

@testitem "Catchall integer exponentiation" begin
  # Catchall tests use real SI types (Meter, Mole) rather than isolated @makeBaseMeasure
  # test types.  The previous isolation strategy was superseded when Catchall moved to
  # BaseDimensions: isolated test types have no entry in abstractToSI and would produce
  # all-zero (dimensionless) Catchall values.  Real SI types are always resolvable.
  expr = Meter(2.0) * Mole(3.0)   # no @relateMeasures between Length and Amount → Catchall

  @testset "design: type" begin
    @test expr^2 isa Catchall
    @test expr^1 isa Catchall
    @test expr^0 isa Catchall
  end

  @testset "functional: values and dimensions" begin
    @test (expr^2).value ≈ 36.0
    @test (expr^2).dimensions.length == 2
    @test (expr^2).dimensions.amount == 2
    @test (expr^1).value ≈ 6.0
    @test (expr^0).value ≈ 1.0
  end
end

@testitem "Catchall catch-all arithmetic" begin
  # Catchall tests use real SI types (Meter, Mole) — see note in "Catchall integer
  # exponentiation" above for why isolated test types can no longer be used.
  r = Meter(2.0) * Mole(3.0)    # m*mol → Catchall, value=6, dims={length:1, amount:1}
  d = Meter(6.0) / Mole(2.0)    # m/mol → Catchall, value=3, dims={length:1, amount:-1}

  @test r isa Catchall
  @test r.value ≈ 6.0
  @test r.dimensions.length == 1
  @test r.dimensions.amount == 1

  @test d isa Catchall
  @test d.value ≈ 3.0
  @test d.dimensions.length == 1
  @test d.dimensions.amount == -1

  @test r * 2.0 isa Catchall
  @test (r * 2.0).value ≈ 12.0

  @test (r + r).value ≈ 12.0
  @test (r - r).value ≈ 0.0
  @test (-r).value ≈ -6.0
  @test_throws ArgumentError r + d   # different dimensions
end

@testitem "Catchall resolves back to named type" begin
  # A Catchall whose BaseDimensions match a named type's SI signature resolves back.
  # NewtonMeter = kg*m^2*s^-2 → BaseDimensions(mass=1,length=2,time=-2)
  nmDims = BaseDimensions(Int8(-2), Int8(2), Int8(1), 0, 0, 0, 0)
  u = Catchall(6.0, nmDims)
  resolved = UnitTypes.resolveOrExpr(6.0, u.dimensions)
  @test resolved isa NewtonMeter
  @test resolved ≈ NewtonMeter(6.0)

  # Catchall * Number triggers resolveOrExpr on the same dims → resolves to named type
  @test (u * 2.0) isa NewtonMeter
  @test (u * 2.0) ≈ NewtonMeter(12.0)

  # Two Catchalls whose combined SI dims match a named type also resolve
  fDims = BaseDimensions(Int8(-2), Int8(1), Int8(1), 0, 0, 0, 0)  # Newton: kg*m*s^-2
  lDims = BaseDimensions(0, Int8(1), 0, 0, 0, 0, 0)                # Meter: m
  cf = Catchall(2.0, fDims)
  cl = Catchall(3.0, lDims)
  @test cf * cl isa NewtonMeter
  @test cf * cl ≈ NewtonMeter(6.0)
end

@testitem "Catchall isapprox" begin
  # See isolation note in "Catchall integer exponentiation" above.
  r1 = Meter(2.0) * Mole(3.0)   # value=6, dims={length:1, amount:1}
  r2 = Meter(3.0) * Mole(2.0)   # same dims and value
  @test r1 ≈ r2

  r3 = Meter(2.0) * Mole(4.0)   # same dims, different value
  @test !(r1 ≈ r3)
end

@testitem "parseCatchall compound strings" begin
  # unknown combination → Catchall
  result = UnitTypes.parseCatchall("m*kg")
  @test result isa Catchall

  # 1 mm*s/kg in base SI = 1e-3 m*s/kg
  result2 = UnitTypes.parseCatchall("mm*s/kg")
  @test result2 isa Catchall
  @test result2.value ≈ 1e-3

  # squared unit resolves to named type
  result3 = UnitTypes.parseCatchall("m^2")
  @test result3 isa Meter2   # @relateMeasures Meter*Meter=Meter2 is in SI.jl

  # unknown abbreviation → nothing
  @test UnitTypes.parseCatchall("quux") === nothing
end

@testitem "u_str compound fallback" begin
  x = 2.5u"mm*s/kg"
  @test x isa Catchall
  @test x.value ≈ 2.5e-3   # 2.5 mm*s/kg in base SI
end

@testitem "dimension tracking on @makeBaseMeasure and @makeMeasure" begin
  @makeBaseMeasure LengthTest2 MeterX "mX"
  @test UnitTypes.allUnitTypes[MeterX].dimensions == Dict{DataType,Int}(AbstractLengthTest2 => 1)

  @makeMeasure 1e-3 MeterX = 1 MilliMeterX "mmX"
  @test UnitTypes.allUnitTypes[MilliMeterX].dimensions == Dict{DataType,Int}(AbstractLengthTest2 => 1)
end

@testitem "dimension tracking after @relateMeasures" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure TimeT SecondT "sT"
  @makeBaseMeasure FreqT HertzT "hzT"

  @makeBaseMeasure ProductT ProdT "pT"
  @relateMeasures MeterT*HertzT = ProdT

  @test UnitTypes.allUnitTypes[ProdT].dimensions == Dict{DataType,Int}(AbstractLengthT => 1, AbstractFreqT => 1)

  @makeBaseMeasure QuotientT DivT "divT"
  @relateMeasures MeterT/HertzT = DivT
  @test UnitTypes.allUnitTypes[DivT].dimensions == Dict{DataType,Int}(AbstractLengthT => 1, AbstractFreqT => -1)

  @relateMeasures 1/SecondT = HertzT
  @test 1/SecondT(4) ≈ HertzT(0.25)

  @makeMeasure 1 HertzT = 1 PerSecondT "sT^-1"

  @makeBaseMeasure VelocityT MeterPerSecondT "mT/sT"
  @relateMeasures MeterT * PerSecondT = MeterPerSecondT
  # PerSecondT shares AbstractFreqT with HertzT, so *(AbstractLengthT, AbstractFreqT) already dispatches to ProdT.
  # The numeric value is the same; compare via base float rather than isapprox(ProdT, MeterPerSecondT).
  @test toBaseFloat(MeterT(2)*PerSecondT(1)) ≈ toBaseFloat(MeterPerSecondT(2))
  @test MeterT(2)/SecondT(1) ≈ MeterPerSecondT(2)

  # Catchall dimension exponent accumulation — use real SI types (Meter, MeterPerSecond,
  # Mole) so abstractToSI can resolve BaseDimensions correctly.
  a = MeterPerSecond(4) * Meter(3)   # m/s * m = m^2/s → Catchall
  @test a isa Catchall
  @test a.dimensions.length == 2
  @test a.dimensions.time == -1

  b = Meter(4) * Mole(3) / Second(2)  # m*mol/s → Catchall
  @test b isa Catchall
  @test b.dimensions.time == -1
  @test b.dimensions.length == 1
  @test b.dimensions.amount == 1
end

"""
  `parseCatchall(str) -> Union{AbstractMeasure, Nothing}`

  Parses a compound unit string such as `"mm*s/kg"` or `"m^2"` into a measure.
  Each token is looked up by abbreviation in `allUnitTypes`; the combined scale factor and dimension map are computed, and `resolveOrExpr` is applied so a named type is returned whenever one matches.
  Returns `nothing` if any token is unrecognised or affine (Temperature-style).
  Syntax: `*` separates numerator factors; `/` introduces denominator factors; `^N` applies an integer exponent (may be negative) to the preceding abbreviation.
"""
function parseCatchall(str::String)::Union{AbstractMeasure, Nothing}
  dims       = Dict{DataType,Int}()
  baseFactor = 1.0

  # split on '*' first, then on '/' within each group
  for starPart in split(str, '*')
    slashParts = split(string(starPart), '/')
    for (j, rawPart) in enumerate(slashParts)
      sign = j == 1 ? 1 : -1   # first piece is numerator, rest are denominators

      # parse optional exponent suffix:  "m^2" → ("m", 2),  "kg^-1" → ("kg", -1)
      expMatch = match(r"^(.+)\^(-?\d+)$", string(rawPart))
      if expMatch !== nothing
        abbr    = string(expMatch[1])
        unitExp = parse(Int, expMatch[2])
      else
        abbr    = string(rawPart)
        unitExp = 1
      end

      matched = filter(kv -> kv[2].abbreviation == abbr, allUnitTypes)
      isempty(matched) && return nothing

      T, uta = first(matched)
      uta.isAffine && return nothing   # affine units (e.g. °F) cannot compose

      totalExp    = sign * unitExp
      baseFactor *= uta.toBase(1.0)^totalExp
      for (k, v) in uta.dimensions
        dims[k] = get(dims, k, 0) + totalExp * v
      end
    end
  end

  filter!(kv -> last(kv) != 0, dims)
  return resolveOrExpr(baseFactor, dims)
end
