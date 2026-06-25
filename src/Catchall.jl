# Written by Claude (claude-sonnet-4-6)
# Guiding prompts:
#   "Resolve back to named types by adding dimension tracking to UnitTypeAttributes"
#   "We always want to prefer the named, concrete types over Catchall"
#   "Add exponent handling to UnitTypes via Catchall; restrict to integer powers"
# Catch-all unit representation for unanticipated unit combinations arising from arithmetic
# or from u_str strings that don't match a registered abbreviation.  Named concrete types
# always take priority: resolveOrExpr() attempts a reverse dimension-map lookup first and
# only falls back to Catchall when no registered type matches.

export Catchall, parseCatchall

"""
  `struct Catchall <: AbstractMeasure`

  Catch-all for unit expressions with no defined named type.  The stored `value` is in SI
  base units; `dimensions` maps abstract dimension types to their integer exponents, e.g.
  `{AbstractLength=>1, AbstractTime=>-1}` for velocity.

  Prefer registered named types (Meter, Second, Newton …) wherever possible.
  Catchall is produced only when arithmetic or u_str parsing yields a combination that has
  no matching entry in `allUnitTypes`.
"""
struct Catchall <: AbstractMeasure
  value::Float64           # in SI base units
  dimensions::Dict{DataType,Int}
end

getDimensions(x::Catchall) = x.dimensions
toBaseFloat(x::Catchall)   = x.value

function _dimAbbreviation(dims::Dict{DataType,Int})::String
  isempty(dims) && return "dimensionless"
  parts = String[]
  for (absT, exp) in sort(collect(dims), by = x -> string(x[1]))
    # find the base type (i.e. the type that is its own base) for this abstract
    entries = filter(kv -> kv[2].abstract == absT && kv[2].base == kv[1], allUnitTypes)
    isempty(entries) && continue
    abbr = first(entries)[2].abbreviation
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

  Returns a named type instance if `dims` exactly matches a registered type's dimension
  signature; otherwise returns `Catchall(value, dims)`.
"""
function resolveOrExpr(value::Float64, dims::Dict{DataType,Int})::AbstractMeasure
  T = findNamedType(dims)
  T !== nothing && return T(allUnitTypes[T].fromBase(value))
  return Catchall(value, dims)
end

Base.:*(x::Catchall, y::Number) = resolveOrExpr(x.value * Float64(y), x.dimensions)
Base.:*(x::Number,   y::Catchall) = resolveOrExpr(Float64(x) * y.value, y.dimensions)
Base.:/(x::Catchall, y::Number) = resolveOrExpr(x.value / Float64(y), x.dimensions)
Base.:-(x::Catchall) = Catchall(-x.value, x.dimensions)

Base.:+(x::Catchall, y::Catchall) =
  x.dimensions == y.dimensions ?
    resolveOrExpr(x.value + y.value, x.dimensions) :
    throw(ArgumentError("Cannot add incompatible Catchall: $(abbreviation(x)) + $(abbreviation(y))"))

Base.:-(x::Catchall, y::Catchall) =
  x.dimensions == y.dimensions ?
    resolveOrExpr(x.value - y.value, x.dimensions) :
    throw(ArgumentError("Cannot subtract incompatible Catchall: $(abbreviation(x)) - $(abbreviation(y))"))

Base.isapprox(x::Catchall, y::Catchall; atol::Real=0, rtol::Real=atol) =
  x.dimensions == y.dimensions && isapprox(x.value, y.value; atol=atol, rtol=rtol)

Base.isapprox(x::Catchall, y::T; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure} =
  x.dimensions == getDimensions(y) && isapprox(x.value, toBaseFloat(y); atol=atol, rtol=rtol)

Base.isapprox(x::T, y::Catchall; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure} =
  isapprox(y, x; atol=atol, rtol=rtol)

# Catch-all unit × unit arithmetic:
# These fire only when no more-specific method (from @relateMeasures) exists.
# Julia dispatch gives the (AbstractX, AbstractY) methods from addRelations priority over these (AbstractMeasure, AbstractMeasure) methods, so named-type results are always preferred automatically.

Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} =
  resolveOrExpr(toBaseFloat(x) * toBaseFloat(y),
                mergeBaseDimensions(getDimensions(x), getDimensions(y), 1))

Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} =
  resolveOrExpr(toBaseFloat(x) / toBaseFloat(y),
                mergeBaseDimensions(getDimensions(x), getDimensions(y), -1))

# Integer exponentiation: (2mm)^3 = 8mm^3; Integer exponents keep dimension maps exact.
# A single method covers both named types and Catchall since both define toBaseFloat/getDimensions.
# Base.inv handles literal_pow(^, x, Val{-1}), which Julia emits for compile-time literal -1 exponents.
Base.:^(x::AbstractMeasure, n::Integer) = resolveOrExpr(toBaseFloat(x)^n, Dict{DataType,Int}(k => v*n for (k, v) in getDimensions(x) if v*n != 0))

Base.inv(x::AbstractMeasure) =
  resolveOrExpr(1.0 / toBaseFloat(x),
                Dict{DataType,Int}(k => -v for (k, v) in getDimensions(x) if v != 0))

@testitem "AbstractMeasure integer exponentiation" begin
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
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure ForceT NewtonT "nT"
  expr = MeterT(2.0) * NewtonT(3.0)   # Catchall with value 6.0

  @testset "design: type" begin
    @test expr^2 isa Catchall
    @test expr^1 isa Catchall
    @test expr^0 isa Catchall
  end

  @testset "functional: values and dimensions" begin
    @test (expr^2).value ≈ 36.0
    @test (expr^2).dimensions[AbstractLengthT] == 2
    @test (expr^2).dimensions[AbstractForceT] == 2
    @test (expr^1).value ≈ 6.0
    @test (expr^0).value ≈ 1.0
  end
end

"""
  `parseCatchall(str) -> Union{AbstractMeasure, Nothing}`

  Parses a compound unit string such as `"mm*s/kg"` or `"m^2"` into a measure.
  Each token is looked up by abbreviation in `allUnitTypes`; the combined scale factor
  and dimension map are computed, and `resolveOrExpr` is applied so a named type is
  returned whenever one matches.

  Returns `nothing` if any token is unrecognised or affine (Temperature-style).

  Syntax supported:
  - `*` separates numerator factors
  - `/` introduces denominator factors (applies to all tokens after the first `/` per `*`-group)
  - `^N` applies an integer exponent (may be negative) to the preceding abbreviation
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

@testitem "Catchall catch-all arithmetic" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure ForceT NewtonT "nT"

  r = MeterT(2.0) * NewtonT(3.0)
  # @show r, r.dimensions, r.dimensions[AbstractLengthT], r.dimensions[AbstractForceT]
  @test r isa Catchall
  @test r.value ≈ 6.0
  @test r.dimensions[AbstractLengthT] ≈ 1

  d = MeterT(6.0) / NewtonT(2.0)
  @test d isa Catchall
  @test d.value ≈ 3.0
  @test d.dimensions[AbstractLengthT] ≈ 1

  @test r * 2.0 isa Catchall
  @test (r * 2.0).value ≈ 12.0

  @test (r + r).value ≈ 12.0
  @test (r - r).value ≈ 0.0
  @test (-r).value ≈ -6.0
  @test_throws ArgumentError r + d   # different dimensions
end

@testitem "Catchall resolves back to named type" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure FreqT HertzT "hzT"
  @makeBaseMeasure ProductT ProdT "pT"
  @relateMeasures MeterT*HertzT = ProdT

  # direct multiplication goes through the specific method — should give ProdT
  @test MeterT(2.0) * HertzT(3.0) isa ProdT
  @test MeterT(2.0) * HertzT(3.0) ≈ ProdT(6.0)
  @test ProdT(6.0) / HertzT(3.0) ≈ MeterT(2.0)

  # construct Catchall manually with matching dims, then verify resolution
  u = Catchall(6.0, Dict{DataType,Int}(AbstractLengthT => 1, AbstractFreqT => 1))
  resolved = UnitTypes.resolveOrExpr(6.0, u.dimensions)
  @test resolved isa ProdT
  @test resolved ≈ ProdT(6.0)
end

@testitem "Catchall isapprox" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure ForceT NewtonT "NT"

  r1 = MeterT(2.0) * NewtonT(3.0)   # Catchall
  r2 = MeterT(3.0) * NewtonT(2.0)
  @test r1 ≈ r2

  r3 = MeterT(2.0) * NewtonT(4.0)
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

  # squared unit
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

  @makeMeasure 1 HertzT = 1 PerSecondT "s^-1"

  @makeBaseMeasure VelocityT MeterPerSecondT "m/s"
  @relateMeasures MeterT * PerSecondT = MeterPerSecondT
  # PerSecondT shares AbstractFreqT with HertzT, so *(AbstractLengthT, AbstractFreqT) already dispatches to ProdT.
  # The numeric value is the same; compare via base float rather than isapprox(ProdT, MeterPerSecondT).
  @test toBaseFloat(MeterT(2)*PerSecondT(1)) ≈ toBaseFloat(MeterPerSecondT(2))
  @test MeterT(2)/SecondT(1) ≈ MeterPerSecondT(2)

  # check complex exponent counting
  a = MeterPerSecondT(4)*MeterT(3)
  @test a.dimensions[AbstractLengthT] == 2
  b = MeterPerSecondT(4)/SecondT(2)
  @test b.dimensions[AbstractTimeT] == -2
end
