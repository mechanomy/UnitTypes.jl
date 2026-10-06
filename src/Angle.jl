@makeBaseMeasure Angle Radian "rad"
@makeMeasure π/180 Radian = 1 Degree "°"


# @makeMeasure arcmin

# constants: https://github.com/JuliaLang/julia/blob/master/base/mathconstants.jl
# On the one hand I'd like to overwrite these to be unit-correct, but that would likely break other base libraries that expect unitless
# Not exported to avoid clashing with Base.pi; reference as UnitTypes.pi and UnitTypes.tau.
# Within this module `pi` is a Radian, so unitless uses must be written Base.pi.
const pi = Radian(Base.pi)
const tau = Radian(Base.pi*2)


@testitem "Angle Radian Degree definitions" begin
  @test convert(Radian, Degree(180)) ≈ Radian(π)
  @test convert(Degree, Radian(π)) ≈ Degree(180)

  @test Radian(π) ≈ Degree(180)
  @test Degree(180) ≈ Radian(π)
  @test Radian(Degree(180)) ≈ Radian(π)
  @test Degree(Radian(π)) ≈ Degree(180)

  @test Degree(1) + Degree(2) ≈ Degree(3)
  @test isapprox(Degree(3) - Degree(1), Degree(2), atol=1e-3)

  @test Radian(1)*2 ≈ Radian(2)
  @test Degree(1)*2 ≈ Degree(2)
  @test -Degree(45) ≈ Degree(-45)
end

# Forward trigonometry: AbstractAngle → Float64.
# Every AbstractAngle is evaluated through its Radian value; convert() is used rather than toBaseFloat() to stay allocation-free.
for f in (:sin, :cos, :tan, :sec, :csc, :cot, :sincos)
  @eval Base.$f(x::AbstractAngle) = $f(convert(Radian, x).value)
end

# Degree uses Base's degree functions, which are exact at multiples of 90°.
for (f, fd) in ((:sin, :sind), (:cos, :cosd), (:tan, :tand), (:sec, :secd), (:csc, :cscd), (:cot, :cotd), (:sincos, :sincosd))
  @eval Base.$f(x::Degree) = $fd(x.value)
end

# Inverse trigonometry: Real → AbstractAngle, with the returned angle type given first, as in asin(Degree, 0.5).
# Base.asin(::Real) is untouched and still returns a Float64.
for (f, fd) in ((:asin, :asind), (:acos, :acosd), (:atan, :atand), (:asec, :asecd), (:acsc, :acscd), (:acot, :acotd))
  @eval Base.$f(::Type{T}, x::Real) where T<:AbstractAngle = convert(T, Radian($f(x)))
  @eval Base.$f(::Type{Degree}, x::Real) = Degree($fd(x))
end

"""
  `atan(y::AbstractMeasure, x::AbstractMeasure) :: Radian`

  Two-argument arctangent of measures sharing the same abstract type, as in `atan(Meter(1), Inch(10))`.
  `x` is converted into the unit of `y` before the ratio is taken.
  `atan(Degree, y, x)` returns the angle as any other AbstractAngle.
"""
function Base.atan(y::AbstractMeasure, x::AbstractMeasure)
  return Radian(atan(y.value, sameUnitValue(y, x)))
end
Base.atan(::Type{T}, y::AbstractMeasure, x::AbstractMeasure) where T<:AbstractAngle = convert(T, atan(y, x))
Base.atan(::Type{Degree}, y::AbstractMeasure, x::AbstractMeasure) = Degree(atand(y.value, sameUnitValue(y, x)))
Base.atan(::Type{T}, y::Real, x::Real) where T<:AbstractAngle = convert(T, Radian(atan(y, x)))
Base.atan(::Type{Degree}, y::Real, x::Real) = Degree(atand(y, x))

# Returns x's value expressed in y's unit, rejecting measures of differing dimension.
function sameUnitValue(y::T, x::U) where {T<:AbstractMeasure, U<:AbstractMeasure}
  supertype(T) == supertype(U) || throw(ArgumentError("atan requires measures of the same dimension, given $T and $U"))
  return convert(T, x).value
end
function sameUnitValue(y::Catchall, x::Catchall)
  y.dimensions == x.dimensions || throw(ArgumentError("atan requires measures of the same dimension, given $(abbreviation(y)) and $(abbreviation(x))"))
  return x.value
end

@testitem "Angle trigonometry" begin
  @test isapprox( √2/2, sin(Radian(π/4)), atol=1e-3 )
  @test isapprox( sin(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( sin(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( cos(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( cos(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( tan(Radian(π/4)), 1, atol=1e-3 )
  @test isapprox( tan(Degree(45)), 1, atol=1e-3)

  @testset "reciprocal functions" begin
    @test sec(Radian(π/3)) ≈ 2
    @test csc(Radian(π/6)) ≈ 2
    @test cot(Radian(π/4)) ≈ 1
    @test sec(Degree(60)) ≈ 2
    @test csc(Degree(30)) ≈ 2
    @test cot(Degree(45)) ≈ 1
  end

  @testset "sincos" begin
    @test all(sincos(Radian(π/6)) .≈ (0.5, √3/2))
    @test all(sincos(Degree(30)) .≈ (0.5, √3/2))
  end

  @testset "Degree is exact at multiples of 90°" begin
    @test sin(Degree(180)) === 0.0
    @test cos(Degree(90)) === 0.0
    @test sin(Degree(270)) === -1.0
    @test tan(Degree(180)) === -0.0
    @test sincos(Degree(90)) === (1.0, 0.0)
  end

  @testset "Float64 results" begin
    @test sin(Radian(1)) isa Float64
    @test cos(Degree(1)) isa Float64
    @test sincos(Degree(1)) isa Tuple{Float64,Float64}
  end

  @testset "non-angles are rejected" begin
    @test_throws MethodError sin(Meter(1))
    @test_throws MethodError cos(Second(1))
  end
end

@testitem "Angle inverse trigonometry" begin
  @test asin(Radian, 0.5) isa Radian
  @test isapprox(asin(Radian, 0.5), Radian(π/6), atol=1e-12)
  @test isapprox(acos(Radian, 0.5), Radian(π/3), atol=1e-12)
  @test isapprox(atan(Radian, 1), Radian(π/4), atol=1e-12)
  @test isapprox(asec(Radian, 2), Radian(π/3), atol=1e-12)
  @test isapprox(acsc(Radian, 2), Radian(π/6), atol=1e-12)
  @test isapprox(acot(Radian, 1), Radian(π/4), atol=1e-12)

  @test asin(Degree, 0.5) isa Degree
  @test asin(Degree, 1) === Degree(90.0)
  @test acos(Degree, 0) === Degree(90.0)
  @test isapprox(atan(Degree, 1), Degree(45), atol=1e-12)
  @test isapprox(asec(Degree, 2), Degree(60), atol=1e-12)
  @test isapprox(acsc(Degree, 2), Degree(30), atol=1e-12)
  @test isapprox(acot(Degree, 1), Degree(45), atol=1e-12)

  @test asin(0.5) isa Float64 # Base behavior is unchanged

  @testset "round trip" begin
    @test sin(asin(Degree, 0.3)) ≈ 0.3
    @test cos(acos(Radian, 0.3)) ≈ 0.3
  end
end

@testitem "Angle two-argument atan" begin
  @test atan(Meter(1), Meter(1)) isa Radian
  @test isapprox(atan(Meter(1), Meter(1)), Radian(π/4), atol=1e-12)
  @test isapprox(atan(Meter(1), Meter(-1)), Radian(3π/4), atol=1e-12)
  @test isapprox(atan(MilliMeter(1000), Meter(1)), Radian(π/4), atol=1e-12)
  @test isapprox(atan(Inch(1), MilliMeter(25.4)), Radian(π/4), atol=1e-12)
  @test isapprox(atan(Newton(-1), Newton(0)), Radian(-π/2), atol=1e-12)

  @test atan(Degree, Meter(1), Meter(1)) isa Degree
  @test atan(Degree, Meter(0), Meter(-1)) === Degree(180.0)
  @test atan(Degree, Meter(1), MilliMeter(0)) === Degree(90.0)
  @test isapprox(atan(Radian, Meter(1), Meter(1)), Radian(π/4), atol=1e-12)

  @test isapprox(atan(Degree, 1, -1), Degree(135), atol=1e-12)
  @test isapprox(atan(Radian, 1, 1), Radian(π/4), atol=1e-12)

  @test_throws ArgumentError atan(Meter(1), Second(1))

  a = Meter(2)*Mole(3)
  b = Meter(2)*Mole(3)
  @test isapprox(atan(a, b), Radian(π/4), atol=1e-12)
  @test_throws ArgumentError atan(a, Meter(1)*Candela(1))
end

@testitem "Angle constants are unexported" begin
  @test UnitTypes.pi ≈ Radian(π)
  @test UnitTypes.tau ≈ Degree(360)
  @test pi === Base.pi # using UnitTypes must not shadow Base.pi
end
