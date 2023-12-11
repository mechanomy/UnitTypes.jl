@makeBaseMeasure Angle Radian "rad"
@makeDerivedMeasure Degree "°" π/180 Radian # 1 degree = pi/180 rad

@testitem "Angle Radian Degree definitions" begin
  @test Radian(π) ≈ Radian(π)
  @test Degree(180) ≈ Degree(180.0)
  @test Radian(π) ≈ Degree(180)

  @test Radian(Degree(180)) ≈ Radian(π)
  @test Degree(Radian(π)) ≈ Degree(180)

  @test Radian(1) + Radian(2) ≈ Radian(3)
  @test Degree(1) + Degree(2) ≈ Degree(3)
  @test Radian(1)*2 ≈ Radian(2)
  @test Degree(1)*2 ≈ Degree(2)

  # @test isapprox(Radian(1) + Degree(1), Radian(1 + 1/360), atol=1e-4)
  # @test isapprox(Radian(1) + Degree(1), Degree(1/π*360 + 1), atol=1e-4)
end

#I like having * defined for Degree, for 3*30° things, but the danger is that theta*r operations need convert()
# the correct solution is to push the result units back into the calc, but I don't know if this can be inferred from the types.
# there may be a way through promotions? https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion
Base.promote_rule(::Type{Radian}, ::Type{Degree}) = Radian #promote to Radian in general
@testitem "Angle promotion" begin
  @test promote_type(Radian, Degree) == Radian
end

# all Measures can already be converted to Number, why do I need to specify these?
Base.sin(x::Radian) = sin(x.value)
Base.sin(x::Degree) = sin(convert(Radian, x))
Base.cos(x::Radian) = cos(x.value)
Base.cos(x::Degree) = cos(convert(Radian, x))
Base.tan(x::Radian) = tan(x.value)
Base.tan(x::Degree) = tan(convert(Radian, x))

#...can't dispatch on output types!:
# Base.asin(x::T where T<:Number)::Radian = Radian(asin(x)) 
# --> @test isapprox( asin(√2/2), Radian(π/4), atol=1e-3) # this succeeds even without preceding
# Base.asin(x::T where T<:Number)::Degree = convert(Degree, Radian(asin(x))) 
# --> @test isapprox( asin(√2/2), Degree(45), atol=1e-3) # this fails
# asind() works fine, is more correct, but means that asind() will return a untyped radian number rather than Radian.  Now this can be converted to Radian by a later call but there's a gap there

@testitem "Angle trigonometry" begin
  @test isapprox( √2/2, sin(Radian(π/4)), atol=1e-3 )
  @test isapprox( sin(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( sin(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( cos(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( cos(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( tan(Radian(π/4)), 1, atol=1e-3 )
  @test isapprox( tan(Degree(45)), 1, atol=1e-3)

  # @test isapprox( asin(√2/2), Radian(π/4), atol=1e-3)
  # @test isapprox( asin(√2/2), Degree(45), atol=1e-3)
  # @test isapprox( asind(√2/2), Degree(45), atol=1e-3)
  # @test isapprox( acos(√2/2), Radian(π/4), atol=1e-3)
  # @test isapprox( acosd(√2/2), Degree(45), atol=1e-3)
  # @test isapprox( atan(1), Radian(π/4), atol=1e-3)
  # @test isapprox( atand(1), Degree(45), atol=1e-3)
end