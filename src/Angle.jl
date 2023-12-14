@makeBaseMeasure Angle Radian "rad"
@deriveMeasure Radian(π) = Degree(180) "°"

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
end

Base.promote_rule(::Type{Radian}, ::Type{Degree}) = Radian #promote to Radian in general
@testitem "Angle promotion" begin
  @test promote_type(Radian, Degree) == Radian # what's the use case that I should be testing?
end

Base.sin(x::Radian) = sin(x.value)
Base.sin(x::Degree) = sin(convert(Radian, x))
Base.cos(x::Radian) = cos(x.value)
Base.cos(x::Degree) = cos(convert(Radian, x))
Base.tan(x::Radian) = tan(x.value)
Base.tan(x::Degree) = tan(convert(Radian, x))

@testitem "Angle trigonometry" begin
  @test isapprox( √2/2, sin(Radian(π/4)), atol=1e-3 )
  @test isapprox( sin(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( sin(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( cos(Radian(π/4)), √2/2, atol=1e-3 )
  @test isapprox( cos(Degree(45)), √2/2, atol=1e-3)
  @test isapprox( tan(Radian(π/4)), 1, atol=1e-3 )
  @test isapprox( tan(Degree(45)), 1, atol=1e-3)
end