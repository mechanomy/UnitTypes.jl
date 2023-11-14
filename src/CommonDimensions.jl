
@makeDimension Diameter Meter
@makeDimension Radius Meter

Base.convert(::Type{Radius}, x::Diameter) = Radius(x.value/2)
Base.convert(::Type{Diameter}, x::Radius) = Diameter(x.value*2)
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Diameter, U<:Radius} = isapprox(convert(Diameter, y), x, atol=atol, rtol=rtol)
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Radius, U<:Diameter} = isapprox(convert(Diameter, x), y, atol=atol, rtol=rtol)
@testitem "CommonDimensions.jl: DiameterRadius" begin
  @testset "conversion" begin
    @test convert(Radius, Diameter(Meter(3.4))) ≈ Radius(Meter(1.7))
    @test convert(Diameter, Radius{Millimeter}(1700)) ≈ Diameter(Meter(3.4))
  end
  @testset "isapprox" begin
    @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))
    @test Radius(Meter(1.7)) ≈ Diameter(Meter(3.4)) 
  end
end

# @makeDimension Length  Meter #omit to avoid similarlity to SIBase.jl/Length
@makeDimension Height Meter
@makeDimension Width Meter
@makeDimension Depth Meter
