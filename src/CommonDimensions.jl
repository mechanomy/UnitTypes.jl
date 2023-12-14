
@makeDimension Diameter Meter
@makeDimension Radius Meter
Diameter( r::Radius ) = Diameter(r.measure*2)
Radius(d::Diameter) = Radius(d.measure/2)
Base.convert(::Type{Radius}, x::Diameter) = Radius(x)
Base.convert(::Type{Diameter}, x::Radius) = Diameter(x)

Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Diameter, U<:Radius} = isapprox(convert(Diameter, y), x, atol=atol, rtol=rtol)
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Radius, U<:Diameter} = isapprox(convert(Diameter, x), y, atol=atol, rtol=rtol)

Base.:+(d::Diameter, r::Radius) = Diameter(d.measure + r.measure*2)
Base.:+(r::Radius, d::Diameter) = Radius(r.measure + d.measure/2) 
Base.:-(d::Diameter, r::Radius) = Diameter(d.measure - r.measure*2)
Base.:-(r::Radius, d::Diameter) = Radius(r.measure - d.measure/2)

# =
@testitem "CommonDimensions.jl: DiameterRadius" begin
  @testset "conversion" begin
    @test convert(Radius, Diameter(Meter(3.4))) ≈ Radius(Meter(1.7))
    @test convert(Diameter, Radius(MilliMeter(1700))) ≈ Diameter(Meter(3.4))
    @test Radius(Diameter(Meter(2))) ≈ Radius(Meter(1))
    @test Diameter(Meter(3.4)) - Radius(CentiMeter(20)) ≈ Diameter(Meter(3.0))
    @test Diameter(Meter(3.4)) + Radius(CentiMeter(20)) ≈ Diameter(Meter(3.8))
  end
  @testset "isapprox" begin
    @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))
    @test Radius(Meter(1.7)) ≈ Diameter(Meter(3.4)) 
  end
end
# =#

# @makeDimension Length  Meter #omit to avoid similarlity to SIBase.jl/Length
@makeDimension Height Meter
@makeDimension Width Meter
@makeDimension Depth Meter

@makeDimension Duration Second