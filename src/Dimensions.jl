module Dimensions
  using TestItems
  using ..AbsDimension
  using ..AbsExtent

  export AbstractLength, Length, Height, Width, Depth
  abstract type AbstractLength <: AbstractDimension end
  @makeDimension Length AbstractLength
  @makeDimension Height AbstractLength
  @makeDimension Width AbstractLength
  @makeDimension Depth AbstractLength

  export AbstractDiameter, Diameter, Radius
  abstract type AbstractDiameter <: AbstractDimension end # things relating to circular diameter
  @makeDimension Diameter AbstractDiameter
  @makeDimension Radius AbstractDiameter
  # Base.convert(::Type{Diameter}, x::Radius) = Diameter{typeof(x).parameters[1]}(x.value*2) 
  # Base.convert(::Type{Diameter}, x::Radius) = Diameter(typeof(x).parameters[1](x.value*2))
  # Base.convert(::Type{Radius}, x::Diameter) = Radius{typeof(x).parameters[1]}(x.value/2) 
  Base.convert(::Type{Radius}, x::Diameter) = Radius(x.value/2)
  Base.convert(::Type{Diameter}, x::Radius) = Diameter(x.value*2)
  @testitem "AbsDimension: DiameterRadius conversion" begin
    @test convert(Radius, Diameter(Meter(3.4))) ≈ Radius(Meter(1.7))
    @test convert(Diameter, Radius{MilliMeter}(1700)) ≈ Diameter(Meter(3.4))
 
  end

  # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Diameter, U<:Radius} = isapprox(convert(Diameter, y), x, atol=atol, rtol=rtol)
  # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Radius, U<:Diameter} = isapprox(convert(Diameter, x), y, atol=atol, rtol=rtol)
  # @testitem "AbsDimension: DiameterRadius isapprox" begin
  #   @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))
  #   @test Radius(Meter(1.7)) ≈ Diameter(Meter(3.4)) 
  # end

  

end