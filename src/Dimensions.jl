module Dimensions
  using TestItems
  using ..Dimension
  using ..Length

  # export  Abstractength, Length, Height, Width, Depth
  # abstract type  Abstractength <:  Abstractimension end
  # @makeDimension Length  Abstractength
  # @makeDimension Height  Abstractength
  # @makeDimension Width  Abstractength
  # @makeDimension Depth  Abstractength


  export  AbstractDiameter, Diameter, Radius
  abstract type AbstractDiameter <: AbstractDimension end # things relating to circular diameter
  @makeDimension Diameter AbstractDiameter
  @makeDimension Radius AbstractDiameter

  Base.convert(::Type{Radius}, x::Diameter) = Radius(x.value/2)
  Base.convert(::Type{Diameter}, x::Radius) = Diameter(x.value*2)
  # @testitem "Dimension: DiameterRadius conversion" begin
  #   @test convert(Radius, Diameter(Meter(3.4))) ≈ Radius(Meter(1.7))
  #   @test convert(Diameter, Radius{MilliMeter}(1700)) ≈ Diameter(Meter(3.4))
  # end

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Diameter, U<:Radius} = isapprox(convert(Diameter, y), x, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Radius, U<:Diameter} = isapprox(convert(Diameter, x), y, atol=atol, rtol=rtol)
  # @testitem "Dimension: DiameterRadius isapprox" begin
  #   @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))
  #   @test Radius(Meter(1.7)) ≈ Diameter(Meter(3.4)) 
  # end

  

end