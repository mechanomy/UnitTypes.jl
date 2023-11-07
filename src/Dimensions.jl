module Dimensions

  export AbstractDiameter, Diameter, Radius
  abstract type AbstractDiameter <: AbstractDimension end # things relating to circular diameter
  @makeDimension Diameter AbstractDiameter
  @makeDimension Radius AbstractDiameter
  # Base.convert(::Type{Diameter}, x::Radius) = Diameter{typeof(x).parameters[1]}(x.value*2) 
  Base.convert(::Type{Diameter}, x::Radius) = Diameter(typeof(x).parameters[1](x.value*2))
  Base.convert(::Type{Radius}, x::Diameter) = Radius{typeof(x).parameters[1]}(x.value/2) 
  # @testitem "AbsDimension: DiameterRadius conversion" begin
  #   d = Diameter(Meter(3.4))
  #   @test convert(Radius, d) ≈ Meter(1.7)

  #   r = Radius(Meter(1.7))
  #   @test convert(Diameter, r) ≈ Meter(3.4)
    
  #   @test convert(Diameter{Inch}, Diameter(MilliMeter(25.4))).value ≈ 1.0
  #   # @test convert(Inch, Diameter(MilliMeter(25.4))).value ≈ 1.0

  #   # down-convert Diameter to Length?
  # # rod = convert(Float64,s.outside.value/2) # make a strip()?
  
  #   # @testset "things I want to do" begin
  #   #   @test isapprox( Inch(Foot(1.2)), Inch(14.4), atol=1e-3) #conversion by constructor
  #   # end
  # end

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Diameter, U<:Radius} = isapprox(convert(Diameter, y), x, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Radius, U<:Diameter} = isapprox(convert(Diameter, x), y, atol=atol, rtol=rtol)
  # @testitem "AbsDimension: DiameterRadius isapprox" begin
  #   @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))
  #   @test Radius(Meter(1.7)) ≈ Diameter(Meter(3.4)) 
  # end

  
  export AbstractLength, Length, Height, Width, Depth
  abstract type AbstractLength <: AbstractDimension end
  @makeDimension Length AbstractLength
  @makeDimension Height AbstractLength
  @makeDimension Width AbstractLength
  @makeDimension Depth AbstractLength


end