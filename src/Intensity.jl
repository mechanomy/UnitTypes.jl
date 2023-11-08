module Intensity
  using TestItems 
  using ..Measure 

  export tractIntensity
  abstract type tractIntensity <: tractMeasure end 

  @makeMeasure Candela "cd" 1.0 tractIntensity

  Base.isequal(x::T, y::U) where {T<:tractIntensity, U<:tractIntensity} = convert(Candela,x).value == convert(Candela,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractIntensity, U<:tractIntensity} = isapprox(convert(Candela,x).value, convert(Candela,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractIntensity, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:tractIntensity, U<:tractIntensity} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

  @testitem "Intensity conversions" begin
    @makeMeasure TestIntensity "te" 1.0 tractIntensity
    @makeMeasure TestIntensityMilli "mte" 0.001 TestIntensity

    @test TestIntensity(1.0) ≈ TestIntensityMilli(1000)
    @test TestIntensityMilli(1000) ≈ TestIntensity(1.0) 
    @test TestIntensity(TestIntensityMilli(1000)) ≈ 1.0
  end

end