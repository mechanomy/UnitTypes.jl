module Temperature
  using TestItems 
  using ..Measure 

  export tractTemperature
  abstract type tractTemperature <: tractMeasure end 

  @makeMeasure Kelvin "K" 1.0 tractTemperature

  Base.isequal(x::T, y::U) where {T<:tractTemperature, U<:tractTemperature} = convert(Kelvin,x).value == convert(Kelvin,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractTemperature, U<:tractTemperature} = isapprox(convert(Kelvin,x).value, convert(Kelvin,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractTemperature, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:tractTemperature, U<:tractTemperature} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

  @testitem "Temperature conversions" begin
    @makeMeasure TestTemperature "te" 1.0 tractTemperature
    @makeMeasure TestTemperatureMilli "mte" 0.001 TestTemperature

    @test TestTemperature(1.0) ≈ TestTemperatureMilli(1000)
    @test TestTemperatureMilli(1000) ≈ TestTemperature(1.0) 
    @test TestTemperature(TestTemperatureMilli(1000)) ≈ 1.0
  end

end