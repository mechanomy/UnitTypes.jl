module Mass
  using TestItems 
  using ..Measure 

  export tractMass
  abstract type tractMass <: tractMeasure end 

  @makeMeasure Kilogram "kg" 1.0 tractMass

  Base.isequal(x::T, y::U) where {T<:tractMass, U<:tractMass} = convert(Kilogram,x).value == convert(Kilogram,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractMass, U<:tractMass} = isapprox(convert(Kilogram,x).value, convert(Kilogram,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractMass, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:tractMass, U<:tractMass} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

  @testitem "Mass conversions" begin
    @makeMeasure TestMass "te" 1.0 tractMass
    @makeMeasure TestMassMilli "mte" 0.001 TestMass

    @test TestMass(1.0) ≈ TestMassMilli(1000)
    @test TestMassMilli(1000) ≈ TestMass(1.0) 
    @test TestMass(TestMassMilli(1000)) ≈ 1.0
  end

end