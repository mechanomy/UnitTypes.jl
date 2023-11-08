module Current
  using TestItems 
  using ..Measure 

  export tractCurrent
  abstract type tractCurrent <: tractMeasure end 

  @makeMeasure Ampere "A" 1.0 tractCurrent

  Base.isequal(x::T, y::U) where {T<:tractCurrent, U<:tractCurrent} = convert(Ampere,x).value == convert(Ampere,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractCurrent, U<:tractCurrent} = isapprox(convert(Ampere,x).value, convert(Ampere,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractCurrent, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:tractCurrent, U<:tractCurrent} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

  @testitem "Current conversions" begin
    @makeMeasure TestCurrent "te" 1.0 tractCurrent
    @makeMeasure TestCurrentMilli "mte" 0.001 TestCurrent

    @test TestCurrent(1.0) ≈ TestCurrentMilli(1000)
    @test TestCurrentMilli(1000) ≈ TestCurrent(1.0) 
    @test TestCurrent(TestCurrentMilli(1000)) ≈ 1.0
  end

end