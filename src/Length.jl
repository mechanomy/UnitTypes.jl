module Length
  using TestItems 
  using ..Measure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export tractLength
  abstract type tractLength <: tractMeasure end # an extent is a physical length or distance with a base unit of Meter

  export Meter
  @makeMeasure Meter "m" 1.0 tractLength 
  # it's more natural to have these in Length than SIBase, but then there is a circular dependence with Meter defined here via functions found in Measure/Length that also need to import SIBase to use Meter..
  Base.isequal(x::T, y::U) where {T<:tractLength, U<:tractLength} = convert(Meter,x).value == convert(Meter,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractLength, U<:tractLength} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractLength, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:tractLength, U<:tractLength} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

  @testitem "Length conversions" begin
    @makeMeasure TestLength "te" 1.0 tractLength
    @makeMeasure TestLengthMilli "mte" 0.001 TestLength

    @test TestLength(1.0) ≈ TestLengthMilli(1000)
    @test TestLengthMilli(1000) ≈ TestLength(1.0) 
    @test TestLength(TestLengthMilli(1000)) ≈ 1.0
  end
end
