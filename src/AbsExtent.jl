


module AbsExtent
  using TestItems 
  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export AbstractExtent
  abstract type AbstractExtent <: AbstractMeasure end # an extent is a physical length or distance with a base unit of Meter

  @makeMeasureFromAbstract Meter "m" 1.0 AbstractExtent # need at least one Extent defined to enable conversion between all others
  export Meter

  #Since we already have the ability to convert between Measures, is this necessary?
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.isequal(x::T, y::U) where {T<:AbstractExtent, U<:AbstractExtent} = convert(Meter,x).value == convert(Meter,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:AbstractExtent} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo

  @testitem "AbsExtent conversions" begin
    # @makeMeasure TestMeasure AbstractMeasure 1.0 "tm"
    # @test Meter(1.0) ≈ TestMeasure(1.0) # this should fail because there we can only convert between AbstractExtents

    @makeMeasureFromAbstract TestExtent "te" 1.0 AbstractExtent
    @makeMeasure TestExtentMilli "mte" 0.001 TestExtent

    @test TestExtent(1.0) ≈ TestExtentMilli(1000)
    @test TestExtentMilli(1000) ≈ TestExtent(1.0) 
    @test TestExtent(TestExtentMilli(1000)) ≈ 1.0

    # @testset "things I want to do" begin
    #   @show typeof( Inch(1.0) / Inch(2.0) ) # == canceled units? - need to fix the convert()s
    #   @show typeof( Inch(1.0) * Foot(1.0) ) # == in^2 or ft^2?
    # end

  end



  # also define a unit constructor? namespace collisions anyone? probably better to imitate the u_str/DefaultUnits functionality
  # m(x::T) where T<:Number = Meter(x)
  # mm(x::T) where T<:Number = MilliMeter(x)

  # to Unitful
  # from Unitful


end
