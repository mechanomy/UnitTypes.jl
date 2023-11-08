module AbsExtent
  using TestItems 
  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export AbstractExtent
  abstract type AbstractExtent <: AbstractMeasure end # an extent is a physical length or distance with a base unit of Meter

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.convert(::Type{T}, x::U) where {T<:AbstractExtent, U<:AbstractExtent} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...


  @testitem "AbsExtent conversions" begin
    @makeMeasure TestExtent "te" 1.0 AbstractExtent
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
