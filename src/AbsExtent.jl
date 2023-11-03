


module AbsExtent
  using TestItems 
  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export AbstractExtent
  abstract type AbstractExtent <: AbstractMeasure end # an extent is a physical length or distance with a base unit of Meter

  @makeMeasure Meter AbstractExtent 1.0 "m" # need at least one Extent defined to enable conversion between all others
  export Meter

  # include("SI.jl") # already included in UnitTypes.jl
  # using .SI # AbsExtent does not use SI, the testitem runs in its own namespace with a default using PackageName...
  # include("Imperial.jl")
  # using .Imperial

  #Since we already have the ability to convert between Measures, is this necessary?
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
  Base.isequal(x::T, y::U) where {T<:AbstractExtent, U<:AbstractExtent} = convert(Meter,x).value == convert(Meter,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:AbstractExtent} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo

  @testitem "AbsExtent conversions" begin
    # @makeMeasure TestMeasure AbstractMeasure 1.0 "tm"
    # @test Meter(1.0) ≈ TestMeasure(1.0) # this should fail because there we can only convert between AbstractExtents


    @test Inch(1.0) ≈ MilliMeter(25.4)
    @test Inch(12.0) ≈ Foot(1.0)

    @test MilliMeter(Inch(1.0)) ≈ 25.4


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
