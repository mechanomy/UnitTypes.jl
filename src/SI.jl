

module SI
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent

  export MilliMeter
  @makeMeasure MilliMeter "mm" 0.001 Meter

  # # makeMeasure MilliMeter 0.001 Meter "mm" # can I infer AbstractExtent from Meter, given that valid conversions are between siblings? supertype(meter) == AbstractExtent.
  # macro makeMeter(name, toBase, base, unit="")
  #   esc(
  #     quote
  #       struct $name <: $(supertype(typeof(base)))
  #         value::Number
  #         toBase::Number
  #         unit::String
  #         $name(x) = new(x,$toBase,$unit)
  #       end
  #       $name(x::T where T<:$(supertype(typeof(base)))) = convert($name, x) # conversion by constructor: Inch(x::T where T<:AbstractExtent) = convert(Inch, x)
  #     end
  #   )
  # end
  # export MakeMeter
  # # @makeMeter CentiMeter 0.01 Meter cm
  # # @show CentiMeter(1)




  # eventually move this standard consistency testing to some test module than can be run across all subtypes of AbstractExtent
  @testitem "testing SI" begin
    @test Meter(1.0) == Meter(1.0)
    @test Meter(1.0) ≈ MilliMeter(1000.0)
  end
end

