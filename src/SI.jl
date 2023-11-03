

module SI
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent

  export MilliMeter
  # @makeMeasure Meter AbstractExtent 1.0 "m" #Meter already defined as global base
  @makeMeasure MilliMeter AbstractExtent 0.001 "mm"

  # makeMeasure MilliMeter 0.001 Meter "mm" # can I infer AbstractExtent from Meter, given that valid conversions are between siblings? supertype(meter) == AbstractExtent.

  #rationale: test conversions between measures defined here
  # eventually move this standard consistency testing to some test module than can be run across all subtypes of AbstractExtent
  @testitem "testing SI" begin
    @test Meter(1.0) == Meter(1.0)
    @test Meter(1.0) ≈ MilliMeter(1000.0)
  end
end

