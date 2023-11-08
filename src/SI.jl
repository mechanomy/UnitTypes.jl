module SI
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent
  using ..SIBase

  export MilliMeter
  @makeMeasure MilliMeter "mm" 0.001 Meter

  # eventually move this standard consistency testing to some test module than can be run across all subtypes of AbstractExtent
  @testitem "testing SI" begin
    @test Meter(1.0) == Meter(1.0)
    @test Meter(1.0) ≈ MilliMeter(1000.0)
  end
end

