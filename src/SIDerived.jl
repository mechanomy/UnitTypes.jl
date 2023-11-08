module SIDerived
  using TestItems 
  using ..Measure
  using ..Current
  using ..Intensity
  using ..Length
  using ..Mass
  using ..Temperature
  using ..Time


  @makeMeasure MilliMeter "mm" 0.001 Meter
  @makeMeasure CentiMeter "cm" 0.01 Meter
  @makeMeasure DeciMeter "dm" 0.01 Meter
  @makeMeasure KiloMeter "km" 1000 Meter

  # eventually move this standard consistency testing to some test module than can be run across all subtypes of tractLength
  @testitem "testing SI" begin
    @test Meter(1.0) == Meter(1.0)
    @test Meter(1.0) ≈ MilliMeter(1000.0)
  end


  # @makeMeasure Newton "N" 

end

