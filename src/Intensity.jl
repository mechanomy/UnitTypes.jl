@makeBaseUnit Intensity Candela "cd"
@testitem "Intensity conversions" begin
  @makeMeasure TestIntensity "te" 1.0 AbstractIntensity
  @makeMeasure TestIntensityMilli "mte" 0.001 TestIntensity

  @test TestIntensity(1.0) ≈ TestIntensityMilli(1000)
  @test TestIntensityMilli(1000) ≈ TestIntensity(1.0) 
  @test TestIntensity(TestIntensityMilli(1000)) ≈ 1.0
end