@makeBaseUnit Temperature Kelvin "K"

@testitem "Temperature conversions" begin
  @makeMeasure TestTemp "tt" 1.0 AbstractTemperature
  @makeMeasure TestTempMilli "ttm" 0.001 TestTemp

  @test TestTemp(1.0) ≈ 1.0
  @test TestTempMilli(1000) ≈ 1000

  @test TestTemp(1.0) ≈ TestTempMilli(1000.0)
  @test TestTempMilli(1000) ≈ TestTemp(1.0) 
  @test TestTemp(TestTempMilli(1000)) ≈ 1.0
end