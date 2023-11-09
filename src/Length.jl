@makeBaseUnit Length Meter "m"
@testitem "Length conversions" begin
  @makeMeasure TestLength "te" 1.0 AbstractLength
  @makeMeasure TestLengthMilli "mte" 0.001 TestLength

  @test TestLength(1.0) ≈ TestLengthMilli(1000)
  @test TestLengthMilli(1000) ≈ TestLength(1.0) 
  @test TestLength(TestLengthMilli(1000)) ≈ 1.0
end
