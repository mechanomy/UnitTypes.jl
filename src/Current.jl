@makeBaseUnit Current Ampere "A"
@testitem "Current conversions" begin
  @makeMeasure TestCurrent "te" 1.0 AbstractCurrent
  @makeMeasure TestCurrentMilli "mte" 0.001 TestCurrent

  @test Ampere(1) ≈ TestCurrent(1)
  @test TestCurrent(1.0) ≈ TestCurrentMilli(1000)
  @test TestCurrentMilli(1000) ≈ TestCurrent(1.0) 
  @test TestCurrent(TestCurrentMilli(1000)) ≈ 1.0
end