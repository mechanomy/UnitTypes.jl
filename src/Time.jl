@makeBaseUnit Time Second "s"

@testitem "Time conversions" begin
  @makeMeasure TestTime "te" 1.0 AbstractTime
  @makeMeasure TestTimeMilli "mte" 0.001 TestTime

  @test TestTime(1.0) ≈ TestTimeMilli(1000)
  @test TestTimeMilli(1000) ≈ TestTime(1.0) 
  @test TestTime(TestTimeMilli(1000)) ≈ 1.0
end