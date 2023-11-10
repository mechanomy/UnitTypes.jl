
@makeBaseUnit Current Ampere "A"
@testitem "Current conversions" begin
  @makeMeasure TestCurrent "te" 1.0 AbstractCurrent
  @makeMeasure TestCurrentMilli "mte" 0.001 TestCurrent

  @test Ampere(1) ≈ TestCurrent(1)
  @test TestCurrent(1.0) ≈ TestCurrentMilli(1000)
  @test TestCurrentMilli(1000) ≈ TestCurrent(1.0) 
  @test TestCurrent(TestCurrentMilli(1000)) ≈ 1.0
end

@makeBaseUnit Intensity Candela "cd"
@testitem "Intensity conversions" begin
  @makeMeasure TestIntensity "te" 1.0 AbstractIntensity
  @makeMeasure TestIntensityMilli "mte" 0.001 TestIntensity

  @test TestIntensity(1.0) ≈ TestIntensityMilli(1000)
  @test TestIntensityMilli(1000) ≈ TestIntensity(1.0) 
  @test TestIntensity(TestIntensityMilli(1000)) ≈ 1.0
end

@makeBaseUnit Length Meter "m"
@testitem "Length conversions" begin
  @makeMeasure TestLength "te" 1.0 AbstractLength
  @makeMeasure TestLengthMilli "mte" 0.001 TestLength

  @test TestLength(1.0) ≈ TestLengthMilli(1000)
  @test TestLengthMilli(1000) ≈ TestLength(1.0) 
  @test TestLength(TestLengthMilli(1000)) ≈ 1.0
end

@makeBaseUnit Mass Kilogram "kg"
@testitem "Test conversions" begin
  @makeMeasure TestMass "te" 1.0 AbstractMass
  @makeMeasure TestMassMilli "mte" 0.001 TestMass
  @test TestMass(1.0) ≈ TestMassMilli(1000)
  @test TestMassMilli(1000) ≈ TestMass(1.0) 
  @test TestMass(TestMassMilli(1000)) ≈ 1.0
end

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

@makeBaseUnit Time Second "s"
@testitem "Time conversions" begin
  @makeMeasure TestTime "te" 1.0 AbstractTime
  @makeMeasure TestTimeMilli "mte" 0.001 TestTime

  @test TestTime(1.0) ≈ TestTimeMilli(1000)
  @test TestTimeMilli(1000) ≈ TestTime(1.0) 
  @test TestTime(TestTimeMilli(1000)) ≈ 1.0
end