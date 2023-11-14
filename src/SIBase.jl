
@makeBaseMeasure Current Ampere "A"
@testitem "Current conversions" begin
  @makeDerivedMeasure TestCurrent "te" 1.0 AbstractCurrent
  @makeDerivedMeasure TestCurrentMilli "mte" 0.001 TestCurrent

  @test Ampere(1) ≈ TestCurrent(1)
  @test TestCurrent(1.0) ≈ TestCurrentMilli(1000)
  @test TestCurrentMilli(1000) ≈ TestCurrent(1.0) 
  @test TestCurrent(TestCurrentMilli(1000)) ≈ 1.0
end

@makeBaseMeasure Intensity Candela "cd"
@testitem "Intensity conversions" begin
  @makeDerivedMeasure TestIntensity "te" 1.0 AbstractIntensity
  @makeDerivedMeasure TestIntensityMilli "mte" 0.001 TestIntensity

  @test TestIntensity(1.0) ≈ TestIntensityMilli(1000)
  @test TestIntensityMilli(1000) ≈ TestIntensity(1.0) 
  @test TestIntensity(TestIntensityMilli(1000)) ≈ 1.0
end

@makeBaseMeasure Length Meter "m"
@testitem "Length conversions" begin
  @makeDerivedMeasure TestLength "te" 1.0 AbstractLength
  @makeDerivedMeasure TestLengthMilli "mte" 0.001 TestLength

  @test TestLength(1.0) ≈ TestLengthMilli(1000)
  @test TestLengthMilli(1000) ≈ TestLength(1.0) 
  @test TestLength(TestLengthMilli(1000)) ≈ 1.0
end

@makeBaseMeasure Mass Kilogram "kg"
@testitem "Test conversions" begin
  @makeDerivedMeasure TestMass "te" 1.0 AbstractMass
  @makeDerivedMeasure TestMassMilli "mte" 0.001 TestMass
  @test TestMass(1.0) ≈ TestMassMilli(1000)
  @test TestMassMilli(1000) ≈ TestMass(1.0) 
  @test TestMass(TestMassMilli(1000)) ≈ 1.0
end

@makeBaseMeasure Temperature Kelvin "K"
@testitem "Temperature conversions" begin
  @makeDerivedMeasure TestTemp "tt" 1.0 AbstractTemperature
  @makeDerivedMeasure TestTempMilli "ttm" 0.001 TestTemp

  @test TestTemp(1.0) ≈ 1.0
  @test TestTempMilli(1000) ≈ 1000

  @test TestTemp(1.0) ≈ TestTempMilli(1000.0)
  @test TestTempMilli(1000) ≈ TestTemp(1.0) 
  @test TestTemp(TestTempMilli(1000)) ≈ 1.0
end

@makeBaseMeasure Time Second "s"
@testitem "Time conversions" begin
  @makeDerivedMeasure TestTime "te" 1.0 AbstractTime
  @makeDerivedMeasure TestTimeMilli "mte" 0.001 TestTime

  @test TestTime(1.0) ≈ TestTimeMilli(1000)
  @test TestTimeMilli(1000) ≈ TestTime(1.0) 
  @test TestTime(TestTimeMilli(1000)) ≈ 1.0
end