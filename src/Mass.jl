@makeBaseUnit Mass Kilogram "kg"

@testitem "Test conversions" begin
  @makeMeasure TestMass "te" 1.0 AbstractMass
  @makeMeasure TestMassMilli "mte" 0.001 TestMass
  @test TestMass(1.0) ≈ TestMassMilli(1000)
  @test TestMassMilli(1000) ≈ TestMass(1.0) 
  @test TestMass(TestMassMilli(1000)) ≈ 1.0
end