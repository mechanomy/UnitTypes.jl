# following https://en.wikipedia.org/wiki/Imperial_units

const mPerIn = 0.0254
const inPerFt = 12
const ftPerMi = 5280

@makeMeasure mPerIn Meter = 1 Inch "in"
@makeMeasure mPerIn*inPerFt Meter = 1 Foot "ft"
@makeMeasure mPerIn*inPerFt*3 Meter = 1 Yard "yd"
@makeMeasure mPerIn*inPerFt*ftPerMi Meter = 1 Mile "mi"
@makeMeasure 1852 Meter = 1 NauticalMile "nmi"

@makeMeasure mPerIn^2 Meter2 = 1 Inch2 "in^2"
@makeMeasure (mPerIn*inPerFt)^2 Meter2 = 1 Foot2 "sqft"
@makeMeasure 4.046873e3 Meter2 = 1 Acre "ac"
@makeMeasure (mPerIn*inPerFt*ftPerMi)^2 Meter2 = 1 Mile2 "sqmi"

@makeMeasure 1/mPerIn*inPerFt MeterPerSecond = 1 FootPerSecond "ft/s"

@makeMeasure 28.4130625e-3 Meter3 = 1 FluidOunce "floz"
@makeMeasure 568.26126e-3 Meter3 = 1 Pint "pt"
@makeMeasure 1136.5225e-3 Meter3 = 1 Quart "qt"
@makeMeasure 4546.09e-3 Meter3 = 1 Gallon "gal"

@makeMeasure 28.349523125e-3 KiloGram = 1 Ounce "oz"
@makeMeasure 0.45359237 KiloGram = 1 PoundMass "lbm"

@makeMeasure 14.59390294 KiloGram = 1 Slug "slug"
@makeMeasure 4.448 Newton = 1 PoundForce "lbf"

@testitem "Imperial" begin
  @test isapprox(Inch(12), Foot(1), atol=1e-3) # @test Inch(12) ≈ Foot(1)
  @test Foot(3) ≈ Yard(1)
  @test Foot(5280) ≈ Mile(1)
  @test isapprox(Acre(640), Mile2(1), atol=0.1)
  @test isapprox(Foot2(5280^2), Mile2(1), atol=0.1)

  @test isapprox(Slug(1), PoundMass(32.174), atol=1e-3)
  @test isapprox(Newton(10), PoundForce(2.2481), atol=1e-3)
  @test isapprox(PoundForce(10), Newton(44.48), atol=1e-3)

  @test 1u"in" ≈ Inch(1)
  @test MilliMeter(1)*MilliMeter(2) ≈ Meter2(2e-6)

  @test Meter(Inch(1))*Meter(Inch(2)) ≈ Meter2(0.00129032)
  @test Inch(1)*Inch(2) ≈ Meter2(0.00129032)
  @test Inch(1)*Inch(2) ≈ Inch2(2)
  @test isapprox(Inch(1)*Foot(1), Inch2(12), atol=1e-3)
  @test isapprox(Foot(1)*Foot(2), Inch2(12*24), atol=1e-3)
  @test Inch2(1)/Inch(2) ≈ Inch(0.5)
end