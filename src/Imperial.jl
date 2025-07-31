# following https://en.wikipedia.org/wiki/Imperial_units

mPerIn = 0.0254
inPerFt = 12
ftPerMi = 5280

@makeMeasure Meter(mPerIn) = Inch(1) "in"
@makeMeasure Meter(mPerIn*inPerFt) = Foot(1) "ft"
@makeMeasure Meter(mPerIn*inPerFt*3) = Yard(1) "yd"
@makeMeasure Meter(mPerIn*inPerFt*ftPerMi) = Mile(1) "mi"
@makeMeasure Meter(1852) = NauticalMile(1) "nmi"

@makeMeasure Meter2(mPerIn ^2) = Inch2(1) "in^2"
@makeMeasure Meter2((mPerIn*inPerFt)^2) = Foot2(1) "sqft"
@makeMeasure Meter2(4.046873e3) = Acre(1) "ac"
@makeMeasure Meter2((mPerIn*inPerFt*ftPerMi)^2) = Mile2(1) "sqmi"

@makeMeasure MeterPerSecond(mPerIn*inPerFt) = FootPerSecond(1) "ft/s"

@makeMeasure Meter3(28.4130625e-3) = FluidOunce(1) "floz"
@makeMeasure Meter3(568.26126e-3) = Pint(1) "pt"
@makeMeasure Meter3(1136.5225e-3) = Quart(1) "qt"
@makeMeasure Meter3(4546.09e-3) = Gallon(1) "gal"

@makeMeasure KiloGram(28.349523125e-3) = Ounce(1) "oz"
@makeMeasure KiloGram(0.45359237) = PoundMass(1) "lbm"
@makeMeasure KiloGram(14.59390294) = Slug(1) "slug"

@makeMeasure Newton(1) = PoundForce(0.22481) "lbf"


@testitem "Imperial" begin
  @test Inch(12) ≈ Foot(1)
  @test Foot(3) ≈ Yard(1)
  @test Foot(5280) ≈ Mile(1)
  @test isapprox(Acre(640), Mile2(1), atol=0.1)
  @test isapprox(Foot2(5280^2), Mile2(1), atol=0.1)
end