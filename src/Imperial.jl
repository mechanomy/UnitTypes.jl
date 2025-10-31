# following https://en.wikipedia.org/wiki/Imperial_units

mPerIn = 0.0254
inPerFt = 12
ftPerMi = 5280

@makeMeasure Meter = Inch "in" mPerIn
@makeMeasure Meter = Foot "ft" mPerIn*inPerFt
@makeMeasure Meter = Yard "yd" mPerIn*inPerFt*3
@makeMeasure Meter = Mile "mi" mPerIn*inPerFt*ftPerMi
@makeMeasure Meter = NauticalMile "nmi" 1852

@makeMeasure Meter2 = Inch2 "in^2" mPerIn ^2
@makeMeasure Meter2 = Foot2 "sqft" (mPerIn*inPerFt)^2
@makeMeasure Meter2 = Acre "ac" 4.046873e3
@makeMeasure Meter2 = Mile2 "sqmi" (mPerIn*inPerFt*ftPerMi)^2

@makeMeasure MeterPerSecond = FootPerSecond "ft/s" 1/mPerIn*inPerFt

@makeMeasure Meter3 = FluidOunce "floz" 28.4130625e-3
@makeMeasure Meter3 = Pint "pt" 568.26126e-3
@makeMeasure Meter3 = Quart "qt" 1136.5225e-3
@makeMeasure Meter3 = Gallon "gal" 4546.09e-3

@makeMeasure KiloGram = Ounce "oz" 28.349523125e-3
@makeMeasure KiloGram = PoundMass "lbm" 0.45359237

@makeMeasure KiloGram = Slug "slug" 14.59390294
@makeMeasure Newton = PoundForce "lbf" 0.22481

@testitem "Imperial" begin
  @test isapprox(Inch(12), Foot(1), atol=1e-3) # @test Inch(12) ≈ Foot(1)
  @test Foot(3) ≈ Yard(1)
  @test Foot(5280) ≈ Mile(1)
  @test isapprox(Acre(640), Mile2(1), atol=0.1)
  @test isapprox(Foot2(5280^2), Mile2(1), atol=0.1)

  @test isapprox(Slug(1), PoundMass(32.174), atol=1e-3)
end