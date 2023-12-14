# following https://en.wikipedia.org/wiki/Imperial_units

mPerIn = 0.0254
inPerFt = 12
ftPerMi = 5280

@deriveMeasure Meter(mPerIn) = Inch(1) "in"
@deriveMeasure Meter(mPerIn*inPerFt) = Foot(1) "ft"
@deriveMeasure Meter(mPerIn*inPerFt*3) = Yard(1) "yd"
@deriveMeasure Meter(mPerIn*inPerFt*ftPerMi) = Mile(1) "mi"
@deriveMeasure Meter(1852) = NauticalMile(1) "nmi"

@deriveMeasure Meter2((mPerIn*inPerFt)^2) = SquareFoot(1) "sqft"
@deriveMeasure Meter2(4.046873e3) = Acre(1) "ac"
@deriveMeasure Meter2((mPerIn*inPerFt*ftPerMi)^2) = SquareMile(1) "sqmi"

@deriveMeasure MeterPerSecond(mPerIn*inPerFt) = FootPerSecond(1) "ft/s"

@deriveMeasure Meter3(28.4130625e-3) = FluidOunce(1) "floz"
@deriveMeasure Meter3(568.26126e-3) = Pint(1) "pt"
@deriveMeasure Meter3(1136.5225e-3) = Quart(1) "qt"
@deriveMeasure Meter3(4546.09e-3) = Gallon(1) "gal"

@deriveMeasure KiloGram(28.349523125e-3) = Ounce(1) "oz"
@deriveMeasure KiloGram(0.45359237) = PoundMass(1) "lbm"
@deriveMeasure KiloGram(14.59390294) = Slug(1) "slug"

@testitem "Imperial" begin
  @test isapprox(Inch(Foot(1.2)), Inch(14.4), atol=1e-6) # @test Inch(Foot(1.2)) ≈ Inch(14.4)
  @test isapprox(Acre(640), SquareMile(1), atol=0.1)
  @test isapprox(SquareFoot(5280^2), SquareMile(1), atol=0.1)
end