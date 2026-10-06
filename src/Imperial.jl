# following https://en.wikipedia.org/wiki/Imperial_units

const mPerIn = 0.0254
const inPerFt = 12
const ftPerMi = 5280

@makeMeasure mPerIn Meter = 1 Inch "in"
@makeMeasure mPerIn*1e-3 Meter = 1 Mil "mil"
@makeMeasure mPerIn*inPerFt Meter = 1 Foot "ft"
@makeMeasure mPerIn*inPerFt*3 Meter = 1 Yard "yd"
@makeMeasure mPerIn*inPerFt*ftPerMi Meter = 1 Mile "mi"
@makeMeasure 1852 Meter = 1 NauticalMile "nmi"

@makeMeasure mPerIn^2 Meter2 = 1 Inch2 "in^2"
@makeMeasure (mPerIn*inPerFt)^2 Meter2 = 1 Foot2 "sqft"
@makeMeasure (mPerIn*inPerFt)^2*43560 Meter2 = 1 Acre "ac" # international acre, 4046.8564224 m^2
@makeMeasure (mPerIn*inPerFt*ftPerMi)^2 Meter2 = 1 Mile2 "sqmi"

@makeMeasure mPerIn*inPerFt MeterPerSecond = 1 FootPerSecond "ft/s"
@makeMeasure 0.44704 MeterPerSecond = 1 MilePerHour "mi/hr"
@makeMeasure 0.44704 MeterPerSecond = 1 MilesPerHour "mph"

@testitem "Velocity" begin
  @test isapprox( MilePerHour(1), KiloMeterPerHour(1.609), atol=1e-3)
end


@makeMeasure 28.4130625e-6 Meter3 = 1 FluidOunce "floz"
@makeMeasure 568.26125e-6 Meter3 = 1 Pint "pt"
@makeMeasure 1136.5225e-6 Meter3 = 1 Quart "qt"
@makeMeasure 4546.09e-6 Meter3 = 1 Gallon "gal"
@testitem "Imperial volume" begin
  @test isapprox(Gallon(1), Liter(4.54609), rtol=1e-12) # regression: these were defined 1000x too large
  @test isapprox(Gallon(1), Quart(4), rtol=1e-12)
  @test isapprox(Quart(1), Pint(2), rtol=1e-12)
  @test isapprox(Pint(1), FluidOunce(20), rtol=1e-12)
end

@makeMeasure 28.349523125e-3 KiloGram = 1 Ounce "oz"
@makeMeasure 0.45359237 KiloGram = 1 PoundMass "lbm"
@makeMeasure 0.0017718451953125 KiloGram = 1 Dram "dr"
@makeMeasure 6.479891e-5 KiloGram = 1 Grain "gr"

@makeMeasure 14.59390294 KiloGram = 1 Slug "slug"
@makeMeasure 4.4482216152605 Newton = 1 PoundForce "lbf"
@makeMeasure 6894.757293168361 Pascal = 1 PoundsPerSquareInch "psi"
@makeMeasure 4.184 Joule = 1 Calorie "cal"
@makeMeasure 1055.06 Joule = 1 BritishThermalUnit "btu"

@testitem "Imperial" begin
  @test isapprox(Inch(12), Foot(1), atol=1e-3) # @test Inch(12) ≈ Foot(1)
  @test Foot(3) ≈ Yard(1)
  @test Foot(5280) ≈ Mile(1)
  @test isapprox(Acre(640), Mile2(1), rtol=1e-12)
  @test isapprox(Acre(1), Meter2(4046.8564224), rtol=1e-12)
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