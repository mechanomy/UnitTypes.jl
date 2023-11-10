# following https://en.wikipedia.org/wiki/Imperial_units

mPerIn = 0.0254
inPerFt = 12
ftPerMi = 5280

@makeMeasure Inch "in" mPerIn Meter
@makeMeasure Foot "ft" mPerIn*inPerFt Meter
@makeMeasure Yard "yd" mPerIn*inPerFt*3 Meter
@makeMeasure Mile "mi" mPerIn*inPerFt*ftPerMi Meter
@makeMeasure NauticalMile "nmi" 1852 Meter

@makeMeasure SquareFoot "sqft" (mPerIn*inPerFt)^2 Meter2
@makeMeasure Acre "ac" 4840*(mPerIn*inPerFt)^2 Meter2
@makeMeasure SquareMile "sqmi" (mPerIn*inPerFt*ftPerMi)^2 Meter2

@makeMeasure FootPerSecond "ft/s" mPerIn*inPerFt MeterPerSecond



@makeMeasure FluidOunce "floz" 28.4130625e-3 Meter3
@makeMeasure Pint "pt" 568.26126e-3 Meter3
@makeMeasure Quart "qt" 1136.5225e-3 Meter3
@makeMeasure Gallon "gal" 4546.09e-3 Meter3

@makeMeasure Ounce "oz" 28.349523125e-3 Kilogram
@makeMeasure PoundMass "lbm" 0.45359237 Kilogram
@makeMeasure Slug "slug" 14.59390294 Kilogram



@testitem "Imperial" begin
  @test isapprox(Inch(Foot(1.2)), Inch(14.4), atol=1e-6) # @test Inch(Foot(1.2)) ≈ Inch(14.4)

  # @show typeof(SquareMile(1))
  # @show supertype(typeof(SquareMile(1)))
  # @show Meter(Foot(1))
  # @show Meter2(SquareFoot(1))
  # @show Meter2(SquareMile(1))
  @test isapprox(Meter2(SquareMile(1)), Meter2(SquareFoot(27878400)), atol=1e-6)
  @test isapprox(SquareMile(1), SquareFoot(27878400), atol=1e-6)

end