# following https://en.wikipedia.org/wiki/Imperial_units

mPerIn = 0.0254
inPerFt = 12
ftPerMi = 5280

@makeDerivedMeasure Inch "in" mPerIn Meter
@makeDerivedMeasure Foot "ft" mPerIn*inPerFt Meter
@makeDerivedMeasure Yard "yd" mPerIn*inPerFt*3 Meter
@makeDerivedMeasure Mile "mi" mPerIn*inPerFt*ftPerMi Meter
@makeDerivedMeasure NauticalMile "nmi" 1852 Meter

@makeDerivedMeasure SquareFoot "ft^2" (mPerIn*inPerFt)^2 Meter2
@makeDerivedMeasure Acre "ac" 4840*(mPerIn*inPerFt)^2 Meter2
@makeDerivedMeasure SquareMile "mi^2" (mPerIn*inPerFt*ftPerMi)^2 Meter2

@makeDerivedMeasure FootPerSecond "ft/s" mPerIn*inPerFt MeterPerSecond


@makeDerivedMeasure FluidOunce "floz" 28.4130625e-3 Meter3
@makeDerivedMeasure Pint "pt" 568.26126e-3 Meter3
@makeDerivedMeasure Quart "qt" 1136.5225e-3 Meter3
@makeDerivedMeasure Gallon "gal" 4546.09e-3 Meter3

@makeDerivedMeasure Ounce "oz" 28.349523125e-3 Kilogram
@makeDerivedMeasure PoundMass "lbm" 0.45359237 Kilogram
@makeDerivedMeasure Slug "slug" 14.59390294 Kilogram



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