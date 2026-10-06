# following https://en.wikipedia.org/wiki/United_States_customary_units
# Units shared with the imperial system (Inch, Foot, Yard, Mile, Ounce, PoundMass, Grain, Dram, ...) are defined in Imperial.jl.
# Volumes whose US and imperial definitions differ carry a US prefix, so Gallon remains the imperial gallon and USGallon is the US gallon.
# Every @makeMeasure references the base unit of its quantity, since toBase converts into the referenced type.

const mPerFt = mPerIn*inPerFt
const mPerSurveyFt = 1200/3937 # US survey foot, deprecated by NIST at the end of 2022
const m3PerUSGallon = 231*mPerIn^3 # 3.785411784 L
const m3PerUSDryGallon = 268.8025*mPerIn^3 # 4.40488377086 L
const kgPerLb = 0.45359237
const kgPerGrain = kgPerLb/7000
const nPerLbf = 4.4482216152605
const jPerBtu = 1055.06 # matches BritishThermalUnit in Imperial.jl

# length
@makeMeasure mPerIn/72 Meter = 1 Point "point"
@makeMeasure mPerIn/6 Meter = 1 Pica "pica"
@makeMeasure mPerFt*6 Meter = 1 Fathom "ftm"
@makeMeasure mPerFt*720 Meter = 1 Cable "cb"

# survey lengths, defined on the international foot since 2023
@makeMeasure mPerFt*0.66 Meter = 1 Link "li"
@makeMeasure mPerFt*16.5 Meter = 1 Rod "rd"
@makeMeasure mPerFt*66 Meter = 1 Chain "ch"
@makeMeasure mPerFt*660 Meter = 1 Furlong "fur"
@makeMeasure mPerFt*ftPerMi*3 Meter = 1 League "lea"
@makeMeasure mPerSurveyFt Meter = 1 SurveyFoot "ftUS"
@makeMeasure mPerSurveyFt*ftPerMi Meter = 1 SurveyMile "miUS"

@testitem "USCustomary length" begin
  @test isapprox(Point(72), Inch(1), rtol=1e-12)
  @test isapprox(Pica(6), Inch(1), rtol=1e-12)
  @test isapprox(Pica(1), Point(12), rtol=1e-12)
  @test isapprox(Fathom(1), Yard(2), rtol=1e-12)
  @test isapprox(Cable(1), Fathom(120), rtol=1e-12)
  @test isapprox(Rod(1), Link(25), rtol=1e-12)
  @test isapprox(Chain(1), Rod(4), rtol=1e-12)
  @test isapprox(Furlong(1), Chain(10), rtol=1e-12)
  @test isapprox(Mile(1), Furlong(8), rtol=1e-12)
  @test isapprox(League(1), Mile(3), rtol=1e-12)
  @test isapprox(SurveyFoot(1), Meter(1200/3937), rtol=1e-12)
  @test isapprox(SurveyFoot(1), Foot(1.000002), atol=1e-7)
  @test isapprox(SurveyMile(1), SurveyFoot(5280), rtol=1e-12)
  @test 1u"ftm" isa Fathom
end

# area
@makeMeasure mPerFt^2*9 Meter2 = 1 Yard2 "sqyd"
@makeMeasure mPerFt^2*66^2 Meter2 = 1 Chain2 "sqch"
@makeMeasure (mPerFt*ftPerMi)^2 Meter2 = 1 Section "section"
@makeMeasure (mPerFt*ftPerMi)^2*36 Meter2 = 1 Township "twp"

@testitem "USCustomary area" begin
  @test isapprox(Yard2(1), Foot2(9), rtol=1e-12)
  @test isapprox(Yard(1)*Yard(1), Yard2(1), rtol=1e-12)
  @test isapprox(Chain(1)*Chain(1), Chain2(1), rtol=1e-12)
  @test isapprox(Chain2(10), Acre(1), rtol=1e-12)
  @test isapprox(Section(1), Mile2(1), rtol=1e-12)
  @test isapprox(Township(1), Section(36), rtol=1e-12)
end

# volume
@makeMeasure mPerIn^3 Meter3 = 1 Inch3 "in^3"
@makeMeasure mPerFt^3 Meter3 = 1 Foot3 "ft^3"
@makeMeasure mPerFt^3*27 Meter3 = 1 Yard3 "yd^3"
@makeMeasure mPerFt^3*43560 Meter3 = 1 AcreFoot "ac*ft"

@testitem "USCustomary volume" begin
  @test isapprox(Foot3(1), Inch3(1728), rtol=1e-12)
  @test isapprox(Yard3(1), Foot3(27), rtol=1e-12)
  @test isapprox(Inch(1)*Inch(1)*Inch(1), Inch3(1), rtol=1e-12)
  @test isapprox(Foot2(1)*Foot(1), Foot3(1), rtol=1e-12)
  @test isapprox(AcreFoot(1), Meter3(1233.48183754752), rtol=1e-12)
  @test 1u"ft^3" isa Foot3
end

# fluid volume
@makeMeasure m3PerUSGallon/128/480 Meter3 = 1 USMinim "usminim"
@makeMeasure m3PerUSGallon/128/8 Meter3 = 1 USFluidDram "usfldr"
@makeMeasure m3PerUSGallon/128/6 Meter3 = 1 USTeaspoon "tsp"
@makeMeasure m3PerUSGallon/128/2 Meter3 = 1 USTablespoon "tbsp"
@makeMeasure m3PerUSGallon/128 Meter3 = 1 USFluidOunce "usfloz"
@makeMeasure m3PerUSGallon/128*1.5 Meter3 = 1 USJigger "jig"
@makeMeasure m3PerUSGallon/32 Meter3 = 1 USGill "usgi"
@makeMeasure m3PerUSGallon/16 Meter3 = 1 USCup "cup"
@makeMeasure m3PerUSGallon/8 Meter3 = 1 USPint "uspt"
@makeMeasure m3PerUSGallon/4 Meter3 = 1 USQuart "usqt"
@makeMeasure m3PerUSGallon Meter3 = 1 USGallon "usgal"
@makeMeasure m3PerUSGallon*31.5 Meter3 = 1 USBarrel "usbbl"
@makeMeasure m3PerUSGallon*42 Meter3 = 1 OilBarrel "bbl"
@makeMeasure m3PerUSGallon*63 Meter3 = 1 USHogshead "hhd"

@testitem "USCustomary fluid volume" begin
  @test isapprox(USGallon(1), Liter(3.785411784), rtol=1e-12)
  @test isapprox(USGallon(1), Inch3(231), rtol=1e-12)
  @test isapprox(USGallon(1), USQuart(4), rtol=1e-12)
  @test isapprox(USQuart(1), USPint(2), rtol=1e-12)
  @test isapprox(USPint(1), USCup(2), rtol=1e-12)
  @test isapprox(USCup(1), USGill(2), rtol=1e-12)
  @test isapprox(USGill(1), USFluidOunce(4), rtol=1e-12)
  @test isapprox(USFluidOunce(1), USTablespoon(2), rtol=1e-12)
  @test isapprox(USTablespoon(1), USTeaspoon(3), rtol=1e-12)
  @test isapprox(USFluidOunce(1), USFluidDram(8), rtol=1e-12)
  @test isapprox(USFluidDram(1), USMinim(60), rtol=1e-12)
  @test isapprox(USJigger(1), USFluidOunce(1.5), rtol=1e-12)
  @test isapprox(USBarrel(2), USHogshead(1), rtol=1e-12)
  @test isapprox(OilBarrel(1), USGallon(42), rtol=1e-12)
  @test isapprox(Gallon(1), USGallon(1.20095), atol=1e-5) # imperial gallon is larger
  @test 1u"usgal" isa USGallon
end

# dry volume
@makeMeasure m3PerUSDryGallon/8 Meter3 = 1 USDryPint "usdrypt"
@makeMeasure m3PerUSDryGallon/4 Meter3 = 1 USDryQuart "usdryqt"
@makeMeasure m3PerUSDryGallon Meter3 = 1 USDryGallon "usdrygal"
@makeMeasure m3PerUSDryGallon*2 Meter3 = 1 USPeck "uspk"
@makeMeasure m3PerUSDryGallon*8 Meter3 = 1 USBushel "usbu"
@makeMeasure mPerIn^3*7056 Meter3 = 1 USDryBarrel "usdrybbl"

@testitem "USCustomary dry volume" begin
  @test isapprox(USDryQuart(1), USDryPint(2), rtol=1e-12)
  @test isapprox(USDryGallon(1), USDryQuart(4), rtol=1e-12)
  @test isapprox(USPeck(1), USDryGallon(2), rtol=1e-12)
  @test isapprox(USBushel(1), USPeck(4), rtol=1e-12)
  @test isapprox(USBushel(1), Inch3(2150.42), rtol=1e-12)
  @test isapprox(USDryBarrel(1), USBushel(3.281), atol=1e-3)
  @test isapprox(USDryPint(1), Liter(0.5506104713575), atol=1e-12)
end

# mass
@makeMeasure kgPerLb*100 KiloGram = 1 ShortHundredweight "cwt"
@makeMeasure kgPerLb*112 KiloGram = 1 LongHundredweight "lcwt"
@makeMeasure kgPerLb*2000 KiloGram = 1 ShortTon "ton"
@makeMeasure kgPerLb*2240 KiloGram = 1 LongTon "LT"

# troy mass, for precious metals
@makeMeasure kgPerGrain*24 KiloGram = 1 Pennyweight "dwt"
@makeMeasure kgPerGrain*480 KiloGram = 1 TroyOunce "ozt"
@makeMeasure kgPerGrain*5760 KiloGram = 1 TroyPound "lbt"

@testitem "USCustomary mass" begin
  @test isapprox(ShortHundredweight(1), PoundMass(100), rtol=1e-12)
  @test isapprox(LongHundredweight(1), PoundMass(112), rtol=1e-12)
  @test isapprox(ShortTon(1), ShortHundredweight(20), rtol=1e-12)
  @test isapprox(LongTon(1), LongHundredweight(20), rtol=1e-12)
  @test isapprox(ShortTon(1), KiloGram(907.18474), rtol=1e-12)
  @test isapprox(Pennyweight(1), Grain(24), rtol=1e-12)
  @test isapprox(TroyOunce(1), Pennyweight(20), rtol=1e-12)
  @test isapprox(TroyPound(1), TroyOunce(12), rtol=1e-12)
  @test isapprox(TroyOunce(1), Gram(31.1034768), rtol=1e-12)
  @test 1u"ozt" isa TroyOunce
end

# force, pressure
@makeMeasure nPerLbf*1000 Newton = 1 KiloPoundForce "kip"
@makeMeasure nPerLbf/mPerFt^2 Pascal = 1 PoundsPerSquareFoot "psf"
@makeMeasure nPerLbf*1000/mPerIn^2 Pascal = 1 KiloPoundsPerSquareInch "ksi"
@makeMeasure 3386.389 Pascal = 1 InchOfMercury "inHg"
@makeMeasure 249.08891 Pascal = 1 InchOfWater "inH2O"

@testitem "USCustomary force and pressure" begin
  @test isapprox(KiloPoundForce(1), PoundForce(1000), rtol=1e-12)
  @test isapprox(PoundsPerSquareInch(1), PoundsPerSquareFoot(144), rtol=1e-12)
  @test isapprox(KiloPoundsPerSquareInch(1), PoundsPerSquareInch(1000), rtol=1e-12)
  @test isapprox(PoundForce(1)/Foot2(1), PoundsPerSquareFoot(1), rtol=1e-12)
  @test isapprox(InchOfMercury(29.92), Atmosphere(1), atol=1e-3)
  @test isapprox(InchOfWater(1), Pascal(249.08891), atol=1e-6)
end

# torque
@makeMeasure nPerLbf*mPerFt NewtonMeter = 1 PoundFoot "lbf*ft"
@makeMeasure nPerLbf*mPerIn NewtonMeter = 1 PoundInch "lbf*in"

@testitem "USCustomary torque" begin
  @test isapprox(PoundFoot(1), PoundInch(12), rtol=1e-12)
  @test isapprox(PoundFoot(1), NewtonMeter(1.3558179483314004), atol=1e-12)
  @test isapprox(PoundForce(2)*Foot(3), PoundFoot(6), rtol=1e-12)
  @test 1u"lbf*ft" isa PoundFoot
end

# energy, power
@makeMeasure nPerLbf*mPerFt Joule = 1 FootPound "ft*lbf"
@makeMeasure jPerBtu*1e5 Joule = 1 Therm "thm"
@makeMeasure 745.69987158227022 Watt = 1 Horsepower "hp"
@makeMeasure jPerBtu/3600 Watt = 1 BritishThermalUnitPerHour "btu/hr"
@makeMeasure jPerBtu*12000/3600 Watt = 1 TonOfRefrigeration "TR"

@testitem "USCustomary energy and power" begin
  @test isapprox(FootPound(1), Joule(1.3558179483314004), atol=1e-12)
  @test isapprox(Therm(1), BritishThermalUnit(1e5), rtol=1e-12)
  @test isapprox(Horsepower(1), Watt(745.7), atol=1e-2)
  @test isapprox(TonOfRefrigeration(1), BritishThermalUnitPerHour(12000), rtol=1e-12)
  @test 1u"ft*lbf" isa FootPound
  @test 1u"hp" isa Horsepower
end

# velocity, acceleration
@makeMeasure mPerFt/60 MeterPerSecond = 1 FootPerMinute "ft/min"
@makeMeasure mPerIn MeterPerSecond = 1 InchPerSecond "in/s"
@makeMeasure 1852/3600 MeterPerSecond = 1 Knot "kn"
@makeMeasure mPerFt MeterPerSecond2 = 1 FootPerSecond2 "ft/s^2"

@testitem "USCustomary velocity and acceleration" begin
  @test isapprox(FootPerSecond(1), FootPerMinute(60), rtol=1e-12)
  @test isapprox(FootPerSecond(1), InchPerSecond(12), rtol=1e-12)
  @test isapprox(Knot(1), MilePerHour(1.150779), atol=1e-6)
  @test isapprox(Foot(10)/Second(2), FootPerSecond(5), rtol=1e-12)
  @test isapprox(EarthGravity(1), FootPerSecond2(32.174), atol=1e-3)
end

# density
@makeMeasure kgPerLb/mPerFt^3 KgPerM3 = 1 PoundMassPerFoot3 "lbm/ft^3"
@makeMeasure kgPerLb/mPerIn^3 KgPerM3 = 1 PoundMassPerInch3 "lbm/in^3"

@testitem "USCustomary density" begin
  @test isapprox(PoundMassPerInch3(1), PoundMassPerFoot3(1728), rtol=1e-12)
  @test isapprox(PoundMassPerFoot3(62.428), GramPerCentiMeter3(1), atol=1e-3) # water
  @test isapprox(PoundMass(2)/Foot3(1), PoundMassPerFoot3(2), rtol=1e-12)
end
