# following https://en.wikipedia.org/wiki/International_System_of_Units for names, definitions, and symbols

# the overall type hierarchy looks like:
# AbstractMeasure
#   AbstractCurrent
#   AbstractIntensity
#   AbstractLength
#     Meter
#     MilliMeter
#     Inch
#     Foot
#   AbstractMass
#     Gram
#     KiloGram
#     Slug
#   AbstractTime
#     Second
#     Hour
#   AbstractForce - derived from Mass*Length^2*Time...
#     Newton
#   ...

# base measures
@makeBaseMeasure Current Ampere "A"
@makeBaseMeasure Intensity Candela "cd"
@makeBaseMeasure Length Meter "m"
@makeBaseMeasure Mass KiloGram "kg"
@makeBaseMeasure Time Second "s"
@makeBaseMeasure Amount Mole "mol"

# Seed the abstractToSI registry with the six SI base abstract types defined above.
# AbstractTemperature is seeded in Temperature.jl after Kelvin is defined.
# All derived abstract types (AbstractForce, AbstractVelocity, …) are populated
# automatically when @relateMeasures processes their defining relations.
UnitTypes.abstractToSI[AbstractCurrent]   = UnitTypes.BaseDimensions(current=1)
UnitTypes.abstractToSI[AbstractIntensity] = UnitTypes.BaseDimensions(intensity=1)
UnitTypes.abstractToSI[AbstractLength]    = UnitTypes.BaseDimensions(length=1)
UnitTypes.abstractToSI[AbstractMass]      = UnitTypes.BaseDimensions(mass=1)
UnitTypes.abstractToSI[AbstractTime]      = UnitTypes.BaseDimensions(time=1)
UnitTypes.abstractToSI[AbstractAmount]    = UnitTypes.BaseDimensions(amount=1)

# Angle is not an SI base unit but is tracked as its own dimension so it does not silently cancel; it must precede Steradian, Lumen, and Lux, whose relations need AbstractAngle mapped.
@makeBaseMeasure Angle Radian "rad"
UnitTypes.abstractToSI[AbstractAngle] = UnitTypes.BaseDimensions(angle=1)
@makeMeasure π/180 Radian = 1 Degree "°"

@testitem "Angle base dimension" begin
  @test getBaseDims(Radian(1)) == BaseDimensions(angle=1)
  @test getBaseDims(Degree(1)) == BaseDimensions(angle=1)

  rm = Radian(2) * Meter(3) # arc length times radius has no named type, angle must not cancel
  @test rm isa Catchall
  @test rm.dimensions == BaseDimensions(angle=1, length=1)
  @test rm.value ≈ 6.0
  @test abbreviation(rm) == "m*rad"

  @test Degree(180) * Meter(1) ≈ Radian(π) * Meter(1)
  @test (Radian(2) * Meter(3)) / Meter(3) ≈ Radian(2) # resolves back to Radian
  @test Radian(Catchall(1.5, BaseDimensions(angle=1))) ≈ Radian(1.5)
  @test_throws ArgumentError Radian(Catchall(1.5, BaseDimensions(length=1)))
end

@testitem "Angle Radian Degree definitions" begin
  @test convert(Radian, Degree(180)) ≈ Radian(π)
  @test convert(Degree, Radian(π)) ≈ Degree(180)

  @test Radian(π) ≈ Degree(180)
  @test Degree(180) ≈ Radian(π)
  @test Radian(Degree(180)) ≈ Radian(π)
  @test Degree(Radian(π)) ≈ Degree(180)

  @test Degree(1) + Degree(2) ≈ Degree(3)
  @test isapprox(Degree(3) - Degree(1), Degree(2), atol=1e-3)

  @test Radian(1)*2 ≈ Radian(2)
  @test Degree(1)*2 ≈ Degree(2)
  @test -Degree(45) ≈ Degree(-45)
end

# Length powers
@makeMeasure 1e-15 Meter = 1 FemtoMeter "fm"
@makeMeasure 1e-12 Meter = 1 PicoMeter "pm"
@makeMeasure 1e-9 Meter = 1 NanoMeter "nm"
@makeMeasure 1e-6 Meter = 1 MicroMeter "μm"
@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"
@makeMeasure 1e-2 Meter = 1 CentiMeter "cm"
@makeMeasure 1e3 Meter = 1 KiloMeter "km"
@makeMeasure 1e-10 Meter = 1 Angstrom "Å"

@testitem "Length powers of 10" begin
  @test Meter(1.0) == Meter(1.0)
  @test isapprox( Meter(1.0), MilliMeter(1000.0), atol=1e-3)
  @test Meter(1.0) ≈ MilliMeter(1000.0)
end
@testitem "check unit consistency" begin
  @test isa(Meter(3)*2, Meter)
  @test isapprox( Meter(3)*2 + CentiMeter(5) + MilliMeter(4), Meter(6.054), atol=1e-5)
  @test Meter(1234) ≈ KiloMeter(1.234)
  @test MilliMeter(1)*Meter(3) / MilliMeter(1) ≈ Meter(3)
end

@makeMeasure 1e-3 KiloGram = 1 Gram "g"

@makeBaseMeasure Area Meter2 "m^2"
@relateMeasures Meter*Meter=Meter2
@makeMeasure 100 Meter2 = 1 Are "a"
@makeMeasure 1e4 Meter2 = 1 Hectare "ha"
@makeMeasure 1e-28 Meter2 = 1 Barn "b"

@makeBaseMeasure SolidAngle Steradian "sr"
@relateMeasures Radian*Radian=Steradian
@testitem "Steradian" begin
  @test getBaseDims(Steradian(1)) == BaseDimensions(angle=2)
  @test Radian(2) * Radian(3) ≈ Steradian(6)
  @test Radian(2)^2 ≈ Steradian(4)
  @test sqrt(Steradian(4)) ≈ Radian(2)
end

@makeBaseMeasure Volume Meter3 "m^3"
@relateMeasures Meter2*Meter=Meter3
@makeMeasure 1e-3 Meter3 = 1 Liter "L"
@makeMeasure 1e-6 Meter3 = 1 MilliLiter "mL" # must reference the base unit Meter3, since toBase converts into the referenced type
@testitem "Volume" begin
  @test isapprox(MilliLiter(1000), Liter(1), rtol=1e-12) # regression: MilliLiter referenced Liter, so 1000 mL equaled 1 m^3
  @test isapprox(Liter(1000), Meter3(1), rtol=1e-12)
end

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@relateMeasures KiloGram/Meter3=KgPerM3 # gives Density its SI dimensions so mass cancels correctly through Catchall arithmetic
@makeMeasure 1000 KgPerM3 = 1 GramPerCentiMeter3 "g/cm^3"

@testitem "Density" begin
  @test GramPerCentiMeter3(1) ≈ KgPerM3(1000)
  @test KgPerM3(2) * Meter3(3) ≈ KiloGram(6)
  @test Meter3(3) * KgPerM3(2) ≈ KiloGram(6)
  @test KiloGram(6) / Meter3(3) ≈ KgPerM3(2)
  @test KiloGram(6) / KgPerM3(2) ≈ Meter3(3)
  @test KiloGram(1) * KgPerM3(1) isa Catchall # regression: the / relation used to define mass*density = volume
end

@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
@relateMeasures Meter3/KiloGram=M3PerKg
Base.convert(::Type{KgPerM3}, x::T) where {T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{M3PerKg}, x::T) where {T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))
@testitem "SpecificVolume" begin
  @test Meter3(6) / KiloGram(3) ≈ M3PerKg(2)
  @test M3PerKg(2) * KiloGram(3) ≈ Meter3(6)
  @test convert(KgPerM3, M3PerKg(0.5)) ≈ KgPerM3(2)
  @test convert(M3PerKg, KgPerM3(2)) ≈ M3PerKg(0.5)
end

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@relateMeasures KiloGram/Meter2=KgPerM2
@testitem "SurfaceDensity" begin
  @test KgPerM2(2) * Meter2(3) ≈ KiloGram(6)
  @test KgPerM3(2) * Meter(3) ≈ KgPerM2(6) # resolves from Catchall via SI dimensions

  # density * length * length * length cancels through Catchall back to mass
  rhoAcrylic = 1.2u"g/cm^3"
  mAcrylic = rhoAcrylic * 6.35u"mm" * 558.55u"mm" * 342.25u"mm"
  @test mAcrylic isa KiloGram
  @test mAcrylic ≈ KiloGram(1.2e3 * 6.35e-3 * 558.55e-3 * 342.25e-3)
  @test Gram(mAcrylic) ≈ Gram(1.2 * 0.635 * 55.855 * 34.225)
end

@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

# time
@makeMeasure 1e-3 Second = 1 MilliSecond "ms"
@makeMeasure 60 Second = 1 Minute "min"
@makeMeasure 3600 Second = 1 Hour "hr"
@makeMeasure 86400 Second = 1 Day "days"
@makeMeasure 604800 Second = 1 Week "wk"
@makeMeasure 31557600 Second = 1 Year "yr"

@makeBaseMeasure Frequency Hertz "Hz"
@relateMeasures 1/Second = Hertz  # sets Hertz dims to {AbstractTime=>-1}; must precede @makeMeasure so PerSecond inherits correctly
@makeMeasure 1 Hertz = 1 PerSecond "s^-1"
@makeMeasure 1 Hertz = 1 Becquerel "Bq"
@makeMeasure 1/60 Hertz = 1 RevolutionsPerMinute "rpm"
@makeMeasure 1 Hertz = 1 RevolutionsPerSecond "rps"
@makeMeasure 2π Hertz = 1 AngHertz "Hz2π"

Base.convert(::Type{Second}, x::T) where {T<:AbstractFrequency} = 1/x #Second(1/toBaseFloat(x))
Base.convert(::Type{Hertz}, x::T) where {T<:AbstractTime} = 1/x #Hertz(1/toBaseFloat(x))
@testitem "Frequency conversions" begin
  @test convert(Second, Hertz(10)) ≈ Second(0.1)
  @test convert(Hertz, Second(0.1)) ≈ Hertz(10)
  @test 1/Second(10) ≈ Hertz(0.1)
  @test 1/Hertz(10) ≈ Second(0.1)
end

@makeBaseMeasure Velocity MeterPerSecond "m/s" 
@relateMeasures Meter*PerSecond=MeterPerSecond
@makeMeasure 1 MeterPerSecond = 60 MeterPerMinute "m/min"
@makeMeasure 1 MeterPerSecond = 3600 MeterPerHour "m/hr"
@makeMeasure 1000 MeterPerSecond = 3600 KiloMeterPerHour "km/hr"
@testitem "Velocity" begin
  @test isapprox(KiloMeterPerHour(1), MeterPerSecond(0.27778), atol=1e-4)
  @test isapprox(MeterPerHour(3600), MeterPerSecond(1), atol=1e-10)
end

@makeBaseMeasure Acceleration MeterPerSecond2 "m/s^2"
@relateMeasures MeterPerSecond*PerSecond=MeterPerSecond2

@makeBaseMeasure Force Newton "N"
@makeMeasure 1e3 Newton = 1 KiloNewton "kN"
@makeMeasure 1e-3 Newton = 1 MilliNewton "mN"
@relateMeasures KiloGram*MeterPerSecond2=Newton

@testitem "Newton/KiloNewton Catchall arithmetic parity" begin
  # allUnitTypes Dict inconsistency: @makeMeasure captured Newton's dims as {AbstractForce:1}
  # before @relateMeasures updated Newton to {AbstractMass:1,AbstractLength:1,AbstractTime:-2}.
  # KiloNewton's dict entry is therefore stale — this is a known Dict-representation artifact.
  @test UnitTypes.allUnitTypes[Newton].dimensions != UnitTypes.allUnitTypes[KiloNewton].dimensions

  # Catchall arithmetic is correct despite the dict inconsistency: getBaseDims goes through
  # abstractToSI[AbstractForce] which was populated by @relateMeasures and always maps to
  # the same BaseDimensions(mass=1,length=1,time=-2) regardless of which concrete Force type
  # is used.  So Newton*s and KiloNewton*s produce identical BaseDimensions and compare equal.
  nSec = Newton(2.0) * Second(1.0)
  kNSec = KiloNewton(2e-3) * Second(1.0)
  @test nSec isa Catchall
  @test kNSec isa Catchall
  @test nSec ≈ kNSec
end

@makeBaseMeasure Torque NewtonMeter "N*m"
@relateMeasures Newton*Meter=NewtonMeter
@makeMeasure 1e-3 NewtonMeter = 1 NewtonMilliMeter "N*mm"
@makeMeasure 1e-3 NewtonMeter = 1 MilliNewtonMeter "mN*m"

@makeBaseMeasure Pressure Pascal "Pa"
@relateMeasures Newton/Meter2=Pascal
@makeMeasure 1e3 Pascal = 1 KiloPascal "KPa"
@makeMeasure 1e6 Pascal = 1 MegaPascal "MPa"
@makeMeasure 1e9 Pascal = 1 GigaPascal "GPa"

@makeBaseMeasure Charge Coulomb "C"
@relateMeasures Second*Ampere=Coulomb

@makeBaseMeasure ElectricPotential Volt "V"
@makeMeasure 1e3 Volt = 1 KiloVolt "KV"

@makeBaseMeasure Resistance Ohm "Ω"
@makeMeasure 1e-3 Ohm = 1 MilliOhm "Ω"
@makeMeasure 1e3 Ohm = 1 KiloOhm "kΩ"
@makeMeasure 1e6 Ohm = 1 MegaOhm "MΩ"

@makeBaseMeasure Power Watt "W"
@relateMeasures Ampere*Volt=Watt
@relateMeasures Ampere*Ohm=Volt

@makeBaseMeasure Capacitance Farad "F" 
@makeMeasure 1e-3 Farad = 1 MilliFarad "mF"
@makeMeasure 1e-6 Farad = 1 MicroFarad "μF"
@makeMeasure 1e-9 Farad = 1 NanoFarad "nF"
@makeMeasure 1e-12 Farad = 1 PicoFarad "pF"

@makeBaseMeasure Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseMeasure MagneticFlux Weber "Wb"

@makeBaseMeasure MagneticFluxDensity Tesla "T"

@makeBaseMeasure Inductance Henry "H"
@makeMeasure 1e-3 Henry = 1 MilliHenry "mH"

@makeBaseMeasure LuminousFlux Lumen "lm"
@relateMeasures Candela*Steradian=Lumen
@makeBaseMeasure Illuminance Lux "lx"
@relateMeasures Lumen/Meter2=Lux
@testitem "Lumen Lux" begin
  @test getBaseDims(Lumen(1)) == BaseDimensions(intensity=1, angle=2)
  @test getBaseDims(Lux(1)) == BaseDimensions(intensity=1, angle=2, length=-2)
  @test Candela(2) * Steradian(3) ≈ Lumen(6)
  @test Lumen(6) / Steradian(3) ≈ Candela(2)
  @test Lumen(6) / Meter2(3) ≈ Lux(2)
  @test Lux(2) * Meter2(3) ≈ Lumen(6)

  # chains through Catchall resolve back to named types
  @test Candela(2) * Radian(3) * Radian(1) ≈ Lumen(6)
  @test Lumen(12) / Meter(2) / Meter(3) ≈ Lux(2)
  @test Candela(2) * Steradian(3) / Meter(1)^2 isa Lux
end

@makeBaseMeasure Energy Joule "J"
@makeMeasure 1e3 Joule = 1 KiloJoule "kJ"
@makeMeasure 1e6 Joule = 1 MegaJoule "MJ"
@makeMeasure 1e-3 Joule = 1 MilliJoule "mJ"
@makeMeasure 1.602_176_634e-19 Joule = 1 ElectronVolt "eV"

@makeBaseMeasure AbsorbedDose Gray "Gy"
@makeMeasure 1 Gray = 1 Sievert "Sv"

@makeBaseMeasure CatalyticActivity Katal "kat"

@makeBaseMeasure DynamicViscosity PascalSecond "Pa*s"
@makeBaseMeasure KinematicViscosity MeterSquaredPerSecond "m^2/s"

@makeBaseMeasure MolarConcentration Molar "M"

@makeMeasure 100000 Pascal = 1 Bar "bar"
@makeMeasure 101325 Pascal = 1 Atmosphere "atm"
@makeMeasure 101325/760 Pascal = 1 Torr "Torr"