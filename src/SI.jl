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

# Length powers
@makeMeasure Meter = FemtoMeter "fm" 1e-15
@makeMeasure Meter = PicoMeter "pm" 1e-12
@makeMeasure Meter = NanoMeter "nm" 1e-9
@makeMeasure Meter = MicroMeter "μm" 1e-6 
@makeMeasure Meter = MilliMeter "mm" 1e-3
@makeMeasure Meter = CentiMeter "cm" 1e-2 
@makeMeasure Meter = KiloMeter "km" 1e3

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

@makeBaseMeasure Area Meter2 "m^2"
@relateMeasures Meter*Meter=Meter2

@makeBaseMeasure Volume Meter3 "m^3"
@relateMeasures Meter2*Meter=Meter3
@makeMeasure Meter3 = Liter "L" 1e3
@makeMeasure Liter = MilliLiter "mL" 1e3

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{KgPerM3}, x::T) where {T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{M3PerKg}, x::T) where {T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

# time
@makeMeasure Second = MilliSecond "ms" 1e-3
@makeMeasure Second = Minute "min" 1/60
@makeMeasure Second = Hour "hr" 1/3600
@makeMeasure Hour = Day "days" 1/24

@makeBaseMeasure Frequency Hertz "Hz"
@makeMeasure Hertz = PerSecond "s^-1" 1

# can relateMeasures be expanded to provide? @relateMeasures 1/Second = Hertz?
Base.convert(::Type{Second}, x::T) where {T<:AbstractFrequency} = 1/x #Second(1/toBaseFloat(x))
Base.convert(::Type{Hertz}, x::T) where {T<:AbstractTime} = 1/x #Hertz(1/toBaseFloat(x))
Base.:/(x::T,y::U) where {T<:Number, U<:AbstractTime} = Hertz(x/toBaseFloat(y)) #1/time
Base.:/(x::T,y::U) where {T<:Number, U<:AbstractFrequency} = Second(x/toBaseFloat(y)) #1/time
@testitem "Frequency conversions" begin
  @test convert(Second, Hertz(10)) ≈ Second(0.1)
  @test convert(Hertz, Second(0.1)) ≈ Hertz(10)
  @test 1/Second(10) ≈ Hertz(0.1)
  @test 1/Hertz(10) ≈ Second(0.1)
end

@makeBaseMeasure Velocity MeterPerSecond "m/s" 
@relateMeasures Meter*PerSecond=MeterPerSecond

@makeBaseMeasure Acceleration MeterPerSecond2 "m/s^2"
@relateMeasures MeterPerSecond*PerSecond=MeterPerSecond2

@makeBaseMeasure Force Newton "N"
@makeMeasure Newton = KiloNewton "kN" 1e3
@makeMeasure Newton = MilliNewton "mN" 1e-3
@relateMeasures KiloGram*MeterPerSecond2=Newton

@makeBaseMeasure Torque NewtonMeter "N*m"
@relateMeasures Newton*Meter=NewtonMeter
@makeMeasure NewtonMeter = NewtonMilliMeter "N*mm" 1e-3
@makeMeasure NewtonMeter = MilliNewtonMeter "mN*m" 1e-3

@makeBaseMeasure Pressure Pascal "Pa"
@relateMeasures Newton/Meter2=Pascal
@makeMeasure Pascal = KiloPascal "KPa" 1e3
@makeMeasure Pascal = MegaPascal "MPa" 1e6
@makeMeasure Pascal = GigaPascal "GPa" 1e9

@makeBaseMeasure Charge Coulomb "C"
@relateMeasures Second*Ampere=Coulomb

@makeBaseMeasure ElectricPotential Volt "V"
@makeMeasure Volt = KiloVolt "KV" 1e3

@makeBaseMeasure Resistance Ohm "Ω"
@makeMeasure Ohm = MilliOhm "Ω" 1e-3
@makeMeasure Ohm = KiloOhm "kΩ" 1e3
@makeMeasure Ohm = MegaOhm "MΩ" 1e6

@makeBaseMeasure Power Watt "W"
@relateMeasures Ampere*Volt=Watt
@relateMeasures Ampere*Ohm=Volt

@makeBaseMeasure Capacitance Farad "F" 
@makeMeasure Farad = MilliFarad "mF" 1e-3
@makeMeasure Farad = MicroFarad "μF" 1e-6
@makeMeasure Farad = NanoFarad "nF" 1e-9
@makeMeasure Farad = PicoFarad "pF" 1e-12

@makeBaseMeasure Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseMeasure MagneticFlux Weber "Wb"

@makeBaseMeasure MagneticFluxDensity Tesla "T"

@makeBaseMeasure Inductance Henry "H"
@makeMeasure Henry = MilliHenry "mH" 1e-3

@makeBaseMeasure LuminousFlux Lumen "lm"
@makeBaseMeasure Illuminance Lux "lx"
#these are waiting for a request and mwe for unit test
# @makeBaseMeasure DecayRate Becquerel "Bq"
# @makeBaseMeasure AbsorbedDose Gray "Gy"
# @makeBaseMeasure EquivalentDose Sievert "Sv"

# others..?:
@makeBaseMeasure Percentage Percent "%" # not a physical unit...
@makeMeasure Percent = BasisPoints "bps" 1e-2 # 100bps in 1%