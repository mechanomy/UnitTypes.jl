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
@makeMeasure 1e-15 Meter = 1 FemtoMeter "fm"
@makeMeasure 1e-12 Meter = 1 PicoMeter "pm"
@makeMeasure 1e-9 Meter = 1 NanoMeter "nm"
@makeMeasure 1e-6 Meter = 1 MicroMeter "μm"
@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"
@makeMeasure 1e-2 Meter = 1 CentiMeter "cm"
@makeMeasure 1e3 Meter = 1 KiloMeter "km"

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
@makeMeasure 1e3 Meter3 = 1 Liter "L"
@makeMeasure 1e3 Liter = 1 MilliLiter "mL"

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{KgPerM3}, x::T) where {T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{M3PerKg}, x::T) where {T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

# time
@makeMeasure 1e-3 Second = 1 MilliSecond "ms"
@makeMeasure 1/60 Second = 1 Minute "min"
@makeMeasure 1/3600 Second = 1 Hour "hr"
@makeMeasure 1/24 Hour = 1 Day "days"

@makeBaseMeasure Frequency Hertz "Hz"
@makeMeasure 1 Hertz = 1 PerSecond "s^-1"

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
@makeMeasure 1e3 Newton = 1 KiloNewton "kN"
@makeMeasure 1e-3 Newton = 1 MilliNewton "mN"
@relateMeasures KiloGram*MeterPerSecond2=Newton

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
@makeBaseMeasure Illuminance Lux "lx"
#these are waiting for a request and mwe for unit test
# @makeBaseMeasure DecayRate Becquerel "Bq"
# @makeBaseMeasure AbsorbedDose Gray "Gy"
# @makeBaseMeasure EquivalentDose Sievert "Sv"

# others..?:
@makeBaseMeasure Percentage Percent "%" # not a physical unit...
@makeMeasure 1e-2 Percent = 1 BasisPoints "bps" # 100bps in 1%