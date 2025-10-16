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
@makeMeasure Meter(1) = FemtoMeter(1e15) "fm"
@makeMeasure Meter(1) = PicoMeter(1e12) "pm"
@makeMeasure Meter(1) = NanoMeter(1e9) "nm"
@makeMeasure Meter(1) = MicroMeter(1e6) "μm"
@makeMeasure Meter(1) = MilliMeter(1e3) "mm"
@makeMeasure Meter(1) = CentiMeter(1e2) "cm"
@makeMeasure Meter(1e3) = KiloMeter(1) "km"


@testitem "Length powers of 10" begin
  @test Meter(1.0) == Meter(1.0)
  @test Meter(1.0) ≈ MilliMeter(1000.0)
end
@testitem "check unit consistency" begin
  @test isa(Meter(3)*2, Meter)
  @test Meter(3)*2 + CentiMeter(5) ≈ Meter(6.05)
  @test MilliMeter(1)*Meter(3) / MilliMeter(1) ≈ Meter(3)
end

@makeBaseMeasure Area Meter2 "m^2"
@relateMeasures Meter*Meter=Meter2

@makeBaseMeasure Volume Meter3 "m^3"
@relateMeasures Meter2*Meter=Meter3
@makeMeasure Meter3(1) = Liter(1e3) "L"
@makeMeasure Liter(1) = MilliLiter(1e3) "mL"

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{KgPerM3}, x::T) where {T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{M3PerKg}, x::T) where {T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

# time
@makeMeasure Second(1) = MilliSecond(1000) "ms"
@makeMeasure Second(60) = Minute(1) "min"
@makeMeasure Minute(60) = Hour(1) "hr"
@makeMeasure Hour(24) = Day(1) "days"

@makeBaseMeasure Frequency Hertz "Hz"
@makeMeasure Hertz(1) = PerSecond(1) "s^-1"

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
@makeMeasure Newton(1e3) = KiloNewton(1) "kN"
@makeMeasure Newton(1) = MilliNewton(1e3) "mN"
@relateMeasures KiloGram*MeterPerSecond2=Newton

@makeBaseMeasure Torque NewtonMeter "N*m"
@relateMeasures Newton*Meter=NewtonMeter
@makeMeasure NewtonMeter(1) = NewtonMilliMeter(1e3) "N*mm"
@makeMeasure NewtonMeter(1) = MilliNewtonMeter(1e3) "mN*m"

@makeBaseMeasure Pressure Pascal "Pa"
@relateMeasures Newton/Meter2=Pascal
@makeMeasure Pascal(1e3) = KiloPascal(1) "KPa"
@makeMeasure Pascal(1e6) = MegaPascal(1) "MPa"
@makeMeasure Pascal(1e9) = GigaPascal(1) "GPa"

@makeBaseMeasure Charge Coulomb "C"
@relateMeasures Second*Ampere=Coulomb

@makeBaseMeasure ElectricPotential Volt "V"
@makeMeasure Volt(1000) = KiloVolt(1) "KV"

@makeBaseMeasure Resistance Ohm "Ω"
@makeMeasure Ohm(1) = MilliOhm(1e3) "Ω"
@makeMeasure Ohm(1e3) = KiloOhm(1) "kΩ"
@makeMeasure Ohm(1e6) = MegaOhm(1) "MΩ"

@makeBaseMeasure Power Watt "W"
@relateMeasures Ampere*Volt=Watt
@relateMeasures Ampere*Ohm=Volt

@makeBaseMeasure Capacitance Farad "F" 
@makeMeasure Farad(1) = MilliFarad(1e3) "mF"
@makeMeasure Farad(1) = MicroFarad(1e6) "μF"
@makeMeasure Farad(1) = NanoFarad(1e9) "nF"
@makeMeasure Farad(1) = PicoFarad(1e12) "pF"

@makeBaseMeasure Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseMeasure MagneticFlux Weber "Wb"

@makeBaseMeasure MagneticFluxDensity Tesla "T"

@makeBaseMeasure Inductance Henry "H"
@makeMeasure Henry(1) = MilliHenry(1e3) "mH"

@makeBaseMeasure LuminousFlux Lumen "lm"
@makeBaseMeasure Illuminance Lux "lx"
#these are waiting for a request and mwe for unit test
# @makeBaseMeasure DecayRate Becquerel "Bq"
# @makeBaseMeasure AbsorbedDose Gray "Gy"
# @makeBaseMeasure EquivalentDose Sievert "Sv"

# others..:
@makeBaseMeasure Percentage Percent "%" # not a physical unit...
@makeMeasure Percent(1) = BasisPoints(100) "bps"