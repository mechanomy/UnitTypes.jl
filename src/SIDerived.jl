# following https://en.wikipedia.org/wiki/International_System_of_Units for names, definitions, and symbols

# Length powers
@deriveMeasure Meter(1) = Femtometer(1e15) "fm"
@deriveMeasure Meter(1) = PicoMeter(1e12) "pm"
@deriveMeasure Meter(1) = NanoMeter(1e9) "nm"
@deriveMeasure Meter(1) = MicroMeter(1e6) "μm"
@deriveMeasure Meter(1) = MilliMeter(1e3) "mm"
@deriveMeasure Meter(1) = CentiMeter(1e2) "cm"
# @deriveMeasure Meter(1000) = KiloMeter(1) "km"
@deriveMeasure_new Meter(1) = Kilometer(1e-3) "km"

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
@deriveMeasure Meter3(1) = Liter(1e3) "L"
@deriveMeasure Liter(1) = MilliLiter(1e3) "mL"

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{KgPerM3}, x::T) where {T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{M3PerKg}, x::T) where {T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

# time
@deriveMeasure Second(60) = Minute(1) "min"
@deriveMeasure Minute(60) = Hour(1) "hr"

@makeBaseMeasure Frequency Hertz "Hz"
@deriveMeasure Hertz(1) = PerSecond(1) "s^-1"

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

@makeBaseMeasure Velocity MeterPerSecond "m/s" #cumbersome...
@relateMeasures Meter*PerSecond=MeterPerSecond
# Base.:/(x::T,y::U) where {T<:AbstractLength, U<:AbstractTime} = Meter(x) * PerSecond(1/toBaseFloat(y)) 
# Base.:/(x::T,y::U) where {T<:AbstractMeasure, U<:AbstractTime} = x * PerSecond(1/toBaseFloat(y)) 
# @testitem "hand holding PerSecond" begin
  # @test Meter(1) / Second(1) ≈ MeterPerSecond(1)
  # @test MilliMeter(1) / Minute(1) ≈ MeterPerSecond(1/1000 / 60/1)
  # @test Joule(1) / Second(1) ≈ Ampere(1)
  # @test 1u"m" / 1u"s" ≈ 1u"m/s"
  # @test m(1) / s(1) ≈ MeterPerSecond(1) # m not defined
  # @test UnitTypes.Measure.m(1) / UnitTypes.Measure.s(1) ≈ MeterPerSecond(1) # m not defined
# end



@makeBaseMeasure Acceleration MeterPerSecond2 "m/s^2"
@relateMeasures MeterPerSecond*PerSecond=MeterPerSecond2

@makeBaseMeasure Force Newton "N"
@deriveMeasure Newton(1e3) = KiloNewton(1) "kN"
@deriveMeasure Newton(1) = MilliNewton(1e3) "mN"
@relateMeasures KiloGram*MeterPerSecond2=Newton

@makeBaseMeasure Torque NewtonMeter "N*m"
@relateMeasures Newton*Meter=NewtonMeter
@deriveMeasure NewtonMeter(1) = NewtonMilliMeter(1e3) "N*mm"
@deriveMeasure NewtonMeter(1) = MilliNewtonMeter(1e3) "mN*m"

@makeBaseMeasure Pressure Pascal "Pa"
@relateMeasures Newton*Meter2=Pascal
@deriveMeasure Pascal(1e3) = KiloPascal(1e3) "KPa"
@deriveMeasure Pascal(1e6) = MegaPascal(1e3) "MPa"
@deriveMeasure Pascal(1e9) = GigaPascal(1e3) "GPa"

@makeBaseMeasure Charge Coulomb "C"
@relateMeasures Second*Ampere=Coulomb

@makeBaseMeasure ElectricPotential Volt "V"
@deriveMeasure Volt(1000) = KiloVolt(1) "KV"

@makeBaseMeasure Resistance Ohm "Ω"
@deriveMeasure Ohm(1) = MilliOhm(1000) "Ω"
@deriveMeasure Ohm(1000) = KiloOhm(1) "kΩ"
@deriveMeasure Ohm(1e6) = MegaOhm(1) "MΩ"

@makeBaseMeasure Power Watt "W"
@relateMeasures Ampere*Volt=Watt
@relateMeasures Ampere*Ohm=Volt

@makeBaseMeasure Capacitance Farad "F" 
@deriveMeasure Farad(1) = MilliFarad(1e3) "mF"
@deriveMeasure Farad(1) = MicroFarad(1e6) "μF"
@deriveMeasure Farad(1) = NanoFarad(1e9) "nF"
@deriveMeasure Farad(1) = PicoFarad(1e12) "pF"

@makeBaseMeasure Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseMeasure MagneticFlux Weber "Wb"

@makeBaseMeasure MagneticFluxDensity Tesla "T"

@makeBaseMeasure Inductance Henry "H"
@deriveMeasure Henry(1) = MilliHenry(1e3) "mH"

@makeBaseMeasure LuminousFlux Lumen "lm"
@makeBaseMeasure Illuminance Lux "lx"
# @makeBaseMeasure DecayRate Becquerel "Bq"
# @makeBaseMeasure AbsorbedDose Gray "Gy"
# @makeBaseMeasure EquivalentDose Sievert "Sv"
