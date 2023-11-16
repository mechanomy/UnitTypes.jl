# following https://en.wikipedia.org/wiki/International_System_of_Units for names, definitions, and symbols

# Length powers
@makeDerivedMeasure Femtometer "fm" 10^-15 Meter
@makeDerivedMeasure Picometer "pm" 10^-12 Meter
@makeDerivedMeasure Nanometer "nm" 10^-9 Meter
@makeDerivedMeasure Micrometer "μm" 10^-6 Meter
@makeDerivedMeasure Millimeter "mm" 10^-3 Meter
@makeDerivedMeasure Centimeter "cm" 0.01 Meter
@makeDerivedMeasure Decimeter "dm" 0.01 Meter
@makeDerivedMeasure Kilometer "km" 1e3 Meter
@makeDerivedMeasure Megameter "Mm" 1e3 Meter

@testitem "Length powers of 10" begin
  @test Meter(1.0) == Meter(1.0)
  @test Meter(1.0) ≈ Millimeter(1000.0)
end
@testitem "check unit consistency" begin
  @test Meter(3)*2 + Centimeter(5) ≈ Meter(6.05)
  @test Millimeter(1)*Meter(3) / Millimeter(1) ≈ Meter(3)
end

@makeBaseMeasure Area Meter2 "m^2"
@addUnitOperations Meter Meter Meter2

@makeBaseMeasure Volume Meter3 "m^3"
@addUnitOperations Meter2 Meter Meter3
@makeDerivedMeasure Liter "L" 1e-3 Meter3
@makeDerivedMeasure Milliliter "mL" 1e-3 Liter

@makeBaseMeasure Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseMeasure SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{U}, x::T) where {U<:AbstractDensity, T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractSpecificVolume, T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseMeasure SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseMeasure CurrentDensity APerM2 "A/m^2"
@makeBaseMeasure MagneticFieldStrength APerM "A/m"

@makeBaseMeasure Frequency Hertz "Hz"
@makeDerivedMeasure PerSecond "s^-1" 1 Hertz

Base.convert(::Type{U}, x::T) where {U<:AbstractTime, T<:AbstractFrequency} = Second(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractFrequency, T<:AbstractTime} = Hertz(1/toBaseFloat(x))
@testitem "Frequency" begin
  @test Hertz(10) ≈ Second(0.1)
  @test Second(10) ≈ Hertz(0.1)
end

@makeBaseMeasure Velocity MeterPerSecond "m/s"
@addUnitOperations Meter PerSecond MeterPerSecond # this is ugly...

@makeBaseMeasure Acceleration MeterPerSecond2 "m/s^2"
@addUnitOperations MeterPerSecond PerSecond MeterPerSecond2 # also ugly

@makeBaseMeasure Force Newton "N"
@addUnitOperations Kilogram MeterPerSecond2 Newton

@makeBaseMeasure Torque NewtonMeter "N*m"
@addUnitOperations Newton Meter NewtonMeter
# NewtonMeter(uf::T) where T<:Unitful.AbstractQuantity = convert(NewtonMeter, uf) #because it is more specific this should take precedence over NewtonMeter(::Number(Unitful))

@makeBaseMeasure Pressure Pascal "Pa"
@addUnitOperations Newton Meter2 Pascal

@makeBaseMeasure Charge Coulomb "C"
@addUnitOperations Second Ampere Coulomb

@makeBaseMeasure ElectricPotential Volt "V"
# @addUnitOperations I * R = V
# @addUnitOperations P / I = V
# @addUnitOperations kg*m^2/s^3/A -- I just need to write a function to parse unit symbols into con/destructive operations
@makeDerivedMeasure Kilovolt "kV" 1e3 Volt

@makeBaseMeasure Resistance Ohm "Ω"
@makeDerivedMeasure Milliohm "mΩ" 1e-3 Ohm
@makeDerivedMeasure Kiloohm "kΩ" 1e3 Ohm
@makeDerivedMeasure Megaohm "MΩ" 1e6 Ohm

@makeBaseMeasure Capacitance Farad "F" 
@makeDerivedMeasure Millifarad "mF" 1e-3 Farad
@makeDerivedMeasure Microfarad "μF" 1e-6 Farad
@makeDerivedMeasure Nanofarad "nF" 1e-9 Farad
@makeDerivedMeasure Picofarad "pF" 1e-12 Farad

@makeBaseMeasure Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseMeasure MagneticFlux Weber "Wb"

@makeBaseMeasure MagneticFluxDensity Tesla "T"

@makeBaseMeasure Inductance Henry "H"
@makeDerivedMeasure Millihenry "mH" 1e-3 Henry

@makeBaseMeasure LuminousFlux Lumen "lm"
@makeBaseMeasure Illuminance Lux "lx"
# @makeBaseMeasure DecayRate Becquerel "Bq"
# @makeBaseMeasure AbsorbedDose Gray "Gy"
# @makeBaseMeasure EquivalentDose Sievert "Sv"
