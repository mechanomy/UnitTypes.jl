# following https://en.wikipedia.org/wiki/International_System_of_Units for names, definitions, and symbols

# Length powers
@makeMeasure Femtometer "fm" 10^-15 Meter
@makeMeasure Picometer "pm" 10^-12 Meter
@makeMeasure Nanometer "nm" 10^-9 Meter
@makeMeasure Micrometer "μm" 10^-6 Meter
@makeMeasure Millimeter "mm" 10^-3 Meter
@makeMeasure Centimeter "cm" 0.01 Meter
@makeMeasure Decimeter "dm" 0.01 Meter
@makeMeasure Kilometer "km" 1e3 Meter
@makeMeasure Megameter "Mm" 1e3 Meter

@testitem "Length powers of 10" begin
  @test Meter(1.0) == Meter(1.0)
  @test Meter(1.0) ≈ Millimeter(1000.0)
end
@testitem "check unit consistency" begin
  @test Meter(3)*2 + Centimeter(5) ≈ Meter(6.05)
  @test Millimeter(1)*Meter(3) / Millimeter(1) ≈ Meter(3)
end

@makeBaseUnit Area Meter2 "m^2"
@addUnitOperations Meter Meter Meter2

@makeBaseUnit Volume Meter3 "m^3"
@addUnitOperations Meter2 Meter Meter3
@makeMeasure Liter "L" 1e-3 Meter3
@makeMeasure Milliliter "mL" 1e-3 Liter

@makeBaseUnit Density KgPerM3 "kg/m^3" # this is making the case to add a default constructor Density(3) with assumed units kg/m3
@makeBaseUnit SpecificVolume M3PerKg "m^3/kg"
Base.convert(::Type{U}, x::T) where {U<:AbstractDensity, T<:AbstractSpecificVolume} = KgPerM3(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractSpecificVolume, T<:AbstractDensity} = M3PerKg(1/toBaseFloat(x))

@makeBaseUnit SurfaceDensity KgPerM2 "kg/m^2"
@makeBaseUnit CurrentDensity APerM2 "A/m^2"
@makeBaseUnit MagneticFieldStrength APerM "A/m"

@makeBaseUnit Frequency Hertz "Hz"
@makeMeasure PerSecond "1/s" 1 Hertz

Base.convert(::Type{U}, x::T) where {U<:AbstractTime, T<:AbstractFrequency} = Second(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractFrequency, T<:AbstractTime} = Hertz(1/toBaseFloat(x))
@testitem "Frequency" begin
  @test Hertz(10) ≈ Second(0.1)
  @test Second(10) ≈ Hertz(0.1)
end

@makeBaseUnit Velocity MeterPerSecond "m/s"
@addUnitOperations Meter PerSecond MeterPerSecond # this is ugly...

@makeBaseUnit Acceleration MeterPerSecond2 "m/s^2"
@addUnitOperations MeterPerSecond PerSecond MeterPerSecond2 # also ugly

@makeBaseUnit Force Newton "N"
@addUnitOperations Kilogram MeterPerSecond2 Newton

@makeBaseUnit Torque NewtonMeter "Nm"
@addUnitOperations Newton Meter NewtonMeter

@makeBaseUnit Pressure Pascal "Pa"
@addUnitOperations Newton Meter2 Pascal

@makeBaseUnit Charge Coulomb "C"
@addUnitOperations Second Ampere Coulomb

@makeBaseUnit ElectricPotential Volt "V"
# @addUnitOperations I * R = V
# @addUnitOperations P / I = V
# @addUnitOperations kg*m^2/s^3/A -- I just need to write a function to parse unit symbols into con/destructive operations
@makeMeasure Kilovolt "KV" 1e3 Volt

@makeBaseUnit Resistance Ohm "Ω"
@makeMeasure Milliohm "mΩ" 1e-3 Ohm
@makeMeasure Kiloohm "kΩ" 1e3 Ohm
@makeMeasure Megaohm "MΩ" 1e6 Ohm

@makeBaseUnit Capacitance Farad "F" 
@makeMeasure Millifarad "mF" 1e-3 Farad
@makeMeasure Microfarad "μF" 1e-6 Farad
@makeMeasure Nanofarad "nF" 1e-9 Farad
@makeMeasure Picofarad "pF" 1e-12 Farad

@makeBaseUnit Conductance Siemens "Ω^-1"
Base.convert(::Type{U}, x::T) where {U<:AbstractResistance, T<:AbstractConductance} = Ohm(1/toBaseFloat(x))
Base.convert(::Type{U}, x::T) where {U<:AbstractConductance, T<:AbstractResistance} = Siemens(1/toBaseFloat(x))

@makeBaseUnit MagneticFlux Weber "Wb"

@makeBaseUnit MagneticFluxDensity Tesla "T"

@makeBaseUnit Inductance Henry "H"
@makeMeasure Millihenry "mH" 1e-3 Henry


@makeBaseUnit LuminousFlux Lumen "lm"
@makeBaseUnit Illuminance Lux "lx"
@makeBaseUnit Becquerel Becquerel "Bq"
@makeBaseUnit Gray Gray "Gy"
@makeBaseUnit Sievert Sievert "Sv"
