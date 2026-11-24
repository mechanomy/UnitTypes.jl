# Test fixture for @registerMeasures: a precompiled package that defines measures, loaded by the "registerMeasures survives a package precompile" testitem in src/Registry.jl.
module RegisterFixture
using UnitTypes

@makeBaseMeasure HyperVolumeF Meter5F "m^5"
@relateMeasures Meter4*Meter=Meter5F # records allUnitTypes, abstractToSI and powerTypes (AbstractLength, 5) entries
@relateMeasures Meter2*Meter3=Meter5F
@makeMeasure 1e-15 Meter5F = 1 MilliMeter5F "mm^5"

function __init__()
  @registerMeasures
end

end
