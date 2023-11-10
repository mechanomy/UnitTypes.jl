
module UnitTypesDev

using UnitTypes
using InteractiveUtils




# @derivedUnit Torque "Nm" Force * Length
function makeDerived(name, unit, numerator=[1], denominator=[1] )
  @show name
  @show unit
  @show numerator
  @show denominator
end

# makeDerived(:Power, "W", [Kilogram, Meter, Meter], [Second, Second, Second] )
# makeDerived(:Voltage, "V", [Kilogram, Meter, Meter], [Second, Second, Second, Ampere] )

end ;