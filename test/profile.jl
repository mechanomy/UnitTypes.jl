# using ProfileView # https://github.com/timholy/ProfileView.jl
using ProfileCanvas # https://github.com/pfitzseb/ProfileCanvas.jl
include("../src/UnitTypes.jl")
# import UnitTypes
import Unitful

fun(m) = m*m
n = 10

aUnitType = UnitTypes.Meter(1.1)
aUnitful = 1.2Unitful.@u_str("m")
mUnitTypes = fill(aUnitType, (n,n))
mUnitful = fill( aUnitful, (n,n))

println("\nUnitTypes:")
# @profview fun(mUnitTypes)
# @profview_allocs fun(mUnitTypes)
fun(mUnitTypes)
# display(resUnitTypes)

# println("\nUnitful:")
# resUnitful = @profile fun(mUnitful)
# # display(resUnitful)
