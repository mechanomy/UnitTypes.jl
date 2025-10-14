
using BenchmarkTools
include("../src/UnitTypes.jl")
# import UnitTypes
import Unitful


# test time some common operations

# multiplication
fun(m) = m*m
n = 100
mNumber = collect(rand(n,n))

aUnitType = UnitTypes.Meter(1.1)
aUnitful = 1.2Unitful.@u_str("m")

mUnitTypes = fill(aUnitType, (n,n))
mUnitful = fill( aUnitful, (n,n))

println("\nrandom floats:")
resNumber = @benchmark fun(mNumber)
display(resNumber)

println("\nUnitTypes:")
resUnitTypes = @benchmark fun(mUnitTypes)
display(resUnitTypes)

println("\nUnitful:")
resUnitful = @benchmark fun(mUnitful)
display(resUnitful)



