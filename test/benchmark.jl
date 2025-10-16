
using BenchmarkTools
include("../src/UnitTypes.jl")
# import UnitTypes
import Unitful


# test time some common operations

# multiplication
n = 100
mNumber = collect(rand(n,n))

aUnitType = UnitTypes.Meter(1.1)
aUnitful = 1.2Unitful.@u_str("m")

mUnitTypes = fill(aUnitType, (n,n))
mUnitful = fill( aUnitful, (n,n))

# +
# println("\nrandom floats:")
# resNumber = @benchmark $mNumber + $mNumber
# display(resNumber)

# println("\nUnitTypes:")
# resUnitTypes = @benchmark $mUnitTypes + $mUnitTypes
# display(resUnitTypes)

# println("\nUnitful:")
# resUnitful = @benchmark $mUnitful + $mUnitful
# display(resUnitful)



# *
fun(m) = m*m
println("\nrandom floats:")
resNumber = @benchmark fun($mNumber)
display(resNumber)

println("\nUnitTypes:")
resUnitTypes = @benchmark fun($mUnitTypes)
display(resUnitTypes)

println("\nUnitful:")
resUnitful = @benchmark fun($mUnitful)
display(resUnitful)


# n = 100
# function fun(m)
#   for i in range(1,n)
#     m += typeof(m)(0.1)
#   end
#   return m
# end

# println("unitTypes:")
# resUnitTypes = @benchmark fun(UnitTypes.Meter(1.1))
# display(resUnitTypes)

# println("unitful:")
# resUnitful = @benchmark fun(2.1Unitful.@u_str("m"))
# display(resUnitful)