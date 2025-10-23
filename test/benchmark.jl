
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



# # *
# fun(m) = m*m
# println("\nrandom floats:")
# resNumber = @benchmark fun($mNumber)
# display(resNumber)

# println("\nUnitTypes:")
# resUnitTypes = @benchmark fun($mUnitTypes)
# display(resUnitTypes)

# println("\nUnitful:")
# resUnitful = @benchmark fun($mUnitful)
# display(resUnitful)


# fun(m) = m*m
# println("floats:")
# resFloats = @benchmark fun(1.1)
# display(resFloats)

# println("unitTypes:")
# resUnitTypes = @benchmark fun(UnitTypes.Meter(1.1))
# display(resUnitTypes)

# println("unitful:")
# resUnitful = @benchmark fun(2.1Unitful.@u_str("m"))
# display(resUnitful)


fun(m) = m*m
println("floats:")
resFloats = @timed fun(1.1)
display(resFloats)

println("unitTypes:")
resUnitTypes = @timed fun(UnitTypes.Meter(1.1))
display(resUnitTypes)

println("unitful:")
resUnitful = @timed fun(2.1Unitful.@u_str("m"))
display(resUnitful)

resUnitTypes = @timed UnitTypes.Meter(1.1)*UnitTypes.Meter(1.1) # bytes 620,282
display(resUnitTypes)


println("Meter(1.1)")
res = @timed UnitTypes.Meter(1.1) # 0
println("bytes: $(res.bytes)")

println("a+a")
a = UnitTypes.Meter(1.1)
res = @timed a+a # 872,330
println("bytes: $(res.bytes)")

Base.:+(x::UnitTypes.Meter, y::UnitTypes.Meter) = UnitTypes.Meter(x.value+convert(UnitTypes.Meter,y).value)
res = @timed a+a # 77936
println("bytes: $(res.bytes)")

Base.:+(x::UnitTypes.Meter, y::UnitTypes.Meter) = UnitTypes.Meter(x.value+y.value)
res = @timed a+a # 
println("bytes: $(res.bytes)")

Base.:+(x::UnitTypes.Meter, y::UnitTypes.Meter) = x.value 
# @show @which a+a
res = @timed a+a # 
println("bytes: $(res.bytes)")
