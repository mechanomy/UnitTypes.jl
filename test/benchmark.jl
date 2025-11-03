
using BenchmarkTools
import Unitful
include("../src/UnitTypes.jl")

println("Julia $(VERSION)")
println("Unitful version: $(pkgversion(Unitful))")
println("UnitTypes version: $(pkgversion(UnitTypes))")

fun(m) = (m + 3*m - m/4)*m
resFloats = @timed fun(1.1)
resUnitTypes = @timed fun(UnitTypes.Meter(1.1))
resUnitful = @timed fun(2.1Unitful.@u_str("m"))

println("\nfloats: $(resFloats.time), $(resFloats.bytes)")
display(@benchmark fun(1.1))
println("\nUnitful: $(resUnitful.time), $(resFloats.bytes)")
display(@benchmark fun(1.1Unitful.@u_str("m")))
println("\nUnitTypes: $(resUnitTypes.time), $(resFloats.bytes)")
display(@benchmark fun(UnitTypes.Meter(1.1)))
