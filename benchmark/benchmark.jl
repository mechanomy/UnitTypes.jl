using BenchmarkTools
import Unitful
include("../src/UnitTypes.jl")

println("Julia $(VERSION)")
println("Unitful version: $(pkgversion(Unitful))")
println("UnitTypes version: $(pkgversion(UnitTypes))")

# ── Arithmetic: (m + 3m - m/4)*m ────────────────────────────────────────────
fun(m) = (m + 3*m - m/4)*m
resFloats    = @timed fun(1.1)
resUnitTypes = @timed fun(UnitTypes.Meter(1.1))
resUnitful   = @timed fun(2.1Unitful.@u_str("m"))

println("\n=== arithmetic: (m + 3m - m/4)*m ===")
println("floats:    time=$(resFloats.time), bytes=$(resFloats.bytes)")
display(@benchmark fun(1.1))

println("\nUnitful:   time=$(resUnitful.time), bytes=$(resUnitful.bytes)")
display(@benchmark fun(1.1Unitful.@u_str("m")))

println("\nUnitTypes: time=$(resUnitTypes.time), bytes=$(resUnitTypes.bytes)")
display(@benchmark fun(UnitTypes.Meter(1.1)))

# ── Exponentiation: m^3 ──────────────────────────────────────────────────────
powFun(m) = m^3

println("\n=== exponentiation: m^3 ===")
println("floats:")
display(@benchmark powFun(2.0))

println("\nUnitful:")
display(@benchmark powFun(2.0Unitful.@u_str("m")))

println("\nUnitTypes:")
display(@benchmark powFun(UnitTypes.Meter(2.0)))

# ── Conversion: Inch → MilliMeter ────────────────────────────────────────────
# float baseline: manually apply the inch→mm factor
convFun_float(x)     = x * 25.4
convFun_unitful(x)   = Unitful.uconvert(Unitful.@u_str("mm"), x)
convFun_unittypes(x) = UnitTypes.MilliMeter(x)

println("\n=== conversion: Inch → MilliMeter ===")
println("floats:")
display(@benchmark convFun_float(1.0))

println("\nUnitful:")
display(@benchmark convFun_unitful(1.0Unitful.@u_str("inch")))

println("\nUnitTypes:")
display(@benchmark convFun_unittypes(UnitTypes.Inch(1.0)))

# ── Comparison: isapprox(1m, 1000mm) ─────────────────────────────────────────
println("\n=== comparison: isapprox(1m, 1000mm) ===")
println("floats:")
display(@benchmark isapprox(1.0, 1000.0 * 1e-3))

println("\nUnitful:")
display(@benchmark isapprox(1.0Unitful.@u_str("m"), 1000.0Unitful.@u_str("mm")))

println("\nUnitTypes:")
display(@benchmark isapprox(UnitTypes.Meter(1.0), UnitTypes.MilliMeter(1000.0)))
