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

# ── Trigonometry: sin(30°), asin(0.5) → °, atan(1m, 500mm) ───────────────────
println("\n=== trigonometry: sin(30°) ===")
println("floats:")
display(@benchmark sind(30.0))

println("\nUnitful:")
display(@benchmark sin(30.0Unitful.@u_str("°")))

println("\nUnitTypes:")
display(@benchmark sin(UnitTypes.Degree(30.0)))

println("\n=== inverse trigonometry: asin(0.5) as Degree ===")
println("floats:")
display(@benchmark asind(0.5))

println("\nUnitTypes:")
display(@benchmark asin(UnitTypes.Degree, 0.5))

println("\n=== two-argument atan: atan(1m, 500mm) ===")
println("floats:")
display(@benchmark atan(1.0, 0.5))

println("\nUnitful:")
display(@benchmark atan(1.0Unitful.@u_str("m"), 500.0Unitful.@u_str("mm")))

println("\nUnitTypes:")
display(@benchmark atan(UnitTypes.Meter(1.0), UnitTypes.MilliMeter(500.0)))

println("\n=== isapprox with a measure atol: isapprox(1m, 1000mm; atol=1μm) ===")
println("floats:")
display(@benchmark isapprox(1.0, 1.0; atol=1e-6))

println("\nUnitTypes:")
display(@benchmark isapprox(UnitTypes.Meter(1.0), UnitTypes.MilliMeter(1000.0); atol=UnitTypes.MicroMeter(1.0)))

println("\n=== rounding: round(12.00000003mm; digits=3) ===")
println("floats:")
display(@benchmark round(12.00000003; digits=3))

println("\nUnitTypes:")
display(@benchmark round(UnitTypes.MilliMeter(12.00000003); digits=3))

println("\n=== zero of a type: zero(MilliMeter) ===")
println("UnitTypes:")
display(@benchmark zero(UnitTypes.MilliMeter))

println("\n=== hypot(3mm, 0.004m) ===")
println("floats:")
display(@benchmark hypot(3.0, 4.0))

println("\nUnitTypes:")
display(@benchmark hypot(UnitTypes.MilliMeter(3.0), UnitTypes.Meter(0.004)))
