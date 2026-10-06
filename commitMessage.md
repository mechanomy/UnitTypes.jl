# Trigonometry on AbstractAngle

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Forward trig `sin`, `cos`, `tan`, `sec`, `csc`, `cot`, `sincos` are defined once on `AbstractAngle`, returning `Float64`.
* `Degree` dispatches to Base's `sind`, `cosd`, etc., so values at multiples of 90° are exact, e.g. `sin(Degree(180)) === 0.0`.
* Inverse trig takes the result angle type first: `asin(Radian, 0.5)`, `acos(Degree, x)`, plus `atan`, `asec`, `acsc`, `acot`; Base's `asin(::Real)` is unchanged.
* Two-argument `atan(y, x)` accepts measures of the same dimension in any units and returns `Radian`; `atan(Degree, y, x)` selects the angle type; mismatched dimensions throw an ArgumentError.
* `pi` and `tau` are no longer exported, which fixes `pi` being undefined in user code after `using UnitTypes`; they remain as `const UnitTypes.pi` and `UnitTypes.tau`.
* Fixed `Oersted` conversion, which threw a MethodError because CGS.jl's `pi` resolved to the Radian constant; it now uses `Base.pi`, with a regression test.
* New conversions use `convert()` rather than `toBaseFloat()` or conversion constructors, keeping every new method allocation-free.
* Added testitems for reciprocal functions, sincos, exact degree values, inverse trig, two-argument atan, and the unexported constants.
* Added trigonometry, inverse trigonometry, and two-argument atan sections to benchmark/benchmark.jl; all report 0 allocations.
* Postponed: angle as a dimension in BaseDimensions, additional angle units (ArcMinute, Turn, Gradian, RPM).
