# UnitTypes 4.0.0 release notes (draft)

Draft, 2026-11-24; covers commits bf0e65d through b32a710 since 3.2.0.

## Breaking changes

### New Base methods on AbstractMeasure can collide with downstream definitions
UnitTypes now defines these methods for all measures:
- `abs`, `isfinite`, `isinf`, `isnan`
- `hypot(x, ys...)`
- `zero(::Type{T})`, `oneunit(::Type{T})`, `oneunit(x)`
- `round(x, r; digits, sigdigits, base)`, `floor`, `ceil`, `trunc`
- `LinearAlgebra.norm` for measures and vectors of measures, through package extensions for LinearAlgebra and for StaticArrays

A package that defined any of these itself for `AbstractMeasure`, or for a subtype UnitTypes already covers, will fail to precompile with "Method overwriting is not permitted during Module precompilation".
Delete the package's own definition.
Mechanics.jl hit this with `Base.abs(x::T) where T<:AbstractMeasure` and removed it in its commit b8a0e48.

### isapprox follows Base's default tolerance
The default was `rtol = atol`, so `≈` with no keywords meant exact equality and `atol=x` alone also applied a relative tolerance of x.
It is now Base's `rtol = atol > 0 ? 0 : √eps(Float64)`.
So `MilliMeter(3) ≈ MilliMeter(3.0000000001)` is true, and `atol=x` alone is purely absolute, in the units of the first argument.
A comparison that relied on the old relative slack needs an explicit `rtol`.

### RevolutionsPerMinute and RevolutionsPerSecond are angular velocities
They were Frequency, on a Hertz basis.
They are now AngularVelocity, with 1 rpm = 2π/60 rad/s, and convert to `RadianPerSecond` and `DegreePerSecond`, not to `Hertz`.

### Products that were Catchall now resolve to named types
- `Newton*Second` is `NewtonSecond`.
- `KiloGramMeter2*RadianPerSecond` is `NewtonMeterSecond`.
- `NewtonMeter*Second` is still Catchall, because angular momentum carries rad.
Code that dispatched on, or tested for, `Catchall` from these products must change.

### Meter4 and NewtonPerMeter are defined in UnitTypes
A package that still defines either creates its own distinct type of the same name, so code that loads both packages sees an ambiguous `Meter4` or `NewtonPerMeter` and arithmetic may resolve to the wrong one.
Delete the package's own definitions; Mechanics.jl has removed its definitions.

## Additions
- Rigid-body dynamics measures:
  - `NewtonSecond` (Momentum)
  - `NewtonMeterSecond` (AngularMomentum)
  - `KiloGramMeter2` (MassMomentOfInertia)
  - `Meter4` (AreaMomentOfInertia)
  - `RadianPerSecond` and `DegreePerSecond` (AngularVelocity)
  - `RadianPerSecond2` (AngularAcceleration)
  - `NewtonPerMeter` (LinearStiffness)
  - `NewtonSecondPerMeter` (LinearDamping)
  - `NewtonMeterPerRadian` (RotationalStiffness)
  - `NewtonMeterSecondPerRadian` (RotationalDamping)
- `@registerMeasures`: a package that defines measures calls it from its `__init__` so its registrations survive precompilation; `__precompile__(false)` is no longer needed.
- `isapprox` accepts a measure `atol`, such as `atol=MicroMeter(1)`, converted to the first argument's units.
- `hypot` and `norm` return the first argument's unit and throw an ArgumentError for mixed dimensions.
- LinearAlgebra and StaticArrays are weak dependencies, loaded only through extensions.
