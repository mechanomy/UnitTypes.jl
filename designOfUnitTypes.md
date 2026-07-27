# The Design of UnitTypes.jl

UnitTypes.jl is a Julia library for unit-typed quantities. Its central premise is that units are part of the type, not a runtime tag — `Meter(1.5)` and `MilliMeter(1500)` are distinct concrete types, both subtypes of `AbstractLength`, and the compiler enforces dimensional correctness through ordinary dispatch. This document describes the architecture from the ground up.

## The Type Hierarchy

Every unit type lives in a three-level hierarchy:

```
AbstractMeasure              # root
  AbstractLength             # abstract dimension, created by @makeBaseMeasure
    Meter                    # base concrete type
    MilliMeter               # scale variant, created by @makeMeasure
    CentiMeter
    Inch
    ...
  AbstractMass
    KiloGram
    ...
  AbstractArea               # derived from Length*Length
    Meter2
    ...
```

The abstract intermediate layer (`AbstractLength`, `AbstractMass`, etc.) is the key to the design. Methods that accept any length write `x::AbstractLength`. The concrete types carry the actual value and the implicit scale factor. There is no runtime tag distinguishing "this float is in meters" from "this float is in millimeters" — that distinction is entirely in the type.

All unit types share a minimal common structure: a single `value::Float64` field holding the measurement in the *creator's* unit, not necessarily in SI base units. `Meter(1.5)` stores `1.5`; `MilliMeter(1500)` stores `1500`. Conversion between them is handled by registered functions, not by storing a canonical base value.

## Registering Types: @makeBaseMeasure and @makeMeasure

Two macros introduce types into the system.

`@makeBaseMeasure` creates a root unit — one with no parent to convert through:

```julia
@makeBaseMeasure Length Meter "m"
```

This produces:
- `abstract type AbstractLength <: AbstractMeasure end`
- `struct Meter <: AbstractLength; value::Float64; end`
- An entry in `allUnitTypes[Meter]` with `toBase = x->x`, `fromBase = x->x`, and `dimensions = {AbstractLength=>1}`
- A call to `makeSelfConversion(Meter, mod)` which emits all the arithmetic operators (`+`, `-`, `*`, `/` with scalars, `isapprox`, `isequal`, `isless`) as concrete methods

`@makeMeasure` creates a scaled variant of an existing type:

```julia
@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"
```

The arguments define the linear relationship: `1e-3 * Meter = 1 * MilliMeter`, so `toBase = x -> x * 1e-3` and `fromBase = x -> x * 1000`. Internally, MilliMeter gets the same `AbstractLength` parent as Meter, so it participates in all the same dispatch.

After registering the new type, `@makeMeasure` calls `makeJointConversions(MilliMeter, mod)`, which iterates every already-registered `AbstractLength` type and emits one `convert`, `isapprox`, `isequal`, `isless`, `+`, and `-` method per pair, capturing the `toBase`/`fromBase` closures at emit time for zero-allocation dispatch. (See `zeroAllocations.md` for how that works.)

`@makeMeasure` also supports affine conversions via anonymous function factors:

```julia
@makeMeasure (x->x+273.15) Kelvin = (x->x-273.15) Celsius "°C"
```

The `isAffine` flag is set for these types and `makeJointConversions` skips `+`/`-` between different affine types, since adding 20°C to 300°K is physically meaningless without an offset correction.

## The allUnitTypes Registry

The global dictionary `allUnitTypes::Dict{DataType, UnitTypeAttributes}` is the central registry. Each entry records:

- `abstract` — the abstract dimension type (e.g. `AbstractLength`)
- `base` — the base type for this dimension (e.g. `Meter`)
- `toBase` / `fromBase` — conversion functions to and from SI
- `abbreviation` — the display string (`"mm"`)
- `isAffine` — whether conversions involve an offset
- `dimensions` — a `Dict{DataType,Int}` mapping abstract types to exponents, e.g. `{AbstractLength=>1}` for any length, `{AbstractLength=>1, AbstractTime=>-1}` for velocity

The `dimensions` map is what allows `UnitExpr` to resolve derived units back to named types at runtime (see below). It is populated by `@makeBaseMeasure` (trivially `{AbstractX=>1}`) and updated by `@relateMeasures` when multiplicative relationships are established.

## Relating Units: @relateMeasures

Multiplication and division between different dimensions must be declared explicitly:

```julia
@makeBaseMeasure Area   Meter2 "m^2"
@relateMeasures  Meter * Meter = Meter2

@makeBaseMeasure Force  Newton "N"
@relateMeasures  KiloGram * MeterPerSecond2 = Newton
```

`@relateMeasures` calls `addRelations`, which emits abstract-type dispatch methods such as:

```julia
Base.:*(x::AbstractLength, y::AbstractLength) = Meter2(convert(Meter, x).value * convert(Meter, y).value)
Base.:/(x::AbstractArea,   y::AbstractLength) = Meter(convert(Meter2, x).value / convert(Meter, y).value)
```

The abstract types in the signature mean that any length times any length — `Meter * MilliMeter`, `CentiMeter * Inch`, etc. — is handled by a single method pair without additional code.

`addRelations` also updates the `dimensions` map of the result type (`Meter2` gets `{AbstractLength=>2}`), registers the power relation for zero-allocation exponentiation via `registerPower`, and defines `Base.sqrt` for the squared case.

A critical subtlety: Julia's `hasmethod` returns `true` for parametric catch-all methods, so the guard that prevents duplicate definitions must use `hasExactMethod`, which compares method signatures exactly. Without this, a catch-all `*(T<:AbstractMeasure, U<:AbstractMeasure)` in UnitExpr.jl would fool the guard into skipping the specific `*(AbstractLength, AbstractLength)` definition, forcing all such multiplications through the slow generic path.

`@relateMeasures` also handles inverses:

```julia
@relateMeasures 1/Second = Hertz
```

This sets `Hertz.dimensions = {AbstractTime=>-1}` and emits `Base.:/(x::Number, y::AbstractTime)` and `Base.:*(x::AbstractFrequency, y::AbstractTime)` methods.

## Catchall:

Not every unit combination has a registered named type. Multiplying a `Meter` by a `Newton` produces a torque, but only if `@relateMeasures Meter*Newton = NewtonMeter` has been declared. Without that, the result still needs to go somewhere useful.

`UnitExpr` is that fallback:

```julia
struct Catchall <: AbstractMeasure
  value::Float64           # in SI base units
  dimensions::Dict{DataType,Int}
end
```

The generic catch-all operators at the bottom of the dispatch stack are:

```julia
Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} =
  resolveOrExpr(toBaseFloat(x) * toBaseFloat(y),
                mergeBaseDimensions(getDimensions(x), getDimensions(y), 1))
```

`resolveOrExpr` looks up the resulting dimension map in `allUnitTypes` and returns the named type if one matches, or a `Catchall` if not. This means `Meter(1.0) * Newton(2.0)` returns `NewtonMeter(2.0)` if that relation is registered, without any extra code at the call site.

The `Catchall` path allocates (it constructs a `Dict` and iterates `allUnitTypes`) and is intentionally the slow path. Registered relationships dispatch through the zero-allocation abstract-type methods from `addRelations`. `Catchall` exists so that code combining units in ad-hoc or exploratory ways doesn't crash — it degrades gracefully rather than throwing.

## Integer Exponentiation

`Meter(2.0)^3` follows a two-tiered approach:

For compile-time literal exponents, Julia emits `Base.literal_pow(^, x, Val{3}())`. `registerPower` (called by `addRelations` when squared and cubed relations are declared) emits specific `literal_pow` methods per concrete type:

```julia
Base.literal_pow(::typeof(^), x::Meter, ::Val{3}) = Meter3(toBase_Meter(x.value)^3)
Base.literal_pow(::typeof(^), x::MilliMeter, ::Val{3}) = Meter3(toBase_MilliMeter(x.value)^3)
```

These methods capture `toBase` at registration time, so dispatch is zero-allocation. Chain detection in `addRelations` handles higher powers: when `@relateMeasures Meter2*Meter = Meter3` is processed, the registry already knows that `AbstractLength^2 = Meter2`, so it infers `AbstractLength^3 = Meter3` and emits the corresponding `literal_pow` methods.

For variable exponents (`x^n` where `n` is a runtime integer), the fallback method on `AbstractMeasure` uses `resolveOrExpr` and does allocate — this is unavoidable since the exponent is not known at compile time.

## The u_str Macro

A string macro provides concise literal syntax and supports compound unit expressions:

```julia
1.5u"m"      # Meter(1.5)
2.0u"mm"     # MilliMeter(2.0)
1.0u"m^2"    # Meter2(1.0)
1.0u"mm*s"   # Catchall with value 1e-3 and dims {AbstractLength=>1, AbstractTime=>1}
```

The macro looks up the abbreviation in `allUnitTypes` for simple units, and delegates to `parseUnitExpr` for compound strings. `parseUnitExpr` splits on `*` and `/`, parses `^N` exponents, looks up each token by abbreviation, multiplies the base factors, accumulates the dimension map, and calls `resolveOrExpr` on the result. This means `u"mm^3"` returns `Meter3(1e-9)` if `Meter3` is registered — the string parser and the algebra share the same resolution logic.

## Dimensions: Semantic Context

Above the `AbstractMeasure` layer sits a second abstraction for physical semantics. A `Dimension` wraps a `Measure` with an engineering context:

```julia
@makeDimension Diameter Meter
@makeDimension Radius   Meter
```

`Diameter{T<:AbstractLength}` and `Radius{T<:AbstractLength}` are both parametric structs holding a `.measure`. They prevent mixing incompatible roles: a function requiring a `Diameter` argument will not accept a raw `Meter`, even though `Diameter` stores a `Meter` internally.

`@relateDimensions` defines the linear relationship between two dimensions:

```julia
@relateDimensions Diameter = 2.0 * Radius
```

This emits converting constructors (`Radius(d::AbstractDiameter) = Radius(d.measure/2)`), `convert` methods, `isapprox`, and comparison operators. Arithmetic between dimensions returns the underlying `Measure`, since the result of adding two diameters is a length, not a diameter in general.

## Module Structure

The include order in `UnitTypes.jl` reflects the dependency chain:

1. **Measure.jl** — the machinery: `AbstractMeasure`, `UnitTypeAttributes`, `allUnitTypes`, the macros, `makeSelfConversion`, `makeJointConversions`, `addRelations`, `registerPower`, `hasExactMethod`, `toBaseFloat`, `u_str`
2. **Catchall.jl** — the catch-all `Catchall` type, the generic `*`/`/`/`^` operators, `resolveOrExpr`, `parseUnitExpr`
3. **SI.jl** — all SI units and relations (Length, Mass, Time, Frequency, Velocity, Force, Torque, Pressure, Charge, Voltage, …)
4. **Imperial.jl** — Imperial units (Inch, Foot, Yard, Mile, fluid volumes, mass, force)
5. **Angle.jl** — `Radian`, `Degree`, typed `sin`/`cos`/`tan`, and the `pi`/`tau` constants as `Radian` values
6. **Temperature.jl** — `Kelvin`, `Celsius`, `Fahrenheit`, `Rankine` with affine conversions
7. **Dimension.jl** — the `@makeDimension` and `@relateDimensions` macros
8. **CommonDimensions.jl** — pre-built dimensions: `Diameter`, `Radius`, `Height`, `Width`, `Depth`, `Duration`

Units in SI.jl can be used freely in Imperial.jl and Angle.jl because `makeJointConversions` registers cross-type operations when each new type is added. The order matters: `@relateMeasures` for a derived unit must come after the operand types exist, but the resulting cross-unit arithmetic is available globally once registered.

## Design Tradeoffs

**Named types over expression types.** Unitful.jl represents units as a type parameter (`Quantity{Float64, dimension, unit}`). UnitTypes represents them as concrete named structs. This makes type signatures and error messages read as `Meter` rather than `Quantity{Float64, 𝐋, typeof(m)}`, at the cost of requiring explicit `@makeBaseMeasure`/`@relateMeasures` declarations for every derived unit.

**Explicit declaration over automatic derivation.** There is no automatic dimensional analysis at the type level. `Meter * Second` does not produce `MeterSecond` unless that relation has been declared. The upside is predictability: dispatch is always to a named, human-readable type; there are no generated parametric types to interpret. The downside is that every product or quotient type of interest must be declared.

**Float64 storage.** All values are stored as `Float64`. There is no support for `Float32`, `BigFloat`, or interval arithmetic. This simplifies the generic infrastructure significantly and is appropriate for engineering work where `Float64` precision is nearly always sufficient.

**Zero allocation as a hard requirement.** All registered operations (arithmetic, conversion, comparison, exponentiation with literal exponents) run with zero heap allocation by design. This makes UnitTypes suitable for use in tight numerical loops where GC pauses are unacceptable. Unregistered combinations fall back to `Catchall`, which allocates, making the performance contract clear: if you declare the relation, it is free; if you don't, it is not.
