# UnitTypes.jl
This package provides physical units as Julia types.
[Docs](https://mechanomy.github.io/UnitTypes.jl/dev/).

```julia
julia> using UnitTypes

julia> x = Meter(3)
3m

julia> typeof(x)
Meter

julia> typeof(x) <: AbstractLength
true

julia> typeof(x) <: AbstractCapacitance
false
```

This allows you to easily write functions with arguments restricted to variables having certain types.
```julia
julia> function goFaster(a::T) where T<:AbstractAcceleration end
```

This leads to correctness and very clear error messages.
```julia
julia> goFaster(3u"m/s")
ERROR: MethodError: no method matching goFaster(::MeterPerSecond)

Closest candidates are:
  goFaster(::AbstractAcceleration)
```

## Type hierarchy
UnitTypes introduces an abstract type hierarchy of:
```
AbstractMeasure
├─ AbstractAcceleration
│  └─ MeterPerSecond2
├─ AbstractAngle
│  ├─ Degree
│  └─ Radian
├─ AbstractArea
│  ├─ Acre
│  ├─ Foot2
│  ├─ Inch2
│  ├─ Meter2
│  └─ Mile2
├...and so on
```

See [](./docs/unitTypesTree.md) for the full tree of predefined types.

Internally, a Measure is represented by
```julia
struct Meter <: AbstractLength
  value::Number
end
```
and a Dimension by
```julia
struct Diameter{T <: AbstractLength } <: AbstractDimension
  measure::T
end
```

The idea is that a *Measure* is some quantity bearing units, while a *Dimension* is some context-specific application of a Measure.
(The Measure's unit string and conversion factor are tracked in `UnitTypes.allUnitTypes`.)

Within a Dimension multiple Measures may logically be used as long as they are dimensionally consistent.
For instance, a circle may be described by its radius, diameter, or circumference, concepts that can be interchangeably converted, using any Measure of extent (<:AbstractLength).
A function creating a circle can then internally store radii while accepting Radius, Diameter, or Circumference arguments as the user prefers, since the type system provides conversion between the argument and the function's internal convention.

Please open an [issue](https://github.com/mechanomy/UnitTypes.jl/issues) _with a minimal working example_ if you run into conversion errors or think additional units should be defined by the package.

## Introducing new types
Macros are used to introduce and create relationships around new types:
* `@makeBaseMeasure Length Meter "m"` - introduces a new basic Measure like Meter for Length or Meter3 Volume, this should be rarely used!
* `@makeMeasure 1e3 Meter = 1 KiloMeter "km"` - derives a new measure (KiloMeter) from an existing measure (Meter) with a conversion ratio (1000m = 1km)
* `@relateMeasures KiloGram*MeterPerSecond2=Newton` - relates the product of types to another type, all types preexisting.

For working with Dimensions:
* `@makeDimension Diameter Meter` - creates the Dimension Diameter measured in Meters
* `@relateDimensions Diameter = 2.0*Radius` - relates the Dimensions Diameter and Radius by the scalar 2.0.

The macros in Measure.jl and Dimension.jl define the necessary convert()s and other operators.
If these are insufficient you will receive undefined method errors and can then work around the missing defitions, define them yourself, and/or open an [issue](https://github.com/mechanomy/UnitTypes.jl/issues).

## Catch-all type
When an arithmetic operation produces a unit combination with no named type, the result is a `Catchall`:
```julia
julia> Yard(2) * Second(3)  # no named type with yd*s
6.0yd*s   # Catchall with value=6.0, dimensions={AbstractLength=>1, AbstractTime=>1}
```
The price of operations producing undefined UnitTypes is that Catchalls are allocated; these can be eliminated by defining the needed types.

Compound unit strings are also parsed `1u"mm*s/kg"`, returning an explicit type or a `Catchall` for that combination.

## Logical operations
Using units correctly requires distinguishing between valid and invalid operations, which in some cases means not allowing apparently convenient operations.
Inches can be added, as can inch and millimeter, but only when computing area does inch*inch make sense.
Inch * 3 is convenient while 3 / Inch is unlikely to be desirable.
These conceptual gotchas are especially obvious in affine units like Temperature, where 0°C + 10°F is not 42°F but rather -12.2°C.

With use, patience, and [issues](https://github.com/mechanomy/UnitTypes.jl/issues), these coherence rules will become more clear and explained by example.

## Naming conventions
Combining the names of units to get the resulting type name follows these simple rules:
1. Multiplication is concatenation: Newton * Meter = NewtonMeter (==MeterNewton)
1. Division and negative exponents are indicated by `per`: N*m/s^2 = NewtonMeterPerSecond2
1. Numeric powers are preferred over words: Meter2, not SquareMeter
1. No plurals: Meter, not Meters

## Comparison with other packages

### Unitful.jl
[Unitful](https://painterqubits.github.io/Unitful.jl/latest/) leverages parametric types to store units, giving flexibility at the cost of compile-time type uncertainty.
It's two major limitations are the avoidance of [angular measures](https://painterqubits.github.io/Unitful.jl/latest/trouble/#promotion-with-dimensionless-numbers), as they are not first-class entities but rather ratios, and rather [lengthy type unions](https://discourse.julialang.org/t/how-to-properly-use-unitful/40295/14?u=bcon) that clutter outputs, especially on error:

```julia
julia> function goSlower(x<:Unitful.Acceleration) end
goSlower (generic function with 1 method)

julia> goSlower(1u"mm")
ERROR: MethodError: no method matching goSlower(::Quantity{Int64, 𝐋 , Unitful.FreeUnits{(mm,), 𝐋 , nothing}})

Closest candidates are:
  goSlower(::T) where T<:(Union{Quantity{T, 𝐋 𝐓^-2, U}, Level{L, S, Quantity{T, 𝐋 𝐓^-2, U}} where {L, S}} where {T, U})
```

As Unitful is the dominant unit package and has wide use and support, we provide a separate package [ExchangeUnitful](https://github.com/mechanomy/ExchangeUnitful.jl) to enable interoperation with Unitful.

### DynamicQuantities.jl
[DynamicQuantities](https://github.com/SymbolicML/DynamicQuantities.jl) is newer and faster than Unitful because it "defines a simple statically-typed Quantity type for storing physical units."
It does this by storing the exponents on the basic units, like the Catchall type, allowing any unit traceable to SI to be used.
But this performant representation hurts readability, and while the unit representation may be able to be hidden behind overrides of show(), Julia is designed for types to be read and manipulated directly by users.

In the presence of Julia's type-first design, these two, good attempts feel misdirected and motivate this package's literal typing of units.

## Copyright
Copyright (c) 2026 - [Mechanomy LLC](https://mechanomy.com)

## License
Released under [MIT](./license.md).