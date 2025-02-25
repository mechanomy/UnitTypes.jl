# UnitTypes.jl
This package provides physical units as Julia types.

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

## Introducing new types
Macros are used to introduce and create relationships around new types:
* `@makeBaseMeasure Length Meter "m"` - introduces a new basic Measure like Meter for Length or Meter3 Volume,
* `@makeMeasure Meter(1000) = KiloMeter(1) "km"` - derives a new measure (KiloMeter) from some an existing measure (Meter) with conversion ratio 1000m = 1m
* `@makeDimension Diameter Meter` - creates a Dimension, which is a Measure in some particular context, as diameter, radius, and circumference all refer to lengths of a circle.

## Design
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
│  ├─ Meter2
│  ├─ SquareFoot
│  └─ SquareMile
├...and so on
```

See docs/unitTypesTree.txt for the full tree of pre-defined types.

The idea is that a *Measure* is some quantity bearing units, while a *Dimension* is some context-specific application of a Measure.
Within a Dimension multiple Measures may logically be used as long as they are dimensionally consistent.
For instance, a circle may be described by its radius, diameter, or circumference, concepts that can be interchangeably converted, using any Measure of extent (<:AbstractLength).
A function creating a circle can then internally store radii while accepting Radius, Diameter, or Circumference arguments as the user prefers, since the type system provides conversion between the argument and the function's internal convention.

Internally, Dimensions look like
```julia
struct Diameter{T <: AbstractLength } <: AbstractDimension
  value::T
end
```
and a Measure is represented by
```julia
struct Meter <: AbstractLength
  value::Number
  toBase::Number
  unit::String
end
```

The macros in Measure.jl and Dimension.jl define the necessary convert()s and other operators.
While these macros suffice for most units, defining nonlinear units (like temperature) requires adding some plumbing.
See the temperature converts in Temperature.jl for an example.

Please open an issue _with a minimal working example_ if you run into conversion errors.
**Please open an issue or PR to add more units.**

## Logical operations
Using units correctly requires distinguishing between valid and invalid operations, which in some cases means not allowing apparently convenient operations.
Inches can be added, as can inch and millimeter, but only when computing area does inch*inch make sense.
Inch * 3 is convenient while 3 / Inch is unlikely to be desirable.
This is especially obvious in affine units like Temperature, where 0°C + 10°F = -12.2°C.

With use, patience, and issues, these coherence rules will become more clear and explained by example.

## Comparison with other packages

### Unitful.jl
[Unitful](https://painterqubits.github.io/Unitful.jl/latest/) leverages parametric types to store units, giving flexibility at the cost of compile-time type uncertainty.
It's two major limitations are the avoidance of [angular measures](https://painterqubits.github.io/Unitful.jl/latest/trouble/#promotion-with-dimensionless-numbers), as they are not first-class entities but rather ratios, and rather lengthy type unions that clutter outputs, especially on error:

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
It does this by storing the exponents on the basic units, allowing any unit traceable to SI to be used.
But this performant representation hurts readability, and while the unit representation may be able to be hidden behind overrides of show(), Julia is designed for types to be read and manipulated directly by users.

### UnitTypes.jl
In the presence of Julia's type-heavy UI, these two, good attempts feel misdirected and motivate this package's literal typing of units.
The limitation is that _UnitTypes does not have a catch-all unit representation_.
Only units that have been defined by one of the macros may be represented, and complex units may need to have additional methods written to correctly convert between units.
See Temperature.jl for an example of manual unit conversion.

## Docs
```@meta
CurrentModule=UnitTypes
```

```@docs
UnitTypes.Measure.@makeBaseMeasure
UnitTypes.Measure.@makeMeasure
UnitTypes.Measure.@relateMeasures
```

```@docs
UnitTypes.@makeDimension
UnitTypes.@relateDimensions
```


