# Overview
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
```
julia> v = MeterPerSecond(3)
3m/s

julia> goFaster(v)
ERROR: MethodError: no method matching goFaster(::MeterPerSecond)

Closest candidates are:
  goFaster(::T) where T<:AbstractAcceleration
```

# Introducing new types
Three macros are provided to enable new measures and dimensions to be created.
* `@makeBaseMeasure` - introduces a new basic Measure like Meter or Volume
* `@makeDerivedMeasure` - introduces a new name for a Measure, often a prefix like Millimeter or Liter
* `@makeDimension` - creates a Dimension, which is a Measure in some particular context, as diameter, radius, and circumference all refer to lengths of a circle

# Design
UnitTypes introduces an abstract type hierarchy of:
* `AbstractDimension` - src/Dimension.jl
`AbstractDiameter`, `AbstractRadius`, ...
`AbstractDuration`, ...
See [src/CommonDimensions.jl](src/CommonDimensions.jl)

* AbstractMeasure
`Meter`, `Millimeter`, ..., `MeterPerSecond`, `MeterPerSecond2`, ... See [src/SIDerived.jl](src/SIDerived.jl)
`Inch`, `Foot`, `Mile`, ..., See [src/Imperial.jl](src/Imperial.jl)

Please open an issue or PR to add more units.

The idea is that a Measure is some quantity bearing units, while a Dimension is some context-specific application of a Measure.
For instance, a circle may be described by its radius, diameter, or circumference, concepts that can be interchangeably converted, using any Measure of extent.
A function creating a circle can then store radii while accepting Radius, Diameter, or Circumference arguments, as the type system provides conversion between the argument and the function's internal convention.

Any concrete Dimension consists of a struct with field 
```julia
struct Diameter{T <: AbstractLength } <: AbstractDimension
  value::T
end
```
and a concrete Measure is represented
```julia
struct Meter <: AbstractLength
  value::Number
  toBase::Number
  unit::String
end
```

Measures.jl and the macros define the necessary convert()s and other operators.
Please open an issue _with a minimal working example_ if you run into conversion errors.

## Logical operations
Using units correctly requires some rules on coherence.
Inches can be added, as can inch and millimeter, but only when computing area does inch*inch make sense.
Inch * 3 is convenient while 3 / Inch is unlikely to be desirable.

With use and issues, these coherence rules will become more clear and explained by example.

# Comparsion with other packages
## Unitful.jl
[Unitful](https://painterqubits.github.io/Unitful.jl/latest/) leverages parametric types to store units, giving flexibility at the cost of compile-time type uncertainty.
It's two major limitations are the avoidance of [angular measures](https://painterqubits.github.io/Unitful.jl/latest/trouble/#promotion-with-dimensionless-numbers), as they are not first-class but rather ratios, and rather lengthy type unions that clutter outputs, especially on error:

```julia
julia> function goSlower(x::T) where T<:Unitful.Acceleration end
goSlower (generic function with 1 method)

julia> a = 1u"mm"

julia> goSlower(a)
ERROR: MethodError: no method matching goSlower(::Quantity{Int64, 𝐋 , Unitful.FreeUnits{(mm,), 𝐋 , nothing}})

Closest candidates are:
  goSlower(::T) where T<:(Union{Quantity{T, 𝐋 𝐓^-2, U}, Level{L, S, Quantity{T, 𝐋 𝐓^-2, U}} where {L, S}} wher    e {T, U}) 
```

## DynamicQuantities.jl
[DynamicQuantities](https://github.com/SymbolicML/DynamicQuantities.jl) is newer and faster than Unitful because it "defines a simple statically-typed Quantity type for storing physical units."
It does this by storing the exponents on the basic units, allowing any unit traceable to SI to be used.
But this performant representation hurts readability, and while the unit representation may be able to be hidden behind overrides of show(), Julia is designed for types to be read and manipulated directly by users.

## UnitTypes.jl
In the presence of Julia's type-heavy UI, these two, good attempts feel misdirected and motivate this literal typing of units.
The cost is that UnitTypes does not have a catch-all unit representation.
Only units that have been defined by one of the macros may be represented, and complex units may need to have additional methods written to correctly convert between units, ie Celsius to Fahrenheit.
This is not to say that this catch-all representation is not possible, but rather that it is not the first priority in writing the module as it likely involves unit string parsing and elaboration.
