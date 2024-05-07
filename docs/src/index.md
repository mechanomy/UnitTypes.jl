# UnitTypes.jl
This package provides physical units as Julia types.

[Repo](https://github.com/mechanomy/UnitTypes.jl)

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
julia> function goFaster(a::AbstractAcceleration) end
```

This leads to correctness and very clear error messages.
```julia
julia> v = MeterPerSecond(3)
3m/s

julia> goFaster(v)
ERROR: MethodError: no method matching goFaster(::MeterPerSecond)

Closest candidates are:
  goFaster(::AbstractAcceleration)
```


## Design
UnitTypes introduces an abstract type hierarchy of:

### `AbstractMeasure`
* `Meter`, `MilliMeter`, ..., `MeterPerSecond`, `MeterPerSecond2`, ... See [src/SIDerived.jl](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/SIDerived.jl)
* `Inch`, `Foot`, `Mile`, ..., See [src/Imperial.jl](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/Imperial.jl)

### `AbstractDimension` 
* `AbstractDiameter`, `AbstractRadius`, ...
* `AbstractDuration`, ...,

In organizing types around AbstractMeasure and AbstractDimension, the idea is that a Measure is some quantity bearing units, while a Dimension is some context-specific application of a Measure.
Within a Dimension multiple Measures may logically be used as long as they are dimensionally consistent.
For instance, a circle may be described by its radius, diameter, or circumference, concepts that can be interchangeably converted, using any Measure of extent (<:AbstractLength).
A function creating a circle can then internally store radii while accepting Radius, Diameter, or Circumference types, as the type system provides conversion between the argument and the function's internal representation.

Concrete Dimensions look like
```julia
struct Diameter{T <: AbstractLength } <: AbstractDimension
  value::T
end
```
and a concrete Measure is represented by
```julia
struct Meter <: AbstractLength
  value::Number
  toBase::Number
  unit::String
end
```
## Introducing new types
Macros are used to introduce and create relationships around new types:
* `@makeBaseMeasure Torque NewtonMeter "N*m"` - introduces a new basic Measure like Meter for Length or Meter3 for Volume,
* `@deriveMeasure NewtonMeter(1) = MilliNewtonMeter(1000) "mN*m` - introduces a new name for a Measure, often a prefix like MilliMeter or an alternate name like Inch, 
* `@makeDimension Diameter Meter` - creates a Dimension, which is a Measure in some particular context, as diameter, radius, and circumference all refer to lengths of a circle.

The [typeTree](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/typeTree.txt) lists all of the currently-defined default types.
See [CommonDimensions](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/CommonDimensions.jl) for more example definitions.

## Logical operations
Using units correctly requires distinguishing between valid and invalid operations, which in some cases means not allowing convenient operations.
Inches can be added, as can inch and millimeter, but only when computing area does inch*inch make sense.
Inch * 3 is convenient while 3 / Inch is unlikely to be desirable.

At any time, `.value` can be used to access the within-type numerical value, while `.toBase` provides the conversion factor to the unit's base quantity.
If `a = MilliMeter(1)`, `a.value => 1` while `a.toBase => 1000` and `Meter(a).value => 0.001`
For `b = Inch(144)`, `b.value = 144`, while `b.toBase => 0.0254` since Meter is the base unit of all Lengths.

The macros in Measures.jl and Dimension.jl define the basic convert()s and operators necessary for common tasks, but additional definitions may be necessary.
Please [open an issue](https://github.com/mechanomy/UnitTypes.jl/issues/new/choose)/PR to add more units or functions to the base module.
Please open an issue _with a minimal working example_ if you discover conversion errors.

## Comparison with other packages

### Unitful.jl
[Unitful](https://painterqubits.github.io/Unitful.jl/latest/) leverages parametric types to store units, giving flexibility at the cost of compile-time type uncertainty.
It's two major limitations are the avoidance of [angular measures](https://painterqubits.github.io/Unitful.jl/latest/trouble/#promotion-with-dimensionless-numbers), as they are not first-class but rather ratios, and rather lengthy type unions that clutter outputs, especially on error:

```julia
julia> function goSlower(x::T) where T<:Unitful.Acceleration end
goSlower (generic function with 1 method)

julia> a = 1u"mm"

julia> goSlower(a)
ERROR: MethodError: no method matching goSlower(::Quantity{Int64, 𝐋 , Unitful.FreeUnits{(mm,), 𝐋 , nothing}})

Closest candidates are:
  goSlower(::T) where T<:(Union{Quantity{T, 𝐋 𝐓^-2, U}, Level{L, S, Quantity{T, 𝐋 𝐓^-2, U}} where {L, S}} where {T, U}) 
```

As Unitful is the most widely used unit package, we provide a separate package [ExchangeUnitful](https://github.com/mechanomy/ExchangeUnitful.jl) to enable interoperation with Unitful.

### DynamicQuantities.jl
[DynamicQuantities](https://github.com/SymbolicML/DynamicQuantities.jl) is newer and faster than Unitful because it "defines a simple statically-typed Quantity type for storing physical units."
It does this by storing the exponents on the basic units, allowing any unit traceable to SI to be used.
But this performant representation hurts readability, and while the unit representation may be able to be hidden behind overrides of show(), Julia is designed for types to be read and manipulated directly by users.

### UnitTypes.jl
In the presence of Julia's type-heavy UI, these two, good attempts feel misdirected and motivate this package's literal typing of units.
The limitation is that _UnitTypes does not have a catch-all unit representation_.
Only units that have been defined by one of the macros may be represented, and complex units may need to have additional methods written to correctly convert between units, ie Celsius to Fahrenheit.
See [SIDerived.jl](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/SIDerived.jl) and [Imperial.jl](https://github.com/mechanomy/UnitTypes.jl/tree/main/src/Imperial.jl) for examples.

## Docs

```@meta
CurrentModule=UnitTypes
```

```@autodocs
Modules=[UnitTypes]
```

