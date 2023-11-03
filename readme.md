# Overview
This package provides physical units as Julia types.
```julia
using UnitTypes


```

This allows you to write functions with arguments restricted to variables having certain types.

Conversion is performed between
while 


# Goals
These goals are realized in this package and are elaborated in the documentation.
* Readable type trees
* Conceptually consistent
* Extensible
* Performant
* Convertible to other type systems


# Introducing new types
Non-abstract types are created through two macros, `@makeMeasure` and `@makeDimension`, which 


# Design
UnitTypes introduces an abstract type hierarchy of:
* AbstractDimension // dimension ( a single aspect of a given thing )?
** AbstractDiameter
** AbstractRadius
** AbstractLength
** AbstractRate
** AbstractDuration

* AbstractMeasure
** AbstractExtent
*** Meter...
*** Foot...
** AbstractVelocity
** AbstractAcceleration
** AbstractJerk

** AbstractAngle
*** Radian
*** Degree
** AbstractAngularVelocity

** AbstractForce
*** Newton
** AbstractMoment
*** OunceInch
** AbstractPressure
*** PSI

** AbstractTime

The idea is that a Measure is some quantity bearing units, while a Dimension is some context-specific application of some measure.
For instance, a circle may be described by its radius or diameter, concepts that can be interchangeably converted, using any measure of extent.
A function creating a circle can then store radii while accepting Diameter or Radius arguments, as the type system provides conversion between the argument and the function's internal convention.

Though these types appear verbose, they enable conceptual clarity throughout functions.
See the exemplary functions.

Any concrete Dimension consists of a struct with field 
```julia
struct Dimension{T <: AbstractMeasure } <: AbstractDimension
  value::T
end
```
and a concrete Measure 
```julia
struct $name <: $abstractName
  value::Number
  toBase::Number
  unit::String
end
```



# Comparsion with other packages
## Unitful.jl
Unitful is widely used and quite encompassing.
It's two major limitations are the avoidance of angular measures (as they are not first-class but rather ratios), and length type unions that clutter outputs, especially on MethodErrors.


## DynamicQuantities.jl

## Measurements.jl
