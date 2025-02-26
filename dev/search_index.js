var documenterSearchIndex = {"docs":
[{"location":"#UnitTypes.jl","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"This package provides physical units as Julia types.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"julia> using UnitTypes\n\njulia> x = Meter(3)\n3m\n\njulia> typeof(x)\nMeter\n\njulia> typeof(x) <: AbstractLength\ntrue\n\njulia> typeof(x) <: AbstractCapacitance\nfalse","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"This allows you to easily write functions with arguments restricted to variables having certain types.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"julia> function goFaster(a::T) where T<:AbstractAcceleration end","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"This leads to correctness and very clear error messages.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"julia> goFaster(3u\"m/s\")\nERROR: MethodError: no method matching goFaster(::MeterPerSecond)\n\nClosest candidates are:\n  goFaster(::AbstractAcceleration)","category":"page"},{"location":"#Introducing-new-types","page":"UnitTypes.jl","title":"Introducing new types","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"Macros are used to introduce and create relationships around new types:","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"@makeBaseMeasure Length Meter \"m\" - introduces a new basic Measure like Meter for Length or Meter3 Volume,\n@makeMeasure Meter(1000) = KiloMeter(1) \"km\" - derives a new measure (KiloMeter) from some an existing measure (Meter) with conversion ratio 1000m = 1m\n@makeDimension Diameter Meter - creates a Dimension, which is a Measure in some particular context, as diameter, radius, and circumference all refer to lengths of a circle.","category":"page"},{"location":"#Design","page":"UnitTypes.jl","title":"Design","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"UnitTypes introduces an abstract type hierarchy of:","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"AbstractMeasure\n├─ AbstractAcceleration\n│  └─ MeterPerSecond2\n├─ AbstractAngle\n│  ├─ Degree\n│  └─ Radian\n├─ AbstractArea\n│  ├─ Acre\n│  ├─ Meter2\n│  ├─ SquareFoot\n│  └─ SquareMile\n├...and so on","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"See docs/unitTypesTree.txt for the full tree of pre-defined types.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"The idea is that a Measure is some quantity bearing units, while a Dimension is some context-specific application of a Measure. Within a Dimension multiple Measures may logically be used as long as they are dimensionally consistent. For instance, a circle may be described by its radius, diameter, or circumference, concepts that can be interchangeably converted, using any Measure of extent (<:AbstractLength). A function creating a circle can then internally store radii while accepting Radius, Diameter, or Circumference arguments as the user prefers, since the type system provides conversion between the argument and the function's internal convention.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"Internally, Dimensions look like","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"struct Diameter{T <: AbstractLength } <: AbstractDimension\n  value::T\nend","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"and a Measure is represented by","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"struct Meter <: AbstractLength\n  value::Number\n  toBase::Number\n  unit::String\nend","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"The macros in Measure.jl and Dimension.jl define the necessary convert()s and other operators. While these macros suffice for most units, defining nonlinear units (like temperature) requires adding some plumbing. See the temperature converts in Temperature.jl for an example.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"Please open an issue with a minimal working example if you run into conversion errors. Please open an issue or PR to add more units.","category":"page"},{"location":"#Logical-operations","page":"UnitTypes.jl","title":"Logical operations","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"Using units correctly requires distinguishing between valid and invalid operations, which in some cases means not allowing apparently convenient operations. Inches can be added, as can inch and millimeter, but only when computing area does inch*inch make sense. Inch * 3 is convenient while 3 / Inch is unlikely to be desirable. This is especially obvious in affine units like Temperature, where 0°C + 10°F = -12.2°C.","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"With use, patience, and issues, these coherence rules will become more clear and explained by example.","category":"page"},{"location":"#Comparison-with-other-packages","page":"UnitTypes.jl","title":"Comparison with other packages","text":"","category":"section"},{"location":"#Unitful.jl","page":"UnitTypes.jl","title":"Unitful.jl","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"Unitful leverages parametric types to store units, giving flexibility at the cost of compile-time type uncertainty. It's two major limitations are the avoidance of angular measures, as they are not first-class entities but rather ratios, and rather lengthy type unions that clutter outputs, especially on error:","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"julia> function goSlower(x<:Unitful.Acceleration) end\ngoSlower (generic function with 1 method)\n\njulia> goSlower(1u\"mm\")\nERROR: MethodError: no method matching goSlower(::Quantity{Int64, 𝐋 , Unitful.FreeUnits{(mm,), 𝐋 , nothing}})\n\nClosest candidates are:\n  goSlower(::T) where T<:(Union{Quantity{T, 𝐋 𝐓^-2, U}, Level{L, S, Quantity{T, 𝐋 𝐓^-2, U}} where {L, S}} where {T, U})","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"As Unitful is the dominant unit package and has wide use and support, we provide a separate package ExchangeUnitful to enable interoperation with Unitful.","category":"page"},{"location":"#DynamicQuantities.jl","page":"UnitTypes.jl","title":"DynamicQuantities.jl","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"DynamicQuantities is newer and faster than Unitful because it \"defines a simple statically-typed Quantity type for storing physical units.\" It does this by storing the exponents on the basic units, allowing any unit traceable to SI to be used. But this performant representation hurts readability, and while the unit representation may be able to be hidden behind overrides of show(), Julia is designed for types to be read and manipulated directly by users.","category":"page"},{"location":"#UnitTypes.jl-2","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"In the presence of Julia's type-heavy UI, these two, good attempts feel misdirected and motivate this package's literal typing of units. The limitation is that UnitTypes does not have a catch-all unit representation. Only units that have been defined by one of the macros may be represented, and complex units may need to have additional methods written to correctly convert between units. See Temperature.jl for an example of manual unit conversion.","category":"page"},{"location":"#Docs","page":"UnitTypes.jl","title":"Docs","text":"","category":"section"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"CurrentModule=UnitTypes","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"UnitTypes.Measure.@makeBaseMeasure\nUnitTypes.Measure.@makeMeasure\nUnitTypes.Measure.@relateMeasures","category":"page"},{"location":"","page":"UnitTypes.jl","title":"UnitTypes.jl","text":"UnitTypes.@makeDimension\nUnitTypes.@relateDimensions","category":"page"},{"location":"#UnitTypes.@makeDimension","page":"UnitTypes.jl","title":"UnitTypes.@makeDimension","text":"Make a new dimension dimName of measure; also creates 'AbstractdimName'\n\n```julia\n\n@makeDimension Diameter Meter \n\nd = Diameter(MilliMeter(3.4))\nr = Radius(d)\n\n```\n\n\n\n\n\n","category":"macro"},{"location":"#UnitTypes.@relateDimensions","page":"UnitTypes.jl","title":"UnitTypes.@relateDimensions","text":"Defines various Base. functions that facilitate the given relationship.   All types must already be defined and written in the form type1 = factor * type2, as in:   julia     @relateDimensions Diameter = 2.0*Radius\n\n\n\n\n\n","category":"macro"}]
}
