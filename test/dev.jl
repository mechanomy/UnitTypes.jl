
module UnitTypesDev

using UnitTypes

using Unitful


# figure out Unitful casting
# 1: the basic way is to just cover the base types manually
# 2: the right way is to understand their type unions and directly, automatically map
# 3: the easy way is to assume that unit symbols are identical and just use them to convert

# 1: 
# Base.convert(::Type{Unitful.m}, x::UnitTypes.Meter) = uconvert(u"m", toBaseFloat(x))
# Base.convert(::Type{Unitful.mm}, x::UnitTypes.Millimeter) = uconvert(u"mm", 1000*toBaseFloat(x))

# 2: right way:
# Base.convert(::Type{T}, x::U) where {T<:Unitful.Length, U<:UnitTypes.AbstractLength} = Unitful.m * toBaseFloat(x) # this forces everything to base Meter
# Base.convert(::Type{T}, x::U) where {T<:typeof(1u"mm"), U<:UnitTypes.AbstractLength} = Unitful.mm * Millimeter(x).value #but this is still called becuase it is more specific
#[ @biconvert a b(a.value) ....how to mirror on b's side? a(b.basevalue), where I don't assume that a and b both have .value? ]
#now can I do this automatically?
# I think I would need to iterate through all subtypes of AbstractMeasure and map them onto their unitful analogues via bidirectional converts()
# I can get subtypes(AbstractMeasure) or subtypes(AbstractLength), but this does not exist in Unitful...

# 3: easy way assuming unit symbols are identical
Base.convert(::Type{T}, x::U) where {T<:Unitful.Length, U<:UnitTypes.AbstractLength} = x.value * uparse(x.unit)
# when uparse() fails we get:
# uparse("mile") 'ERROR: ArgumentError: Symbol mile could not be found in unit modules Module[Unitful] Stacktrace: [1] lookup_units(unitmods::Vector{Module}, sym::Symbol)
# and when uparse() returns something different from T:
# > convert(typeof(1u"mm"), Newton(7.8))
# ERROR: DimensionError: mm and 7.8 are not dimensionally compatible. 
# .... so this doesn't seem too dangerous

a = Meter(1.2)
b = Millimeter(3.4)
c = Mile(5.6)
# @show uparse("mi")
d = Newton(7.8)
@show uparse(d.unit)

@show convert(typeof(1u"m"), a)
@show convert(typeof(1u"m"), b)
@show convert(typeof(1u"mm"), a)
@show convert(typeof(1u"mm"), b)
@show convert(typeof(1u"mm"), c)
@show convert(typeof(1u"mm"), d)


end ;