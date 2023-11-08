module SIBase
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent

  export Meter
  @makeMeasure Meter "m" 1.0 AbstractExtent 

  # it's more natual to have these in AbsExtent, but then there is a circular dependence with Meter defined here via functions found in AbsMeasure/AbsExtent that also need to import SIBase to use Meter..
  Base.isequal(x::T, y::U) where {T<:AbstractExtent, U<:AbstractExtent} = convert(Meter,x).value == convert(Meter,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:AbstractExtent} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
end
