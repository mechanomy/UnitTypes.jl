module SIBase
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent

  #defined in absExtent:
  export Meter
  @makeMeasureFromAbstract Meter "m" 1.0 AbstractExtent 

  #these were in absExtent
  Base.isequal(x::T, y::U) where {T<:AbstractExtent, U<:AbstractExtent} = convert(Meter,x).value == convert(Meter,y).value
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:AbstractExtent} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
end
