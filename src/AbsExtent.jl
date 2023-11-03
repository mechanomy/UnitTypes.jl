


module AbsExtent
  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export AbstractExtent
  abstract type AbstractExtent <: AbstractMeasure end # an extent is a physical length or distance with a base unit of Meter

  export Meter, MilliMeter, Inch, Foot, Degree, Radian
  @makeMeasure Meter AbstractExtent 1.0 "m"
  @makeMeasure MilliMeter AbstractExtent 0.001 "mm"
  @makeMeasure Inch AbstractExtent 25.4/1000 "in"
  @makeMeasure Foot AbstractExtent 12*25.4/1000 "ft"

  # also define a unit constructor? namespace collisions anyone?
  # m(x::T) where T<:Number = Meter(x)
  # mm(x::T) where T<:Number = MilliMeter(x)

  # use the above conversions to fairly compare
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:AbstractExtent} = isapprox(convert(Meter,x).value, convert(Meter,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractExtent, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units


  # to Unitful
  # from Unitful


  # abstract type AbstractQuantity <: AbstractMeasure end
  # @makeMeasure Quantity AbstractQuantity 1.0; ...define these with an integer constructor...
  # @makeMeasure Count AbstractQuantity 1.0;


end
