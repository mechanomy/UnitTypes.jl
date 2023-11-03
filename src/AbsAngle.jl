



module AbsAngle
  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths

  export AbstractAngle, Radian, Degree
  abstract type AbstractAngle <: AbstractMeasure end

  @makeMeasure Radian AbstractAngle 1.0 "rad"
  @makeMeasure Degree AbstractAngle π/180 "°"

  #can these be done generically through promote in Measure?
  Radian(x::Degree) = convert(Radian, x)
  Degree(x::Radian) = convert(Degree, x)

  #I like having * defined for Degree, for 3*30° things, but the danger is that theta*r operations need convert()
  # the correct solution is to push the result units back into the calc, but I don't know if this can be inferred from the types.
  # there may be a way through promotions? https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion
  Base.promote_rule(::Type{Radian}, ::Type{Degree}) = Radian #promote to Radian in general


  # all Measures can already be converted to Number, why do I need to specify these?
  Base.sin(x::Radian) = sin(x.value)
  Base.sin(x::Degree) = sin(convert(Radian, x))
  Base.cos(x::Radian) = cos(x.value)
  Base.cos(x::Degree) = cos(convert(Radian, x))
  Base.tan(x::Radian) = tan(x.value)
  Base.tan(x::Degree) = tan(convert(Radian, x))
  # Base.asin(x::T where T<:Number)::Radian = Radian(asin(x)) #given the convert to Number, will these be used and can they be tested?
  # Base.asin(x::T where T<:Number)::Degree = convert(Degree, Radian(asin(x)))


  # abstract type AbstractQuantity <: AbstractMeasure end
  # @makeMeasure Quantity AbstractQuantity 1.0; ...define these with an integer constructor...
  # @makeMeasure Count AbstractQuantity 1.0;


end