


module AbsMeasure
  # MEASURES - these units measure extent in some manner

  export AbstractMeasure, @makeMeasure
  abstract type AbstractMeasure end

  # `@makeMeasure MilliMeter` will create:
  # struct MilliMeter <: AbstractExtent
  #   value::Number
  #   toBase::Number
  #   MilliMeter(x) = new(x, 1e-3)
  # end
  # https://docs.julialang.org/en/v1/manual/metaprogramming/#Code-Generation
  macro makeMeasure(name, abstractName, toBase, unit="")
    esc(
      quote
        struct $name <: $abstractName
          value::Number
          toBase::Number
          unit::String
          $name(x) = new(x,$toBase,$unit)
        end
        $name(x::T where T<:$abstractName) = convert($name, x) # conversion by constructor: Inch(x::T where T<:AbstractExtent) = convert(Inch, x)
      end
    )
  end

  #            desired           given                            how
  Base.convert(::Type{Int32},    x::T) where T<:AbstractMeasure = Int32(round(x.value));
  Base.convert(::Type{Int64},    x::T) where T<:AbstractMeasure = Int64(round(x.value));
  # Base.convert(::Type{Float64},  x::T) where T<:AbstractMeasure = Float64(x.value);
  # Base.convert(::Type{Number},   x::T) where T<:AbstractMeasure = x.value;
  Base.convert(::Type{T},   x::Number) where T<:AbstractMeasure = T(x)::T
  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractMeasure} = convert(T, x.value )::T #convert first to Measure, then to number...this works fine in terminal

  # convert between measures
  Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works...

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {U<:AbstractMeasure, T<:Number} = isapprox(x, y.value, atol=atol, rtol=rtol) #I expected this to be implied by prior..
  Base.isapprox(x::T, y::T; atol::Real=0, rtol::Real=atol) where T<:AbstractMeasure = isapprox(x.value, y.value, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:AbstractMeasure} = isapprox(x.value, convert(T,y), atol=atol, rtol=rtol)

  # argument order matters, need to doubly define everyting:
  Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x + T(y)
  Base.:+(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) + x
  Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value+convert(T,y).value) #result returned in the unit of the first measure
  # Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = +(promote(x,y)) #this fails, probably need to make a simpler demo https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion

  Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x - T(y)
  Base.:-(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) - x
  Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value-convert(T,y).value)

  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x * T(y)
  Base.:*(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) * x
  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value*convert(T,y).value)

  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x / T(y)
  Base.:/(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) / x
  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value/convert(T,y).value)

  """
  """
  function measure2string(m::T)::String where T<:AbstractMeasure
    # return @sprintf("%3.3f []", m)
    return "$(m.value)$(m.unit)"
  end

  """
  """
  function Base.show(io::IO, m::T) where T<:AbstractMeasure
    print(io, measure2string(m))
  end
end