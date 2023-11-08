
module UnitTypesDev

using UnitTypes

  # makeMeasure MilliMeter 0.001 Meter "mm" # can I infer AbstractLength from Meter, given that valid conversions are between siblings? supertype(meter) == AbstractLength.
  macro makeMeter(name, toBase, base, unit="")
    esc(
      quote
        struct $name <: $(supertype(typeof(base)))
          value::Number
          toBase::Number
          unit::String
          $name(x::X where X<:Number) = new(x,$toBase,$unit)
        end
        # $name(x::T where T<:$(supertype(typeof(base)))) = convert($name, x) # conversion by constructor: Inch(x::T where T<:AbstractLength) = convert(Inch, x)
      end
    )
  end

  @makeMeter CentiMeter 0.01 Meter "cm"
  @show CentiMeter(1.0)





end