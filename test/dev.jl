module UnitTypesDev
  using UnitTypes


  # https://docs.julialang.org/en/v1/manual/interfaces/#man-interfaces-broadcasting

  # Base.broadcastable(x::T) where T<:AbstractMeasure = Ref(x) # If a type is intended to act like a "0-dimensional scalar" (a single object) rather than as a container for broadcasting, then the following method should be defined:
  function devBroadcast()
    @show a = [1,2,3] .* Meter(4)
    @show typeof(a)
    @show typeof(a[1])
    @show Meter(1) .* [1,2,3]
  end
  # devBroadcast()

  # Base.iterate(x::T, state=1) where T<:AbstractMeasure = state > T.count ? nothing : (state*state, state+1) not needed
  function devIterate()
    for m in Meter.([1,2,3])
      @show m
    end
  end
  # devIterate()

  # to get ranges it seems I can either implement the lower functions needed to build ranges, or directly add a _colon operator
#   Base.zero(x::T) where T<:AbstractMeasure = T(0)
#   # Base.rem(x::T, y::U, r::RoundingMode=RoundToZero) where {T<:AbstractLength, U<:AbstractLength} = x/y
#   # function Base._colon(start::T, step::U, stop::V) where {T<:AbstractLength, U<:AbstractLength, V<:AbstractLength} # somehow need to ensure that these abstracts are all the same
#   #   s0 = start.value
#   #   s1 = convert(T, stop).value
#   #   st = convert(T, step).value
#   #   rng = s0 : st : s1
#   #   return T.(rng)
#   # end
# # somehow need to ensure that these abstracts are all the same
#   Base._colon(start::T, step::U, stop::V) where {T<:AbstractLength, U<:AbstractLength, V<:AbstractLength} = T.(start.value : convert(T,step).value : convert(T,stop).value)

  function devRange()
    @show b = Meter(1) : Meter(0.3) : Meter(2)
    @show b[1]
    @show b[2]
    @show c = LinRange(Meter(10), Meter(20), 4)
    @show c[1]
    @show c[2]
  end
  # devRange()

end


#=
#print the type tree
  using UnitTypes
  import InteractiveUtils
  import AbstractTrees
  AbstractTrees.children(d::DataType) = InteractiveUtils.subtypes(d)
  AbstractTrees.print_tree(AbstractMeasure)
=#
;