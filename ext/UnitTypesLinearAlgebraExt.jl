module UnitTypesLinearAlgebraExt
using UnitTypes, LinearAlgebra, TestItems

"""
  `norm(x::AbstractMeasure, p=2)`, `norm(v::AbstractVector{<:AbstractMeasure}, p=2)`

  The p-norm of measures of one dimension, in the unit of the first element; other elements are converted to it.
  Elements of different dimensions throw an ArgumentError, and an empty vector returns `zero(eltype(v))`.
"""
LinearAlgebra.norm(x::UnitTypes.AbstractMeasure, p::Real=2) = abs(x)
function LinearAlgebra.norm(v::AbstractVector{<:UnitTypes.AbstractMeasure}, p::Real=2)
  isempty(v) && return zero(eltype(v))
  x = first(v)
  return UnitTypes.withValue(x, norm((UnitTypes.sameUnitValue(x, y) for y in v), p))
end

@testitem "LinearAlgebra norm of measures" begin
  using LinearAlgebra
  @test norm(MilliMeter(-3)) === MilliMeter(3)
  @test norm([MilliMeter(3), MilliMeter(4)]) ≈ MilliMeter(5)
  @test norm([MilliMeter(3), MilliMeter(4)]) isa MilliMeter
  @test norm(AbstractLength[MilliMeter(3), Meter(0.004)]) ≈ MilliMeter(5)
  @test norm([MilliMeter(3), MilliMeter(4)], 1) ≈ MilliMeter(7)
  @test norm([MilliMeter(3), MilliMeter(-4)], Inf) ≈ MilliMeter(4)
  @test norm(MilliMeter[]) === MilliMeter(0)
  @test_throws ArgumentError norm(AbstractMeasure[MilliMeter(3), Second(4)])
  @test norm(view([MilliMeter(3), MilliMeter(4), MilliMeter(12)], 1:3)) ≈ MilliMeter(13)
end

end
