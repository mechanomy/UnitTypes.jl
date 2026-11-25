module UnitTypesStaticArraysExt
using UnitTypes, LinearAlgebra, StaticArrays, TestItems

# StaticArrays' own norm(::StaticArray) is more specific than UnitTypesLinearAlgebraExt's norm(::AbstractVector{<:AbstractMeasure}) and returns the base unit, so static vectors of measures get their own methods with the same first-element-unit rule.
function LinearAlgebra.norm(a::StaticVector{N,T}, p::Real=2) where {N, T<:UnitTypes.AbstractMeasure}
  N == 0 && return zero(T)
  x = first(a)
  return UnitTypes.withValue(x, norm(map(y -> UnitTypes.sameUnitValue(x, y), a), p))
end

@testitem "StaticArrays norm of measures" begin
  using LinearAlgebra, StaticArrays
  v = SVector(MilliMeter(3), MilliMeter(4))
  @test norm(v) === MilliMeter(5)
  @test norm(v, 1) ≈ MilliMeter(7)
  @test norm(v, Inf) ≈ MilliMeter(4)
  @test norm(SVector(MilliMeter(2), MilliMeter(3), MilliMeter(6))) ≈ MilliMeter(7)
  @test norm(SVector{0,MilliMeter}()) === MilliMeter(0)
  @test norm(MVector(MilliMeter(3), MilliMeter(4))) ≈ MilliMeter(5)
  @test (@allocated norm(v)) == 0
end

end
