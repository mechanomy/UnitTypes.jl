# Moles are counts of some entity and would seem to be better treated as tractQuantity? .. https://en.wikipedia.org/wiki/Amount_of_substance
# module Time
#   using TestItems 
#   using ..Measure 

#   export tractTime
#   abstract type tractTime <: tractMeasure end 

#   @makeMeasure Second "s" 1.0 tractTime

#   Base.isequal(x::T, y::U) where {T<:tractTime, U<:tractTime} = convert(Second,x).value == convert(Second,y).value
#   Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractTime, U<:tractTime} = isapprox(convert(Second,x).value, convert(Second,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
#   Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractTime, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
#   Base.convert(::Type{T}, x::U) where {T<:tractTime, U<:tractTime} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...

#   @testitem "Time conversions" begin
#     @makeMeasure TestTime "te" 1.0 tractTime
#     @makeMeasure TestTimeMilli "mte" 0.001 TestTime

#     @test TestTime(1.0) ≈ TestTimeMilli(1000)
#     @test TestTimeMilli(1000) ≈ TestTime(1.0) 
#     @test TestTime(TestTimeMilli(1000)) ≈ 1.0
#   end

# end