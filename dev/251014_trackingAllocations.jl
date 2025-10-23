using BenchmarkTools
import Unitful  # v1.25.0
import UnitTypes 

a = 1.1
b = 2.2
for i in 1:10
  local n = @timed a+b
  println("Float: $(n.time) $(n.bytes)")
end
display(@benchmark a+b)

println("\nUnitful:")
a = 1.1Unitful.@u_str("m")
b = 2.2Unitful.@u_str("m")
c = 3.3Unitful.@u_str("mm")

for i in 1:10
  local n = @timed a+b
  println("Unitful a+b: $(n.time) $(n.bytes)")
end
display(@benchmark a+b)


println("\nFloatWrap by hand:")
abstract type AbsFloat end
struct FloatWrap <: AbsFloat
  value::Float64
end
struct FloatWrapMilli <: AbsFloat
  value::Float64
end
function Base.convert(::Type{FloatWrap}, y::FloatWrapMilli)
  return FloatWrap(y.value/1000 )
end
function Base.convert(::Type{FloatWrapMilli}, y::FloatWrap)
  return FloatWrapMilli(y.value*1000 )
end
a = FloatWrap(1.1)
b = FloatWrap(2.2)
c = FloatWrapMilli(3.3)
Base.:+(x::T, y::U) where {T<:AbsFloat, U<:AbsFloat} = T(x.value+convert(T,y).value)
for i in 1:10
  local n = @timed a+b
  println("FloatWrap a+b: $(n.time) $(n.bytes)")
end
# Base.:+(x::T, y::U) where {T<:AbsFloat, U<:AbsFloat} = T(x.value+y.value) # remove the convert
# for i in 1:10
#   local n = @timed a+b
#   println("FloatWrap a+b no convert: $(n.time) $(n.bytes)")
# end
display(@benchmark a+b)


println("\nUnitTypes:")
# for i in 1:10
#   local a = UnitTypes.Meter(rand())
#   local b = UnitTypes.Meter(rand())
#   local n = @timed a+b
#   println("....$(round(n.time*1e6))us  $(n.bytes)B a+b: package implementation")
# end
a = UnitTypes.Meter(rand())
b = UnitTypes.Meter(rand())
println("package default")
display(@benchmark a+b)

Base.:+(x::T where T<:UnitTypes.AbstractLength, y::U where U<:UnitTypes.AbstractLength) = UnitTypes.Meter(x.value + convert(UnitTypes.Meter,y).value) 
# for i in 1:10
#   local a = UnitTypes.Meter(rand())
#   local b = UnitTypes.Meter(rand())
#   local n = @timed a+b
#   println("....$(round(n.time*1e6))us  $(n.bytes)B a+b: abstract +, abstract convert")
# end
a = UnitTypes.Meter(rand())
b = UnitTypes.Meter(rand())
println("abstract +, abstract convert")
display(@benchmark a+b)

Base.:+(x::UnitTypes.Meter, y::UnitTypes.Meter) = UnitTypes.Meter(x.value + convert(UnitTypes.Meter,y).value) 
# for i in 1:10
#   local a = UnitTypes.Meter(rand())
#   local b = UnitTypes.Meter(rand())
#   local n = @timed a+b
#   println("....$(round(n.time*1e6))us  $(n.bytes)B a+b: concrete +, abstract convert")
# end
a = UnitTypes.Meter(rand())
b = UnitTypes.Meter(rand())
println("concrete +, abstract convert")
display(@benchmark a+b)

function Base.convert(::Type{UnitTypes.Meter}, y::UnitTypes.Meter) 
  return y
end
# function Base.convert(::Type{UnitTypes.Meter}, y::U where U<:UnitTypes.AbstractLength)
#   return T(y.value * allUnitTypes[U].toBaseFactor / allUnitTypes[T].toBaseFactor )
# end
# for i in 1:10
#   local a = UnitTypes.Meter(rand())
#   local b = UnitTypes.Meter(rand())
#   local n = @timed a+b
#   println("....$(round(n.time*1e6))us  $(n.bytes)B a+b: concrete +, concrete convert")
# end
a = UnitTypes.Meter(rand())
b = UnitTypes.Meter(rand())
println("concrete +, concrete convert")
display(@benchmark a+b)

Base.:+(x::UnitTypes.Meter, y::UnitTypes.Meter) = UnitTypes.Meter(x.value + y.value) # from 208 bytes to 48 bytes
# for i in 1:10
#   local a = UnitTypes.Meter(rand())
#   local b = UnitTypes.Meter(rand())
#   local n = @timed a+b
#   println("....$(round(n.time*1e6))us  $(n.bytes)B a+b: concrete +, no convert")
# end
a = UnitTypes.Meter(rand())
b = UnitTypes.Meter(rand())
println("concrete +, no convert")
display(@benchmark a+b)


