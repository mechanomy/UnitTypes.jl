@makeMeasure MilliMeter "mm" 0.001 Meter
@makeMeasure CentiMeter "cm" 0.01 Meter
@makeMeasure DeciMeter "dm" 0.01 Meter
@makeMeasure KiloMeter "km" 1000 Meter

# eventually move this standard consistency testing to some test module than can be run across all subtypes of AbstractLength
@testitem "testing SI" begin
  @test Meter(1.0) == Meter(1.0)
  @test Meter(1.0) ≈ MilliMeter(1000.0)
end


@makeBaseUnit Frequency Hertz "Hz"
Base.convert(::Type{Second}, x::Hertz) = Second(1/(x.value*x.toBase))

@testitem "passing through float?" begin
  @test Meter(3)*2 + CentiMeter(5) ≈ Meter(6.05)
  @test MilliMeter(1)*Meter(3) / MilliMeter(1) ≈ Meter(3)

end

# @testitem " test conversions between units " begin
#   @show s = Second(1)
#   @show f = Hertz(11)
#   @show convert(Second, f)
# end

@makeBaseUnit Velocity Vel "m/s^2"
@makeBaseUnit Acceleration Accel "m/s^2"


@makeBaseUnit Force Newton "N"
@makeBaseUnit Torque NewtonMeter "Nm"
# Base.:*(x::T, y::U) where {T<:AbstractForce, U<:AbstractLength} = NewtonMeter( toBaseFloat(x)*toBaseFloat(y) ) #toBaseFloat() makes the responsibilty for unit consitency explicit...
# Base.:/(x::T, y::U) where {T<:AbstractTorque, U<:AbstractLength} = Newton( toBaseFloat(x)/toBaseFloat(y) ) 
# Base.:/(x::T, y::U) where {T<:AbstractTorque, U<:AbstractForce} = Meter( toBaseFloat(x)/toBaseFloat(y) ) 

# this is going to be tedious, can it be macroed?
# first: macro the safe permutations a*b, b*a

# @unitProduct AbstractForce AbstractLength NewtonMeter
# @unitProduct Newton Meter NewtonMeter
# the commutivity of * allows permuation
# macro unitProduct(Abs1, Abs2, Operation)
#   return esc(
#     quote
#       if Base.isconcretetype($Abs1)
#         abs1 = supertype($Abs1)
#       else
#         abs1 = $Abs1
#       end
#       if Base.isconcretetype($Abs2)
#         abs2 = supertype($Abs2)
#       else
#         abs2 = $Abs2
#       end
#       Base.:*(x::T, y::U) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(x) * toBaseFloat(y) )
#       Base.:*(y::U, x::T) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(y) * toBaseFloat(x) )
#     end
#   )
# end
# @unitProduct AbstractForce AbstractLength NewtonMeter
@unitProduct Newton Meter NewtonMeter

@testitem "unitProduct" begin
  @test Newton(1)*Meter(1) ≈ NewtonMeter(1)
  @test Meter(1)*Newton(1) ≈ NewtonMeter(1)
end

# # division is not commutive, a/b != b/a
# # Base.:/(x::T, y::U) where {T<:NewtonMeter, U<:Newton} = Meter( toBaseFloat(x) / toBaseFloat(y) )
# # Base.:/(x::T, y::U) where {T<:NewtonMeter, U<:Meter} = Newton( toBaseFloat(x) / toBaseFloat(y) )

# # Input / Divsior = Output
# macro typeDivide(Input, Divisor, Output)
#   return esc(
#     quote
#       # Base.:/(x::T, y::U) where {T<:$Input, U<:$Divisor} = $Output( toBaseFloat(x) / toBaseFloat(y) )
#       absInput = supertype($Input)
#       absDivisor = supertype($Divisor)
#       Base.:/(x::T, y::U) where {T<:absInput, U<:absDivisor} = $Output( toBaseFloat(x) / toBaseFloat(y) )
#     end
#   )
# end
@unitDivide NewtonMeter Newton Meter
@unitDivide NewtonMeter Meter Newton

# Input = Watt[kg*m^2/s^2]: 
#   1: W/kg = m^2/s^2(?)
#   2: W/m = kg*m/s^2 = N()
#   3: W*s = kg*m/s(?)
# In this permutation, only 2 maps onto another unit; I'm not seeing a clean way to do this permuation that saves any work over typeDivide
# But what if typeDivide is given a more complex unit
#      W/N = m/s = Vel()

@testitem "unitDivide" begin
  @show NewtonMeter(1) / Meter(1)
  @show NewtonMeter(1) / Newton(1)
end


# second: define @derivedUnit Name Unit Force * Length ... via ?
