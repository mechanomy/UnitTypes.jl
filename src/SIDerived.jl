@makeMeasure Millimeter "mm" 0.001 Meter
@makeMeasure Centimeter "cm" 0.01 Meter
@makeMeasure Decimeter "dm" 0.01 Meter
@makeMeasure Kilometer "km" 1000 Meter

# eventually move this standard consistency testing to some test module than can be run across all subtypes of AbstractLength
@testitem "testing SI" begin
  @test Meter(1.0) == Meter(1.0)
  @test Meter(1.0) ≈ Millimeter(1000.0)
end
@testitem "passing through float?" begin
  # @show Meter(3)*2
  # @show Centimeter(5)
  # @show Meter(3)*2 + Centimeter(5)

  @test Meter(3)*2 + Centimeter(5) ≈ Meter(6.05)
  @test Millimeter(1)*Meter(3) / Millimeter(1) ≈ Meter(3)
end


@makeBaseUnit Area Meter2 "m^2"
# @makeBaseUnit Area MeterSquare "m^2"
# display( names(Measure.@returnModuleName) )

# @unitProduct Meter Meter Meter2
# @unitDivide Meter2 Meter Meter
# @show @macroexpand @unitOps Meter Meter Meter2
@unitOps Meter Meter Meter2




@makeBaseUnit Frequency Hertz "Hz"
Base.convert(::Type{Second}, x::Hertz) = Second(1/(x.value*x.toBase))


# @testitem " test conversions between units " begin
#   @show s = Second(1)
#   @show f = Hertz(11)
#   @show convert(Second, f)
# end

@makeBaseUnit Velocity Vel "m/s^2"

@makeBaseUnit Acceleration Accel "m/s^2"

@makeBaseUnit Force Newton "N"

# @unitProduct Kilogram Accel Newton
# @unitDivide Force Kilogram Accel
# @unitDivide Force Accel Kilogram
@unitOps Kilogram Accel Newton

@makeBaseUnit Torque NewtonMeter "Nm"
# @unitProduct Newton Meter NewtonMeter
# @unitDivide NewtonMeter Newton Meter
# @unitDivide NewtonMeter Meter Newton
@unitOps Newton Meter NewtonMeter

@makeBaseUnit Pressure Pascal "Pa"
# @unitProduct Force Area Pascal
# @unitDivide Pascal Force Area
# @unitDivide Pascal Area Force
@unitOps Newton Meter2 Pascal
