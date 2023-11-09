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


@testitem " test conversions between units " begin
  @show s = Second(1)
  @show f = Hertz(11)
  @show convert(Second, f)
end

@makeBaseUnit Force Newton "N"
# Base.convert(::Type{Newton})