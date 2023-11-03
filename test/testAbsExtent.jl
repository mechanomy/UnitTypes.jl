using Test
using UnitTypes


extents = [ UnitTypes.Meter, UnitTypes.MilliMeter, UnitTypes.Inch, UnitTypes.Foot]

@testset "test AbsExtent" begin
  for ext in extents
    @testset "Converting constructor: $ext(Meter(4.5))" begin
      @test ext(Meter(4.5)) ≈ Meter(4.5)
    end

    @testset "convert $ext(4.5) to Number" begin
      @test ext(4.5).value ≈ 4.5
      @test ext(4.5) ≈ 4.5
      # @test typeof(ext(4)) <: Int # it can't see around corners, that is it won't proactively make type conversions to make this true
    end
  end

  for extA in extents
    for extB in extents
      @testset "convert()ing between $extA and $extB" begin
        a = extA(Meter(1.2))
        b = extB(Meter(1.2))
        @test a ≈ b
        # @test extA(extB) ≈ Meter(1.2) # this requires the genericized version of # MilliMeter(x::T where T<:AbstractExtent) = convert(MilliMeter, x)
        # @test extB(extA) ≈ Meter(1.2)
      end
    end
  end

  @testset "things I want to do" begin
    @show typeof( Inch(1.0) / Inch(2.0) ) # == canceled units?
    @show typeof( Inch(1.0) * Foot(1.0) ) # == in^2 or ft^2?
  end


  # @testset "convert()" begin
  #   @test m ≈ 4.5
  #   @test 4.5 ≈ m
  #   @test mm ≈ 4500
  #   @test m ≈ mm
  #   @test UnitTypes.Meter(4.5) ≈ convert(UnitTypes.Meter, UnitTypes.MilliMeter(4500) )
  #   @test isapprox( UnitTypes.MilliMeter(4500), convert(UnitTypes.MilliMeter, UnitTypes.Meter(4.500) ), rtol=1e-3 )

  #   @test UnitTypes.Meter(4.5) ≈ UnitTypes.MilliMeter(4500)

  #   @test convert(UnitTypes.Meter, mm) ≈ m
  #   @test convert(UnitTypes.MilliMeter, m) ≈ mm

  #   @test UnitTypes.Inch(1.0) ≈ UnitTypes.MilliMeter(25.4)
  #   @test UnitTypes.Foot(1.0) ≈ UnitTypes.Inch(12.0)
  #   @test UnitTypes.Inch( UnitTypes.MilliMeter(25.4)) ≈ 1.0
  # end

end;