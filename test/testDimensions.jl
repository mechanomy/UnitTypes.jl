# note that every struct change requires restarting the kernel, and it's faster to $> julia test/runtests.jl than to hit f5
using Test
using UnitTypes

concepts = [UnitTypes.Diameter, UnitTypes.Radius, UnitTypes.Length, UnitTypes.Height, UnitTypes.Width, UnitTypes.Depth]
measures = [UnitTypes.Meter, UnitTypes.MilliMeter, UnitTypes.Inch, UnitTypes.Foot]
@testset "Dimensions" begin
  @testset "convert() Dimension to Measure " begin
    d = UnitTypes.Diameter{UnitTypes.Meter}(3.4)
    @test convert(UnitTypes.Meter, d) ≈ UnitTypes.Meter(3.4)
    @test convert(UnitTypes.MilliMeter, d) ≈ UnitTypes.MilliMeter(3400)
    @test isapprox( d, UnitTypes.Meter(3.4), rtol=1e-3 )
    @test isapprox( UnitTypes.Meter(3.4), d, rtol=1e-3 )
    
  end
  
  @testset "do not convert() Measure to Dimension" begin
    @test_throws ErrorException convert(UnitTypes.Diameter{UnitTypes.Meter}, UnitTypes.Meter(3.4))
  end

  @testset "convert() measure units within a concept" begin
    @test convert(UnitTypes.Diameter{UnitTypes.MilliMeter}, UnitTypes.Diameter(UnitTypes.Meter(3.4))) ≈ UnitTypes.MilliMeter(3400) # convert underlying Meter to MilliMeter
  end

  for C in concepts
    for M in measures
      @testset "checking $C{$M}" begin
        d = C{M}(3.4)
        @test typeof(d) <: UnitTypes.AbstractDimension
        @test typeof(d) <: C
        @test convert(Float64, d) ≈ 3.4
        

        @testset "Dimension + Number" begin
          @test isapprox( C{M}(1.2) + 0.1, 1.3, rtol=1e-3)
          @test isapprox( C{M}(1.2) - 0.1, 1.1, rtol=1e-3)
          @test isapprox( 0.1 - C{M}(1.2), -1.1, rtol=1e-3)
          @test isapprox( C{M}(1.2) * 0.1, 0.12, rtol=1e-3)
          @test isapprox( C{M}(1.2) / 0.1, 12, rtol=1e-3)
          @test isapprox( 0.1 / C{M}(1.2), 0.08333, rtol=1e-3)
        end
        @testset "Dimension + Dimension" begin
          @test isapprox( C{M}(1.2) + C{M}(0.1), C{M}(1.3), rtol=1e-3)
          @test isapprox( C{M}(1.2) - C{M}(0.1), C{M}(1.1), rtol=1e-3)
          @test isapprox( C{M}(1.2) * C{M}(0.1), C{M}(0.12), rtol=1e-3)
          @test isapprox( C{M}(1.2) / C{M}(0.1), C{M}(12), rtol=1e-3)
        end
      end
    end
  end

  @testset "DiameterRadius conversion" begin
    d = UnitTypes.Diameter(UnitTypes.Meter(3.4))
    @test convert(UnitTypes.Radius, d) ≈ UnitTypes.Meter(1.7)
    # @test UnitTypes.Diameter(UnitTypes.Meter(3.4)) ≈ UnitTypes.Radius(UnitTypes.Meter(1.7))

    r = UnitTypes.Radius(UnitTypes.Meter(1.7))
    @test convert(UnitTypes.Diameter, r) ≈ UnitTypes.Meter(3.4)
    
    @test convert(Diameter{Inch}, Diameter(MilliMeter(25.4))).value ≈ 1.0
    # @test convert(Inch, Diameter(MilliMeter(25.4))).value ≈ 1.0

    # down-convert Diameter to Length?
  # rod = convert(Float64,s.outside.value/2) # make a strip()?
 
    # @testset "things I want to do" begin
    #   @test isapprox( Inch(Foot(1.2)), Inch(14.4), atol=1e-3) #conversion by constructor
    # end
  end

end ;