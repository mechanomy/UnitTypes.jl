using Test
using UnitTypes

# measures = [ UnitTypes.Meter ]
measures = [ UnitTypes.Meter, UnitTypes.MilliMeter, UnitTypes.Inch, UnitTypes.Foot, UnitTypes.Radian, UnitTypes.Degree]

@testset "AbsMeasures" begin

  for M in measures
    @testset "constructing $M" begin
      @test typeof(M(1.2)) <: AbstractMeasure
      @test typeof(M(1.2)) <: M
      @show typeof(M(1.23))
    end

    @testset "convert()ing $M into Float64, Int32, Int64" begin
      a = convert(Float64, M(3.4) )
      @test typeof(a) == Float64
      @test a == 3.4

      b = convert(Int32, M(3.4) )
      @test typeof(b) == Int32
      @test b == 3

      c = convert(Int64, M(3.4) )
      @test typeof(c) == Int64
      @test c == 3

      # @test UnitTypes.measure2string(M(3.4) == 'ft' || 'in' ..
    end


    @testset "$M +-*/ Number" begin
      @test typeof(M(1.2)+0.1) == M
      @test typeof(0.1 + M(1.2)) == M
      @test isapprox(M(1.2)+0.1, M(1.3), rtol=1e-3)
      @test isapprox(0.1+M(1.2), M(1.3), rtol=1e-3)

      @test isapprox(M(1.2)-0.1, M(1.1), rtol=1e-3)
      @test isapprox(0.1-M(1.2), M(-1.1), rtol=1e-3)

      @test isapprox(M(1.2)*0.1, M(0.12), rtol=1e-3)
      @test isapprox(0.1*M(1.2), M(0.12), rtol=1e-3)

      @test isapprox(M(1.2)/0.1, M(12), rtol=1e-3)
      @test isapprox(0.1/M(1.2), M(0.08333), rtol=1e-3)
    end

    @testset "$M +-*/ $M" begin
      @test isapprox(M(1.2)+M(0.1), M(1.3), rtol=1e-3)
      @test isapprox(M(1.2)-M(0.1), M(1.1), rtol=1e-3)
      @test isapprox(M(1.2)*M(0.1), M(0.12), rtol=1e-3)
      @test isapprox(M(1.2)/M(0.1), M(12), rtol=1e-3)
    end
  end

  # # add print functions

  # #vectorize these when structs have converstions to base

end;