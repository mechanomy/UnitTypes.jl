using Test
using UnitTypes

angles = [UnitTypes.Radian, UnitTypes.Degree]

@testset "test AbsAngle" begin
  @testset "converting radian and degree" begin
    @test isapprox( convert(UnitTypes.Degree, UnitTypes.Radian(π)), 180, rtol=1e-3 )
    @test isapprox( convert(UnitTypes.Radian, UnitTypes.Degree(180.0)), π, rtol=1e-3)
    @test isapprox( UnitTypes.Radian(10.0), UnitTypes.Degree(572.958), rtol=1e-3)

    #promote rule:
    @test promote_type(Radian, Degree) == Radian
    # @test typeof(Radian(3)+Degree(4)) == Radian #these fail b/c of the preferential +, etc., need to use promote instead?
    # @test typeof(Degree(4)+Radian(3)) == Radian

    @test isapprox( Radian(Degree(57.296)), 1, atol=1e-3)
  end

  @testset "plays nice with Base math" begin
    @test isapprox( convert(Float64, UnitTypes.Radian(π)), π, atol=1e-3)

    @test isapprox( sin(UnitTypes.Radian(π/4)), √2/2, atol=1e-3 )
    @test isapprox( sin(UnitTypes.Degree(45)), √2/2, atol=1e-3)
    @test isapprox( cos(UnitTypes.Radian(π/4)), √2/2, atol=1e-3 )
    @test isapprox( cos(UnitTypes.Degree(45)), √2/2, atol=1e-3)
    @test isapprox( tan(UnitTypes.Radian(π/4)), 1, atol=1e-3 )
    @test isapprox( tan(UnitTypes.Degree(45)), 1, atol=1e-3)
    
    # I don't seem to be able to get a UnitType out of this call..perhaps because the ::Radian is only taken as a hint?
    # @test isapprox( asin(√2/2), UnitTypes.Radian(π/4), atol=1e-3 )
    # @test isapprox( asin(√2/2), UnitTypes.Degree(45), atol=1e-3 ) #I think it should see that the result of asin needs to be in Degree...
    # @test isapprox( asin(√2/2) + UnitTypes.Degree(45), 90, atol=1e-3 ) 
  end
end;