# https://en.wikipedia.org/wiki/Centimetre%E2%80%93gram%E2%80%93second_system_of_units

@makeMeasure 1e-2 MeterPerSecond2 = 1 Galileo "Galileo"

@makeMeasure 1e-5 Newton = 1 Dyne "dyn"

@makeMeasure 1e-7 Joule = 1 Erg "erg"

@makeMeasure 0.1 Pascal = 1 Barye "Ba"

@makeMeasure 0.1 PascalSecond = 1 Poise "P"

@makeMeasure 1e-4 MeterSquaredPerSecond = 1 Stokes "St"

@makeMeasure 1e-4 Tesla = 1 Gauss "Gauss"

@makeMeasure 1000/(4*pi) APerM = 1 Oersted "Oe"

@makeMeasure 1e-8 Weber = 1 Maxwell "Mx"

@testitem "CGS" begin
  @test isapprox(Galileo(100), MeterPerSecond2(1), atol=1e-10)
  @test isapprox(Dyne(1e5), Newton(1), atol=1e-3)
  @test isapprox(Erg(1e7), Joule(1), atol=1e-3)
  @test isapprox(Barye(10), Pascal(1), atol=1e-10)
  @test isapprox(Poise(10), PascalSecond(1), atol=1e-10)
  @test isapprox(Stokes(1e4), MeterSquaredPerSecond(1), atol=1e-10)
  @test isapprox(Gauss(1e4), Tesla(1), atol=1e-10)
  @test isapprox(Maxwell(1e8), Weber(1), atol=1e-3)
end
