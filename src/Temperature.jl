#temperature: https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature

@makeBaseMeasure Temperature Kelvin "°K"
@makeMeasure Kelvin = Celsius "°C" x->x+273.15 x->x-273.15
@testitem "Celsius" begin
  @test isapprox( convert(Celsius, Kelvin(273.15)), Celsius(0), atol=1e-3)
  @test isapprox( convert(Kelvin, Celsius(0)), Kelvin(273.15), atol=1e-3)
end

@makeMeasure Kelvin = Fahrenheit "°F" x->(x+459.67)*5/9 x->x*9/5-459.67
@testitem "Fahrenheit" begin
  @test isapprox(convert(Kelvin, Fahrenheit(32)), Kelvin(273.15), atol=1e-3)
  @test isapprox(convert(Fahrenheit, Kelvin(273.15)), Fahrenheit(32), atol=1e-3)
  @test isapprox(convert(Celsius, Fahrenheit(32)), Celsius(0), atol=1e-3)
end

@makeMeasure Kelvin = Rankine "°R" x->x*5/9 x->x*9/5
@testitem "Rankine" begin
  @test isapprox(Rankine(491.67), Kelvin(273.15), atol=1e-3)
  @test isapprox(Kelvin(273.15), Rankine(491.67), atol=1e-3)
  @test isapprox(Rankine(491.67), Fahrenheit(32), atol=1e-3)
end

@testitem "comparison between units" begin
  @test Celsius(0) ≈ Kelvin(273.15)
  @test Celsius(0) ≈ Fahrenheit(32)
  @test isapprox( Fahrenheit(32), Kelvin(273.15), atol=1e-3)
  @test isapprox( Rankine(491.67), Kelvin(273.15), atol=1e-3)
  @test Fahrenheit(0) <= Celsius(0)
  @test Fahrenheit(0) < Celsius(0)
  @test Fahrenheit(40) > Celsius(0)
  @test Fahrenheit(40) >= Celsius(0)
end

@testitem "addition subtraction" begin
  @test isapprox(Kelvin(200) + Kelvin(100), Kelvin(300), atol=1e-3)
  @test isapprox(Kelvin(200) - Kelvin(100), Kelvin(100), atol=1e-3)

  @test isapprox(Celsius(0) + Celsius(10), Celsius(10), atol=1e-3)
  @test isapprox(Celsius(0) - Celsius(10), Celsius(-10), atol=1e-3)

  @test isapprox(Fahrenheit(32) + Fahrenheit(10), Fahrenheit(42), atol=1e-3)
  @test isapprox(Fahrenheit(32) - Fahrenheit(10), Fahrenheit(22), atol=1e-3)

  @test isapprox(Rankine(500) + Rankine(10), Rankine(510), atol=1e-3)
  @test isapprox(Rankine(500) - Rankine(10), Rankine(490), atol=1e-3)
end

@testitem "temperature u_str" begin
  @test 1u"°K" isa Kelvin
  @test 1u"°C" isa Celsius
  @test 1u"°R" isa Rankine
  @test 1u"°F" isa Fahrenheit
end

