#temperature: https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature

@makeBaseMeasure Temperature Kelvin "°K" true
Base.:*(x::T, y::U) where {T<:AbstractTemperature, U<:Number} = T(x.value*y) 
Base.:*(x::T, y::U) where {T<:Number, U<:AbstractTemperature} = U(x*y.value)

@makeMeasure Kelvin(0) = Celsius(0) "°C" false # false ignores the factors
Base.convert(::Type{Celsius}, x::Kelvin) = Celsius(x.value - 273.15) # from base
Base.convert(::Type{Kelvin}, x::Celsius) = Kelvin(x.value + 273.15) # to base
Base.convert(::Type{T}, x::Celsius) where T<:AbstractTemperature = T(convert(Kelvin,x)) # between sibling temperatures

@makeMeasure Kelvin(0) = Fahrenheit(0) "°F" false
Base.convert(::Type{Fahrenheit}, x::Kelvin) = Fahrenheit(x.value*9/5 - 459.67) # from base
Base.convert(::Type{Kelvin}, x::Fahrenheit) = Kelvin((x.value+459.67)*5/9) # to base
Base.convert(::Type{T}, x::Fahrenheit) where T<:AbstractTemperature = T(convert(Kelvin,x)) # between sibling temperatures

@makeMeasure Kelvin(0) = Rankine(0) "°R" false
Base.convert(::Type{Rankine}, x::Kelvin) = Rankine(x.value*9/5) # from base
Base.convert(::Type{Kelvin}, x::Rankine) = Kelvin(x.value*5/9) # to base
Base.convert(::Type{T}, x::Rankine) where T<:AbstractTemperature = T(convert(Kelvin,x)) # between sibling temperatures

# temperature is an affine unit, we have to take extra care on +-*/
Base.:+(k1::Kelvin, k2::Kelvin) = Kelvin(k1.value+k2.value)
Base.:-(k1::Kelvin, k2::Kelvin) = Kelvin(k1.value-k2.value) # guard against negative Kelvin? 

Base.:+(c1::Celsius, c2::Celsius) = Celsius(c1.value + c2.value)
Base.:-(c1::Celsius, c2::Celsius) = Celsius(c1.value - c2.value)

Base.:+(f1::Fahrenheit, f2::Fahrenheit) = Fahrenheit(f1.value + f2.value)
Base.:-(f1::Fahrenheit, f2::Fahrenheit) = Fahrenheit(f1.value - f2.value)

Base.:+(k::Kelvin, c::Celsius) = Kelvin(k.value + c.value) # omit c's 273.15 offset
Base.:-(k::Kelvin, c::Celsius) = Kelvin(k.value - c.value) # omit c's 273.15 offset
Base.:+(c::Celsius, k::Kelvin) = Kelvin(k.value + c.value) # omit c's 273.15 offset
Base.:-(c::Celsius, k::Kelvin) = Kelvin(k.value - c.value) # omit c's 273.15 offset
Base.:+(c::Celsius, f::Fahrenheit) = Celsius(c.value + Celsius(f).value)
Base.:-(c::Celsius, f::Fahrenheit) = Celsius(c.value - Celsius(f).value)
Base.:+(f::Fahrenheit, c::Celsius) = Fahrenheit(f.value + Fahrenheit(c).value)
Base.:-(f::Fahrenheit, c::Celsius) = Fahrenheit(f.value - Fahrenheit(c).value)

@testitem "temperature scales" begin
  @test convert(Celsius, Kelvin(273.15)) ≈ Celsius(0)
  @test convert(Kelvin, Celsius(0)) ≈ Kelvin(273.15)

  @test convert(Kelvin, Fahrenheit(32)) ≈ Kelvin(273.15)
  @test convert(Fahrenheit, Kelvin(273.15)) ≈ Fahrenheit(32)
  @test convert(Celsius, Fahrenheit(32)) ≈ Celsius(0)

  @test 1u"°K" isa Kelvin
  @test 1u"°C" isa Celsius
  @test 1u"°R" isa Rankine
  @test 1u"°F" isa Fahrenheit

  @testset "comparison between units" begin
    @test Celsius(0) ≈ Kelvin(273.15)
    @test Celsius(0) ≈ Fahrenheit(32)
    @test Fahrenheit(32) ≈ Kelvin(273.15)
    @test Rankine(491.67) ≈ Kelvin(273.15)
    @test Fahrenheit(0) <= Celsius(0)
    @test Fahrenheit(0) < Celsius(0)
    @test Fahrenheit(40) > Celsius(0)
    @test Fahrenheit(40) >= Celsius(0)
  end

  @testset "addition subtraction" begin
    @test Kelvin(200) + Kelvin(100) ≈ Kelvin(300)
    @test Kelvin(200) - Kelvin(100) ≈ Kelvin(100)
    @test Kelvin(200) + Celsius(10) ≈ Kelvin(210)
    @test Celsius(10) + Kelvin(200) ≈ Kelvin(210)

    @test Celsius(0) + Celsius(10) ≈ Celsius(10)
    @test Celsius(0) - Celsius(10) ≈ Celsius(-10)
    @test Fahrenheit(32) + Fahrenheit(10) ≈ Fahrenheit(42)
    @test Fahrenheit(32) - Fahrenheit(10) ≈ Fahrenheit(22)

    #not intuitive:
    @test isapprox(Celsius(0) + Fahrenheit(10), Celsius(-12.2222), atol=1e-3) 
    @test isapprox(Celsius(0) - Fahrenheit(10), Celsius(+12.2222), atol=1e-3) 
    @test isapprox(Fahrenheit(32)+Celsius(0), Fahrenheit(64), atol=1e-3)
    @test isapprox(Fahrenheit(32)-Celsius(0), Fahrenheit(0), atol=1e-3)
  end
end