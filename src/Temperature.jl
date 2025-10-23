#temperature: https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature

@makeBaseMeasure Temperature Kelvin "°K" true
@makeMeasure Kelvin(0) = Celsius(0) "°C" true # false ignores the factors
Base.convert(::Type{Celsius}, x::Kelvin) = Celsius(x.value - 273.15) # from base
Base.convert(::Type{Kelvin}, x::Celsius) = Kelvin(x.value + 273.15) # to base
Base.isapprox(x::Kelvin, y::Kelvin; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Celsius, y::Celsius; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Kelvin, y::Celsius; atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert(Kelvin,y).value, atol=atol, rtol=rtol)
Base.isapprox(x::Celsius, y::Kelvin; atol::Real=0, rtol::Real=atol) = isapprox( convert(Kelvin,x).value, y.value, atol=atol, rtol=rtol)

@testitem "Celsius" begin
  @test isapprox( convert(Celsius, Kelvin(273.15)), Celsius(0), atol=1e-3)
  @test isapprox( convert(Kelvin, Celsius(0)), Kelvin(273.15), atol=1e-3)
end

@makeMeasure Kelvin(0) = Fahrenheit(0) "°F" true
Base.convert(::Type{Fahrenheit}, x::Kelvin) = Fahrenheit(x.value*9/5 - 459.67) # from base
Base.convert(::Type{Kelvin}, x::Fahrenheit) = Kelvin((x.value+459.67)*5/9) # to base
# Base.convert(::Type{T}, x::Fahrenheit) where T<:AbstractTemperature = T(Kelvin(x)) # between sibling temperatures ... this is giving a NaN, not sure why
Base.convert(::Type{Celsius}, x::Fahrenheit) = Celsius((x.value-32)*9/5) 
Base.convert(::Type{Fahrenheit}, x::Celsius) = Fahrenheit(x.value*5/9+32)
Base.isapprox(x::Fahrenheit, y::Fahrenheit; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Fahrenheit, y::Kelvin; atol::Real=0, rtol::Real=atol) = isapprox( convert(Kelvin,x).value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Kelvin, y::Fahrenheit; atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert(Kelvin,y).value, atol=atol, rtol=rtol)

@testitem "Fahrenheit" begin
  @test isapprox(convert(Kelvin, Fahrenheit(32)), Kelvin(273.15), atol=1e-3)
  @test isapprox(convert(Fahrenheit, Kelvin(273.15)), Fahrenheit(32), atol=1e-3)
  @test isapprox(convert(Celsius, Fahrenheit(32)), Celsius(0), atol=1e-3)
end

@makeMeasure Kelvin(0) = Rankine(0) "°R" true
Base.convert(::Type{Rankine}, x::Kelvin) = Rankine(x.value*9/5) # from base
Base.convert(::Type{Kelvin}, x::Rankine) = Kelvin(x.value*5/9) # to base
Base.convert(::Type{Rankine}, x::Fahrenheit) = Rankine(x.value-459.67) 
Base.convert(::Type{Fahrenheit}, x::Rankine) = Fahrenheit(x+459.67) 
Base.isapprox(x::Rankine, y::Rankine; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Rankine, y::Kelvin; atol::Real=0, rtol::Real=atol) = isapprox( convert(Kelvin,x).value, y.value, atol=atol, rtol=rtol)
Base.isapprox(x::Kelvin, y::Rankine; atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert(Kelvin,y).value, atol=atol, rtol=rtol)
Base.isapprox(x::Fahrenheit, y::Rankine; atol::Real=0, rtol::Real=atol) = isapprox( convert(Kelvin,x).value, convert(Kelvin,y).value, atol=atol, rtol=rtol)
Base.isapprox(x::Rankine, y::Fahrenheit; atol::Real=0, rtol::Real=atol) = isapprox( convert(Kelvin,x).value, convert(Kelvin,y).value, atol=atol, rtol=rtol)

@testitem "Rankine" begin
  @test isapprox(Rankine(491.67), Kelvin(273.15), atol=1e-3)
  @test isapprox(Kelvin(273.15), Rankine(491.67), atol=1e-3)
  @test isapprox(Rankine(491.67), Fahrenheit(32), atol=1e-3)
end

@testitem "comparison between units" begin
  @test Celsius(0) ≈ Kelvin(273.15)
  @test Celsius(0) ≈ Fahrenheit(32)
  @test Fahrenheit(32) ≈ Kelvin(273.15)
  @test Rankine(491.67) ≈ Kelvin(273.15)
  @test Fahrenheit(0) <= Celsius(0)
  @test Fahrenheit(0) < Celsius(0)
  @test Fahrenheit(40) > Celsius(0)
  @test Fahrenheit(40) >= Celsius(0)
end

# # temperature is an affine unit, we have to take extra care on +-*/
Base.:*(x::T, y::U) where {T<:AbstractTemperature, U<:Number} = T(x.value*y) 
Base.:*(x::T, y::U) where {T<:Number, U<:AbstractTemperature} = U(x*y.value)

Base.:+(k1::Kelvin, k2::Kelvin) = Kelvin(k1.value+k2.value)
Base.:-(k1::Kelvin, k2::Kelvin) = Kelvin(k1.value-k2.value) # guard against negative Kelvin? 

Base.:+(c1::Celsius, c2::Celsius) = Celsius(c1.value + c2.value)
Base.:-(c1::Celsius, c2::Celsius) = Celsius(c1.value - c2.value)

Base.:+(f1::Fahrenheit, f2::Fahrenheit) = Fahrenheit(f1.value + f2.value)
Base.:-(f1::Fahrenheit, f2::Fahrenheit) = Fahrenheit(f1.value - f2.value)

Base.:+(r1::Rankine, r2::Rankine) = Rankine(r1.value + r2.value)
Base.:-(r1::Rankine, r2::Rankine) = Rankine(r1.value - r2.value)

Base.:+(k::Kelvin, c::Celsius) = Kelvin(k.value + c.value) # omit c's 273.15 offset
Base.:-(k::Kelvin, c::Celsius) = Kelvin(k.value - c.value)
Base.:+(c::Celsius, k::Kelvin) = Kelvin(k.value + c.value)
Base.:-(c::Celsius, k::Kelvin) = Kelvin(k.value - c.value)

@testitem "addition subtraction" begin
  @test Kelvin(200) + Kelvin(100) ≈ Kelvin(300)
  @test Kelvin(200) - Kelvin(100) ≈ Kelvin(100)
  @test Kelvin(200) + Celsius(10) ≈ Kelvin(210)
  @test Celsius(10) + Kelvin(200) ≈ Kelvin(210)

  @test Celsius(0) + Celsius(10) ≈ Celsius(10)
  @test Celsius(0) - Celsius(10) ≈ Celsius(-10)

  @test Fahrenheit(32) + Fahrenheit(10) ≈ Fahrenheit(42)
  @test Fahrenheit(32) - Fahrenheit(10) ≈ Fahrenheit(22)

  @test Rankine(500) + Rankine(10) ≈ Rankine(510)
  @test Rankine(500) - Rankine(10) ≈ Rankine(490)
end


# cross-unit addition substraction seems poorly defined, dangerous?
# Base.:+(c::Celsius, f::Fahrenheit)::Celsius = c + convert(Celsius,f)
# Base.:-(c::Celsius, f::Fahrenheit)::Celsius = c - convert(Celsius,f)
# Base.:+(f::Fahrenheit, c::Celsius)::Fahrenheit = f + convert(Fahrenheit,c)
# Base.:-(f::Fahrenheit, c::Celsius)::Fahrenheit = f - convert(Fahrenheit,c)

# Base.:+(f::Fahrenheit, r::Rankine)::Fahrenheit = Fahrenheit(f.value + r.value-459.67)
# Base.:+(f::Fahrenheit, r::Rankine)::Rankine = Rankine(f.value+459.67 + r.value)
# Base.:-(f::Fahrenheit, r::Rankine)::Fahrenheit = Fahrenheit(f.value - r.value-459.67)
# Base.:-(f::Fahrenheit, r::Rankine)::Rankine = Rankine(f.value+459.67 - r.value)
# Base.:-(r::Rankine, f::Fahrenheit)::Fahrenheit = Fahrenheit(r.value-459.67 - f.value)
# Base.:-(r::Rankine, f::Fahrenheit)::Rankine = Rankine(r.value - (f.value+459.67))
# Base.:+(f::Fahrenheit, r::Rankine)::Fahrenheit = Fahrenheit(f.value + r.value-459.67) 
# Base.:+(f::Fahrenheit, r::Rankine)::Rankine = Rankine(f.value + r.value) 

# @testitem "cross-unit addition subtraction" begin
  # @test Rankine(500) - Fahrenheit(10) ≈ Rankine(490)
  # @test Fahrenheit(32) + Rankine(10) ≈ Fahrenheit(42)

  #not intuitive:
  # @test isapprox(Celsius(0) + Fahrenheit(10), Celsius(-12.2222), atol=1e-3) 
  # @test isapprox(Celsius(0) - Fahrenheit(10), Celsius(+12.2222), atol=1e-3) 
  # @test isapprox(Fahrenheit(32)+Celsius(0), Fahrenheit(64), atol=1e-3)
  # @test isapprox(Fahrenheit(32)-Celsius(0), Fahrenheit(0), atol=1e-3)
# end

@testitem "temperature u_str" begin
  @test 1u"°K" isa Kelvin
  @test 1u"°C" isa Celsius
  @test 1u"°R" isa Rankine
  @test 1u"°F" isa Fahrenheit
end

