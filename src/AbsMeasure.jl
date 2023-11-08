module AbsMeasure
  using TestItems 

  export AbstractMeasure, @makeMeasureFromAbstract, @makeMeasure#, measure2String
  abstract type AbstractMeasure end

  # `@makeMeasure MilliMeter` will create:
  # struct MilliMeter <: AbstractExtent
  #   value::Number
  #   toBase::Number
  #   MilliMeter(x) = new(x, 1e-3)
  # end


  # if baseUnit is abstract type ...

  # https://docs.julialang.org/en/v1/manual/metaprogramming/#Code-Generation
  macro makeMeasureFromAbstract(name, unit, toBase, abstractType)
    esc(
      quote
        struct $name <: $abstractType
          value::Number
          toBase::Number
          unit::String
          $name(x) = new(x,$toBase,$unit)
        end
        $name(x::T where T<:$abstractType) = convert($name, x) # conversion by constructor: Inch(x::T where T<:AbstractExtent) = convert(Inch, x)
      end
    )
  end

  macro makeMeasure(name, unit, toBase, baseUnit)
    esc( 
      quote
        absName = supertype($baseUnit)
        @makeMeasureFromAbstract $name $unit $toBase absName
      end
    )
  end

  @testitem "AbsMeasure constructors" begin
    @makeMeasureFromAbstract TestMeasure "tm" 1.0 AbstractMeasure # Meter not defined yet, so make a temporary for testing
    @test typeof(TestMeasure(1.2)) <: AbstractMeasure
    @test typeof(TestMeasure(1.2)) <: TestMeasure

    @makeMeasure TestDerivedMeasure "tdm" 0.1 TestMeasure

    @test typeof(TestDerivedMeasure(1.2)) <: AbstractMeasure
    @test typeof(TestDerivedMeasure(1.2)) <: TestDerivedMeasure
  end

  #            desired           given                            how
  Base.convert(::Type{Int32},    x::T) where T<:AbstractMeasure = Int32(round(x.value));
  Base.convert(::Type{Int64},    x::T) where T<:AbstractMeasure = Int64(round(x.value));
  # Base.convert(::Type{Float64},  x::T) where T<:AbstractMeasure = Float64(x.value);
  # Base.convert(::Type{Number},   x::T) where T<:AbstractMeasure = x.value;
  Base.convert(::Type{T},   x::Number) where T<:AbstractMeasure = T(x)::T
  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractMeasure} = convert(T, x.value )::T #convert first to Measure, then to number...this works fine in terminal

  @testitem "AbsMeasure convert to Number" begin
    @makeMeasureFromAbstract TestMeasure "tm" 1.0 AbstractMeasure
    a = convert(Float64, TestMeasure(3.4) )
    @test isa(a, Float64)
    @test a == 3.4

    b = convert(Int32, TestMeasure(3.4) )
    @test isa(b, Int32)
    @test b == 3

    c = convert(Int64, TestMeasure(3.4) )
    @test isa(c, Int64)
    @test c == 3
  end

  # convert between measures...I think this is wrong because it would allow converting between AbstractExtent and AbstractAngle
  Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works...
  @testitem "AbsMeasure convert between Measures" begin
    @makeMeasureFromAbstract TestMeasure "tm" 1.0 AbstractMeasure
    @makeMeasure TestMeasureA "tma" 1.0  TestMeasure
    @makeMeasure TestMeasureB "tmb" 10.0 TestMeasure
    @test convert(TestMeasureA,TestMeasureB(1.2)) ≈ TestMeasureA(12.0)
  end

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {U<:AbstractMeasure, T<:Number} = isapprox(x, y.value, atol=atol, rtol=rtol) #I expected this to be implied by prior..
  Base.isapprox(x::T, y::T; atol::Real=0, rtol::Real=atol) where T<:AbstractMeasure = isapprox(x.value, y.value, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:AbstractMeasure} = isapprox(x.value, convert(T,y), atol=atol, rtol=rtol)

  # argument order matters, need to doubly define everyting:
  Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x + T(y)
  Base.:+(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) + x
  Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value+convert(T,y).value) #result returned in the unit of the first measure
  # Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = +(promote(x,y)) #this fails, probably need to make a simpler demo https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion

  Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x - T(y)
  Base.:-(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) - x
  Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value-convert(T,y).value)

  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x * T(y)
  Base.:*(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) * x
  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value*convert(T,y).value)

  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x / T(y)
  Base.:/(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) / x
  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value/convert(T,y).value)

  @testitem "AbsMeasure operations" begin
    @makeMeasureFromAbstract TestMeasure "tm" 1.0 AbstractMeasure
    @testset "Measure +-*/ Number" begin
      @test isa(TestMeasure(1.2)+0.1, TestMeasure)
      @test isa(0.1 + TestMeasure(1.2), TestMeasure)
      @test isapprox(TestMeasure(1.2)+0.1, TestMeasure(1.3), rtol=1e-3)
      @test isapprox(0.1+TestMeasure(1.2), TestMeasure(1.3), rtol=1e-3)

      @test isapprox(TestMeasure(1.2)-0.1, TestMeasure(1.1), rtol=1e-3)
      @test isapprox(0.1-TestMeasure(1.2), TestMeasure(-1.1), rtol=1e-3)

      @test isapprox(TestMeasure(1.2)*0.1, TestMeasure(0.12), rtol=1e-3)
      @test isapprox(0.1*TestMeasure(1.2), TestMeasure(0.12), rtol=1e-3)

      @test isapprox(TestMeasure(1.2)/0.1, TestMeasure(12), rtol=1e-3)
      @test isapprox(0.1/TestMeasure(1.2), TestMeasure(0.08333), rtol=1e-3)
    end
    @testset "Measure +-*/ Measure" begin
      @test isapprox(TestMeasure(1.2)+TestMeasure(0.1), TestMeasure(1.3), rtol=1e-3)
      @test isapprox(TestMeasure(1.2)-TestMeasure(0.1), TestMeasure(1.1), rtol=1e-3)
      @test isapprox(TestMeasure(1.2)*TestMeasure(0.1), TestMeasure(0.12), rtol=1e-3)
      @test isapprox(TestMeasure(1.2)/TestMeasure(0.1), TestMeasure(12), rtol=1e-3)
    end
  end

  """
  """
  function measure2String(m::T)::String where T<:AbstractMeasure
    # return @sprintf("%3.3f []", m)
    return "$(m.value)$(m.unit)"
  end
  function Base.show(io::IO, m::T) where T<:AbstractMeasure
    print(io, measure2String(m))
  end

  @testitem "AbsMeasure measure2string()" begin
    @makeMeasureFromAbstract TestMeasure "tm" 1.0 AbstractMeasure 
    @test UnitTypes.AbsMeasure.measure2String(TestMeasure(3.4)) == "3.4tm"
    @test string(TestMeasure(3.4)) == "3.4tm"
  end



end


