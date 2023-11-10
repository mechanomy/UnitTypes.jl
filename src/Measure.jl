module Measure
  using TestItems 

  export AbstractMeasure, @makeMeasure, @makeBaseUnit, toBaseFloat, @unitProduct, @unitDivide
  abstract type AbstractMeasure end

  # `@makeMeasure MilliMeter "mm" 0.001 Meter` will create:
  # struct MilliMeter <: AbstractLength
  #   value::Number
  #   toBase::Number
  #   MilliMeter(x) = new(x, 1e-3)
  # end

  """
    referenceType: all Measures are convertible to a defining base type; lengths are defined relative to Meter.
    Now 
  """
  macro makeMeasure(name, unit, toBase, referenceType)
    return esc( 
      quote
        if Base.isconcretetype($referenceType)
          absName = supertype($referenceType) 
        else 
          absName = $referenceType
        end
        struct $name <: absName
          value::Number
          toBase::Number
          unit::String
          $name(x) = new(x,$toBase,$unit)
        end
        $name(x::T where T<:absName) = convert($name, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
        export $name
        # make an alias: Meter(x) == Length(x), where Length assumes Meter?
      end
    )
  end

  @testitem "Measure constructors" begin
    @makeMeasure TestMeasure "tm" 1.0 AbstractMeasure # Meter not defined yet, so make a temporary for testing
    @test typeof(TestMeasure(1.2)) <: AbstractMeasure
    @test typeof(TestMeasure(1.2)) <: TestMeasure

    @makeMeasure TestDerivedMeasure "tdm" 0.1 TestMeasure

    @test typeof(TestDerivedMeasure(1.2)) <: AbstractMeasure
    end

  #            desired           given                            how
  Base.convert(::Type{Int32},    x::T) where T<:AbstractMeasure = Int32(round(x.value)); #this might be too open-ended, maybe restrict to T{Int32}?
  Base.convert(::Type{Int64},    x::T) where T<:AbstractMeasure = Int64(round(x.value));
  Base.convert(::Type{T},   x::Number) where T<:AbstractMeasure = T(x)
  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractMeasure} = convert(T, x.value ) #convert first to Measure, then to number...this works fine in terminal

  @testitem "Measure convert to Number" begin
    @makeMeasure TestMeasure "tm" 1.0 AbstractMeasure
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

  # convert between measures?...this is wrong because it would allow converting between AbstractLength and AbstractAngle # Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value*x.toBase/T(1.0).toBase);

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

  @testitem "Measure operations" begin
    @makeMeasure TestMeasure "tm" 1.0 AbstractMeasure
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
  
  function toBaseFloat(m::T) where T<:AbstractMeasure
    return m.value * m.toBase
  end

  @testitem "Measure measure2string()" begin
    @makeMeasure TestMeasure "tm" 1.0 AbstractMeasure 
    @test UnitTypes.Measure.measure2String(TestMeasure(3.4)) == "3.4tm"
    @test string(TestMeasure(3.4)) == "3.4tm"
  end


  # for @makeBaseUnit Length Meter "m"
  # will make AbstractLength <: AbstractMeasure, Meter
  macro makeBaseUnit(quantityName, unitName, unitSymbol::String)
    abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength
    return esc(
      quote
        export $abstractName #AbstractLength
        abstract type $abstractName <: AbstractMeasure end

        @makeMeasure $unitName $unitSymbol 1.0 $abstractName

        Base.isequal(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = convert(T,x).value == convert(T,y).value
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
        Base.convert(::Type{T}, x::U) where {T<:$abstractName, U<:$abstractName} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...
      end
    )
  end
  

  # @unitProduct AbstractForce AbstractLength NewtonMeter
  # @unitProduct Newton Meter NewtonMeter
  # the commutivity of * allows permuation
  macro unitProduct(Abs1, Abs2, Operation)
    return esc(
      quote
        if Base.isconcretetype($Abs1)
          abs1 = supertype($Abs1)
        else
          abs1 = $Abs1
        end
        if Base.isconcretetype($Abs2)
          abs2 = supertype($Abs2)
        else
          abs2 = $Abs2
        end
        Base.:*(x::T, y::U) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(x) * toBaseFloat(y) )
        Base.:*(y::U, x::T) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(y) * toBaseFloat(x) )
      end
    )
  end

  # division is not commutive, a/b != b/a
  # Base.:/(x::T, y::U) where {T<:NewtonMeter, U<:Newton} = Meter( toBaseFloat(x) / toBaseFloat(y) )
  # Base.:/(x::T, y::U) where {T<:NewtonMeter, U<:Meter} = Newton( toBaseFloat(x) / toBaseFloat(y) )
  # @unitDivide NewtonMeter Newton Meter
  # @unitDivide NewtonMeter Meter Newton
  # Input / Divsior = Output
  macro unitDivide(Input, Divisor, Output)
    return esc(
      quote
        # Base.:/(x::T, y::U) where {T<:$Input, U<:$Divisor} = $Output( toBaseFloat(x) / toBaseFloat(y) )
        absInput = supertype($Input)
        absDivisor = supertype($Divisor)
        Base.:/(x::T, y::U) where {T<:absInput, U<:absDivisor} = $Output( toBaseFloat(x) / toBaseFloat(y) )
      end
    )
  end


end


