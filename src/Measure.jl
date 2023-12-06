module Measure
  using DocStringExtensions
  using TestItems 
  import Unitful

  export AbstractMeasure, @makeDerivedMeasure, @makeBaseMeasure, toBaseFloat, @unitProduct, @unitDivide, @addUnitOperations
  abstract type AbstractMeasure end

  """
    $TYPEDSIGNATURES

    Make a new base measure which has no multiplicative relationship to an existing unit.
    For example, in `@makeBaseMeasure Length Meter "m"`:
    * `quantityName` is the name of the measure, 'Length' above.
    * `unitName` is the name of the unit which will be used to make measures bearing that unit, 'Meter' above.
    * `unitSymbol` is the abbreviation of the unit name, used in all string presentations of the measure.
    The macro will introduce `AbstractLength <: AbstractMeasure` and `Meter()` into the current scope.
    
    Measures created by the macro have fields:
    * `value::Number` raw value of the measure
    * `toBase::Number` == 1 for base measures
    * `unit::String` the unit to be displayed

    See also [toBaseFloat()](toBaseFloat).
  """
  macro makeBaseMeasure(quantityName, unitName, unitSymbol::String)
    # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName unitSymbol:$unitSymbol")
    abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength
    return esc(
      quote
        export $abstractName #AbstractLength
        abstract type $abstractName <: AbstractMeasure end

        @makeDerivedMeasure $unitName $unitSymbol 1.0 $abstractName

        Base.isequal(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = convert(T,x).value == convert(T,y).value
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
        # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units
        Base.convert(::Type{T}, x::U) where {T<:$abstractName, U<:$abstractName} = T(x.value*x.toBase/T(1.0).toBase); #...this is janky but works to get the destination's toBase...
      end
    )
  end
  @testitem "makeBaseMeasure" begin
    @makeBaseMeasure TestMBU TMBU "tmbu" #this makes it in the TestItemRunner context, not Main
    mod = Measure.@returnModuleName
    # @show names(mod)
    @test isdefined(mod, :AbstractTestMBU)
    @test isdefined(mod, :TMBU)
  end

  """
    $TYPEDSIGNATURES

    Makes a new Measure derived from some other Measure.
    For example, `@makeDerivedMeasure Centimeter "cm" 0.01 Meter` defines the Centimeter as 0.01 of a Meter
    * `name` - the name of the derived unit
    * `unit` - the derived unit's abbreviation or symbol
    * `toBase` - the numeric factor relating the derived unit to the `referenceType`, here `1cm = 0.01m`
    * `referenceType` - the base type

    Composite units are defined similarly, first by the base measure 
    `@makeBaseMeasure Torque NewtonMeter "N*m"`
    and then derivations
    `@makeDerivedMeasure NewtonMillimeter "N*mm" 1e-3 NewtonMeter`
    `@makeDerivedMeasure MilliNewtonMeter "mN*m" 1e-3 NewtonMeter`
    and then the composition
    `@addUnitOperations Newton Meter NewtonMeter`


  """
  macro makeDerivedMeasure(name, unit, toBase, referenceType)
    # println("makeDerivedMeasure( $name :: $(typeof(name)), $unit :: $(typeof(unit)) )")
    return esc( 
      quote
        if Base.isconcretetype($referenceType)
          absName = supertype($referenceType) 
        else 
          absName = $referenceType
        end

        """
          This type represents units of $($name).
        """
        struct $name <: absName
          value::Number
          toBase::Number
          unit::String
          $name(x) = new(x,$toBase,$unit)
        end
        $name(x::T where T<:absName) = convert($name, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
        # $name(uf::T) where T<:Unitful.AbstractQuantity = convert($name, uf) # add a constructor for Unitful units to prevent struct.value = Unitful...this requires that all modules that use @makeDerived also import Unitful, commenting out for now...
        $name(uf::T) where T<:UnitTypes.Measure.Unitful.AbstractQuantity = convert($name, uf) 
        export $name

        # Base.:*(x::T, y::U) where {T<:absName, U<:absName} = Name2(x*y)  # this requires $name^2 to exist
      end
    )
  end

  @testitem "Measure constructors" begin
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure # Meter not defined yet, so make a temporary for testing
    @test typeof(TestMeasure(1.2)) <: AbstractMeasure
    @test typeof(TestMeasure(1.2)) <: TestMeasure

    @makeDerivedMeasure TestDerivedMeasure "tdm" 0.1 TestMeasure

    @test typeof(TestDerivedMeasure(1.2)) <: AbstractMeasure
  end

  #            desired           given                            how
  Base.convert(::Type{Int32},    x::T) where T<:AbstractMeasure = Int32(round(x.value)); #this might be too open-ended, maybe restrict to T{Int32}?
  Base.convert(::Type{Int64},    x::T) where T<:AbstractMeasure = Int64(round(x.value));
  Base.convert(::Type{T},   x::Number) where T<:AbstractMeasure = T(x)
  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractMeasure} = convert(T, x.value ) #convert first to Measure, then to number...this works fine in terminal
  @testitem "Measure convert to Number" begin
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure
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

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:AbstractMeasure} = isapprox(x.value, convert(T,y).value, atol=atol, rtol=rtol)
  Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T(x.value+convert(T,y).value) #result returned in the unit of the first measure
  # Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = +(promote(x,y)) #this fails, probably need to make a simpler demo https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion
  Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value-convert(T,y).value)
  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value*convert(T,y).value)
  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = T( x.value/convert(T,y).value)
  Base.:<(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure} = x.value < convert(T,y).value # other <> ops are defined from this
  @testitem "Measure +-*/ Measure" begin
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure
    @test isapprox(TestMeasure(1.2)+TestMeasure(0.1), TestMeasure(1.3), rtol=1e-3)
    @test isapprox(TestMeasure(1.2)-TestMeasure(0.1), TestMeasure(1.1), rtol=1e-3)
    @test TestMeasure(1.2) < TestMeasure(3.4)
    @test TestMeasure(1.2) <= TestMeasure(3.4)
    @test TestMeasure(3.4) > TestMeasure(1.2)
    @test TestMeasure(3.4) >= TestMeasure(1.2)
    @test (TestMeasure(3.4) == TestMeasure(1.2)) == false
    @test (TestMeasure(3.4) == TestMeasure(3.4)) == true
    @test (TestMeasure(3.4) != TestMeasure(1.2)) == true
    @test (TestMeasure(3.4) != TestMeasure(3.4)) == false
  end

  #working with plain Numbers.
  # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractMeasure, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol)
  # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {U<:AbstractMeasure, T<:Number} = isapprox(x, y.value, atol=atol, rtol=rtol) #I expected this to be implied by prior..
  # Base.:+(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x + T(y)
  # Base.:+(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) + x
  # Base.:-(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = x - T(y)
  # Base.:-(y::U, x::T) where {U<:Number, T<:AbstractMeasure} = T(y) - x
  Base.:*(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T
  Base.:*(x::T, y::U) where {T<:Number, U<:AbstractMeasure} = U(x*y.value)
  Base.:/(x::T, y::U) where {T<:AbstractMeasure, U<:Number} = T(x.value/y)
  @testitem "Measure */Number" begin
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure
    # @test isa(TestMeasure(1.2)+0.1, TestMeasure)
    # @test isa(0.1 + TestMeasure(1.2), TestMeasure)
    # @test isapprox(TestMeasure(1.2)+0.1, TestMeasure(1.3), rtol=1e-3)
    # @test isapprox(0.1+TestMeasure(1.2), TestMeasure(1.3), rtol=1e-3)

    # @test isapprox(TestMeasure(1.2)-0.1, TestMeasure(1.1), rtol=1e-3)
    # @test isapprox(0.1-TestMeasure(1.2), TestMeasure(-1.1), rtol=1e-3)

    @test isa(TestMeasure(1.2)*0.1, TestMeasure)
    @test isapprox(TestMeasure(1.2)*0.1, TestMeasure(0.12), rtol=1e-3)
    @test isapprox(0.1*TestMeasure(1.2), TestMeasure(0.12), rtol=1e-3)
    @test isapprox(TestMeasure(1.2)/0.1, TestMeasure(12), rtol=1e-3)
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
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure 
    @test UnitTypes.Measure.measure2String(TestMeasure(3.4)) == "3.4tm"
    @test string(TestMeasure(3.4)) == "3.4tm"
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
        # println("unitProduct($($Abs1) = $abs1, $($Abs2) = $abs2, $($Operation))")
        Base.:*(x::T, y::U) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(x) * toBaseFloat(y) )
        if $Abs1 != $Abs2
          Base.:*(y::U, x::T) where {T<:abs1, U<:abs2} = $Operation( toBaseFloat(y) * toBaseFloat(x) )
        end
      end
    )
  end
  @testitem "unitProduct" begin
    abstract type AbstractTest <: AbstractMeasure end
    @makeDerivedMeasure TestN "tn" 1.0 AbstractTest 
    @makeDerivedMeasure TestM "tm" 1.0 AbstractTest 
    @makeDerivedMeasure TestNM "tnm" 1.0 AbstractTest 
    
    @unitProduct TestN TestM TestNM 

    @test TestN(1) * TestM(1) ≈ TestNM(1)
    @test TestM(1) * TestN(1) ≈ TestNM(1)

    @test isa(TestM(1)*TestN(1), TestNM)
    @test isa(TestN(1)*TestM(1), TestNM)
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
        absInput = supertype($Input)
        absDivisor = supertype($Divisor)
        # println("unitDivide($($Input) = $absInput, $($Divisor) = $absDivisor, $($Output))")
        Base.:/(x::T, y::U) where {T<:absInput, U<:absDivisor} = $Output( toBaseFloat(x) / toBaseFloat(y) )
      end
    )
  end
  @testitem "unitDivide" begin
    @makeBaseMeasure NM TestNM "tnm"
    @makeBaseMeasure N TestN "tn"
    @makeBaseMeasure M TestM "tm" 
    @addUnitOperations TestN TestM TestNM
    
    @unitDivide TestNM TestN TestM
    @test TestNM(1) / TestN(1) ≈ TestM(1)
    @test isa(TestNM(1)/TestN(1), TestM)

    @unitDivide TestNM TestM TestN
    @test TestNM(1) / TestM(1) ≈ TestN(1)
    @test isa(TestNM(1)/TestM(1), TestN)


    @test true

    # @show TestN(1)/TestNM(1)
    @test_throws MethodError TestN(1)/TestNM(1)
  end

  macro addUnitOperations(In1, In2, Result)
    return esc(
      quote
        absIn1 = supertype($In1) # Meter
        absIn2 = supertype($In2) # Meter
        absRes = supertype($Result) # Meter2

        #product
        Base.:*(x::T, y::U) where {T<:absIn1, U<:absIn2} = $Result( toBaseFloat(x) * toBaseFloat(y) )
        if absIn1 != absIn2 #add the opposite if they are different
          Base.:*(x::T, y::U) where {T<:absIn2, U<:absIn1} = $Result( toBaseFloat(x) * toBaseFloat(y) )
        end

        #divide
        Base.:/(x::T, y::U) where {T<:absRes, U<:absIn1} = $In2( toBaseFloat(x) / toBaseFloat(y) )
        if absIn1 != absIn2
          Base.:/(x::T, y::U) where {T<:absRes, U<:absIn2} = $In1( toBaseFloat(x) / toBaseFloat(y) )
        end

      end
    )
  end
  @testitem "addUnitOperations" begin
    @makeBaseMeasure TestNM TNM "tnm"
    @makeBaseMeasure TestN TN "tn"
    @makeBaseMeasure TestM TM "tm"

    @addUnitOperations TN TM TNM
    
    # @unitProduct TN TM TNM
    @test TN(1) * TM(1) ≈ TNM(1)
    @test TM(1) * TN(1) ≈ TNM(1)

    # # @unitDivide TNM TN TM
    @test TNM(1) / TN(1) ≈ TM(1)
    @test isa(TNM(1) / TN(1), TM)

    # # @unitDivide TNM TM TN
    @test TNM(1) / TM(1) ≈ TN(1)
    @test isa(TNM(1) / TM(1), TN)
  end




  macro returnModuleName()
    # println("returnModuleName: $(__module__)")
    return esc( quote
      $(__module__)
    end
    )
  end


end