module Measure
  using DocStringExtensions
  using TestItems 

  export AbstractMeasure, @makeBaseMeasure, @makeDerivedMeasure, @deriveMeasure, @relateMeasures, toBaseFloat, @u_str
  abstract type AbstractMeasure end

  """
    `macro makeBaseMeasure(quantityName, unitName, unitSymbol::String)`

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

    Internally, this calls @makeDerivedMeasure to make the basic type, then overloads common functions (==, <, ≈, +, -, *, /, :) to enable common operations.

    To get the measure's value in the base unit as a float, see [toBaseFloat()](toBaseFloat).
  """
  macro makeBaseMeasure(quantityName, unitName, unitSymbol::String)
    # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName unitSymbol:$unitSymbol")
    abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength

    return esc(
      quote
        export $abstractName #AbstractLength
        abstract type $abstractName <: AbstractMeasure end

        @makeDerivedMeasure $unitName $unitSymbol 1.0 $abstractName

        # these only need to be defined on the base measure as derived <:
        # I am putting them in here with $abstractName instead of <:AbstractMeasure in order to get error messages that say + is not defined on <:AbstractLength, etc., rather than the less direct convert() is not defined on <:AbstractLength and <:AbstractDuration
        # Base.convert(::Type{T}, x::U) where {T<:$abstractName, U<:$abstractName} # this only makes sense with derived measures, but if placed in @derive then it leads to duplicates # = T(x.value*x.toBase/T(1.0).toBase) #...this is janky but works to get the destination's toBase...

        # enable range
        Base.zero(x::T) where T<:$abstractName = T(0) #zero() seems to be required for _colon()
        Base._colon(start::T, step::U, stop::V) where {T<:$abstractName, U<:$abstractName, V<:$abstractName} = T.(start.value : convert(T,step).value : convert(T,stop).value)

        # enable iteration&broadcasting
        Base.broadcastable(x::T) where T<:$abstractName = Ref(x) # If a type is intended to act like a "0-dimensional scalar" (a single object) rather than as a container for broadcasting, then the following method should be defined:

        # enable comparisons
        Base.isequal(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = convert(T,x).value == convert(T,y).value
        Base.:<(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = x.value < convert(T,y).value # other <> ops are defined from this
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should scale these in some fair way, todo
        # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:Number} removed in order to discover what more specific defs are needed = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units; 

        # math
        Base.:+(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = T(x.value+convert(T,y).value) #result returned in the unit of the first measure
        Base.:-(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = T( x.value-convert(T,y).value)
        # Base.:*(x::T, y::U) where {T<:$abstractName, U<:$abstractName} # commented to prevent redefinition warning # = throw(MethodError("Cannot $x * $y, * is not defined yet, define with @relateMeasures"))
        # Base.:/(x::T, y::U) where {T<:$abstractName, U<:$abstractName} # if appropriate, provided by @relateMeasures # = T( x.value/convert(T,y).value)
        # Base.:/(x::T, y::U) where {T<:$abstractName, u<:$abstractName} = toBaseFloat(x)/toBaseFloat(y) # any danger to returning float here?

        # */ Number, usually used in scaling things
        # Base.:+(<:Number) not implemented to prevent random numbers from assuming UnitTypes, the point is to be explicit
        Base.:*(x::T, y::U) where {T<:$abstractName, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T
        Base.:*(x::T, y::U) where {T<:Number, U<:$abstractName} = U(x*y.value)
        # Base.:^(x::T, y::U) where {T<:$abstractName, U<:Number} = T(x.value^y) # is this ever rightly needed?
        Base.:/(x::T, y::U) where {T<:$abstractName, U<:Number} = T(x.value/y)

        # Base.abs()
      end
    )
  end
  @testitem "@makeBaseMeasure" begin
    @makeBaseMeasure MeterTest MeterT "mT" 

    @testset "did the macro create the definitions we expect" begin
      @test isdefined(@__MODULE__, :AbstractMeterTest)
      @test isdefined(@__MODULE__, :MeterT)
    end
    
    @testset "constructor" begin
      @test MeterT(3.4).value == 3.4
      # @show MeterT(2im).value # should imaginary error?
    end

    @testset "range" begin
      b = MeterT(1) : MeterT(0.3) : MeterT(2)
      @test b[1] ≈ MeterT(1)
      @test b[2] ≈ MeterT(1.3)
      @test last(b) ≈ MeterT(1.9)

      c = LinRange(MeterT(10), MeterT(20), 4)
      @test c[1] ≈ MeterT(10)
      @test c[2] ≈ MeterT(13+1/3)
      @test last(c) ≈ MeterT(20)
    end

    @testset "broadcast" begin
      @test isa([1,2,3] .* MeterT(4), Vector{MeterT})
      for m in MeterT.([1,2,3])
        @test m≈MeterT(1) || m≈MeterT(2) || m≈MeterT(3)
      end
    end

    @testset "comparsions" begin
      @test MeterT(1.2) < MeterT(3.4)
      @test MeterT(1.2) <= MeterT(3.4)
      @test MeterT(3.4) > MeterT(1.2)
      @test MeterT(3.4) >= MeterT(1.2)
      @test (MeterT(3.4) == MeterT(1.2)) == false
      @test (MeterT(3.4) == MeterT(3.4)) == true
      @test (MeterT(3.4) != MeterT(1.2)) == true
      @test (MeterT(3.4) != MeterT(3.4)) == false
      @test isapprox(MeterT(1.2), MeterT(1.2), rtol=1e-3)
      @test MeterT(1.2) ≈ MeterT(1.2)
    end

    @testset "ensure prevented from mixing base measures" begin
      @makeBaseMeasure DensityTest DensityT "den"
      @test_throws MethodError MeterT(1.2)*DensityT(3.4)
    end

    @testset "math" begin
      @test isapprox(MeterT(1.2)+MeterT(0.1), MeterT(1.3), rtol=1e-3)
      @test isapprox(MeterT(1.2)-MeterT(0.1), MeterT(1.1), rtol=1e-3)
    end

    @testset "*/Number" begin
      @test isa(MeterT(1.2)*0.1, MeterT)
      @test isapprox(MeterT(1.2)*0.1, MeterT(0.12), rtol=1e-3)
      @test isapprox(0.1*MeterT(1.2), MeterT(0.12), rtol=1e-3)
      @test isapprox(MeterT(1.2)/0.1, MeterT(12), rtol=1e-3)
    end

    @testset "unit constructor" begin
      @test mT(1.2) ≈ MeterT(1.2)
    end
  end

  """
    `macro makeDerivedMeasure(name, unit, toBase, referenceType)`

    Makes a new Measure derived from some other, usually abstract Measure.
    In most cases prefer @deriveMeasure for better readability.

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
    `@relateMeasures Newton*Meter=NewtonMeter`

    The created unit type is a member of whichever module contains the macro call:
    ```julia
    module MyMod
      `@makeDerivedMeasure MilliNewtonMeter "mN*m" 1e-3 NewtonMeter`
    end

    mnm = MyMod.MilliNewtonMeter(3.4)
    ```
    This means that other modules will not have access to MilliNewtonMeter via UnitTypes, but only if they import/using MyMod.
    
  """
  macro makeDerivedMeasure(name, unit, toBase, referenceType)
    # println("makeDerivedMeasure( $name :: $(typeof(name)), $unit :: $(typeof(unit)) )")

    usym = Symbol(unit) # convert from string into symbol
    skip = occursin(r"[*/^-]", unit) # skip ading if this unit is composed, handle those in @relateMeasures

    return esc( 
      quote
        if Base.isconcretetype($referenceType)
          absName = supertype($referenceType) 
        else 
          absName = $referenceType
        end

        """
          This UnitType represents units of $($name).
        """
        struct $name <: absName
          value::Number
          toBase::Number
          unit::String
          $name(x::Number) = new(x,$toBase,$unit)
        end
        $name(x::T where T<:absName) = convert($name, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
        export $name
        Base.convert(::Type{$name}, x::U) where {U<:absName} = $name(x.value*x.toBase/$toBase) # supply the convert

        # $name(uf::T) where T<:Unitful.AbstractQuantity = convert($name, uf) # add a constructor for Unitful units to prevent struct.value = Unitful...this requires that all modules that use @makeDerived also import Unitful, commenting out for now...
        # $name(uf::T) where T<:UnitTypes.Measure.Unitful.AbstractQuantity = convert($name, uf) 

        # use the unit label as a funcional unit label: mm(1) => MilliMeter(1)
        if !$skip
          $usym(num::Number) = $name(num)
        end
      end
    )
  end

  @testitem "derivedMeasure" begin
    @makeBaseMeasure MeterTest MeterT "mT"
    @makeDerivedMeasure MillimeterT "mmT" 1e-3 MeterT

    @testset "constructor & convert" begin
      @test typeof(MillimeterT(1.2)) <: AbstractMeterTest

      @test isa(convert(MeterT, MillimeterT(1200)), MeterT)
      @test isapprox(convert(MeterT, MillimeterT(1200)), MeterT(1.2))

      @test MeterT(MillimeterT(1200)) ≈ MeterT(1.2)
      @test MillimeterT(MeterT(1.2)) ≈ MillimeterT(1200)
    end

    @testset "isapprox" begin
      @test isapprox(MeterT(1.2), MillimeterT(1200), atol=1e-3)
    end

    @testset "unit label" begin
      @test mmT(1.2) ≈ MillimeterT(1.2)
      @makeDerivedMeasure MSquare "mmT^2" 1 MeterT
      # @show names(UnitTypes, all=true)
      # @show names(parentmodule(mmT), all=true)
      # @show mmT^2(3.4)
      # @show Symbol("mmT^2")(3.4)
    end
  end

  """
    `macro deriveMeasure(relation, unit="NoUnit")`
    $TYPEDSIGNATURES

    Derives a new Measure from an existing base measure.
    This is preferred over @makeDerivedMeasure for better readability.
    The left hand side of the equation must already exist, while the right hand side should be undefined, with the trailing string providing the unit symbol.

    ```julia
    @deriveMeasure Meter(1) = Millimeter(1000) "mm" 
    ```
  """
  macro deriveMeasure(relation, unit="NoUnit")
    if length(relation.args) == 2 # maybe there's a better initial error check?
      lhsType = relation.args[1].args[1]
      lhsFactor = relation.args[1].args[2]
      rhsType = relation.args[2].args[2].args[1]
      rhsFactor = relation.args[2].args[2].args[2]

      usym = Symbol(unit) # convert from string into symbol
      skip = occursin(r"[*/^-]", unit) # skip ading if this unit is composed, handle those in @relateMeasures

      if !( isdefined(__module__, lhsType) || isdefined(@__MODULE__, lhsType) ) # lhs must be defined in UnitTypes or caller
        # throw(MethodError(@deriveMeasure, "Cannot derive $rhsType from undefined $lhsType in [$relation]")) #..this doesn't work..
        throw(ArgumentError("Cannot derive $rhsType from undefined $lhsType in [$relation]"))
        return
      end

      if !isdefined(__module__, rhsType) 
        return esc(
          quote
              """
                UnitType [`$($rhsType)`](@ref) is derived from [`$($lhsType)`](@ref), related by [`$($lhsFactor/$rhsFactor)`](@ref), with supertype [`$(supertype($lhsType))`](@ref), and symbol `$($unit)`.
              """
              struct $rhsType <: supertype($lhsType)
                value::Number
                toBase::Number
                unit::String
                $rhsType(x::Number) = new(x, $lhsFactor/$rhsFactor, $unit)
              end
              export $rhsType
              $rhsType(x::T where T<:supertype($lhsType)) = convert($rhsType, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
              Base.convert(::Type{$rhsType}, x::U) where {U<:supertype($lhsType)} = $rhsType(x.value*x.toBase/($lhsFactor/$rhsFactor)) # convert(MilliMeter, Meter())
              Base.convert(::Type{$lhsType}, x::$rhsType) = $lhsType(x.value*($lhsFactor/$rhsFactor)) # convert(Meter, MilliMeter(3))

              # use the unit label as a funcional unit label: mm(1) => MilliMeter(1)
              if !$skip
                $usym(num::Number) = $rhsType(num)
              end
          end
        )
      else
        @warn "$rhsType is already defined, cannot re-define"
      end
    else
      println("@deriveMeasure given incorrect format of [$relation], skipping")
    end
  end
  @testitem "deriveMeasure" begin
    @makeBaseMeasure MeterTest MeterT "mT"
    @deriveMeasure MeterT(1) = MillimeterT(1000) "mmT"

    @test MillimeterT(MeterT(1.2)) ≈ MillimeterT(1200)
    @test MeterT(MillimeterT(1200)) ≈ MeterT(1.200)

    # @deriveMeasure MeterT(1) = MillimeterT(5000) "mmT"  # yes this issues, but I don't have the right test for it...
    # @test MillimeterT(1.2).toBase ≈ 1e-3 #test that it was not executed
    # @test_warn "MillemeterT is already defined, cannot re-define" @deriveMeasure MeterT(1) = MillimeterT(5000) "mmT" # this should be rejected as a redefinition and toBase remain 1e-3 via warning
    # @test_logs (:warn, "MillemeterT is already defined, cannot re-define") @deriveMeasure MeterT(1) = MillimeterT(5000) "mmT" # this should be rejected as a redefinition and toBase remain 1e-3 via warning

    # display(names(@__MODULE__))

    # @test_throws MethodError @deriveMeasure MeterT2(1) = MillimeterT(5000) "mmT" # this should be rejected as MeterT2 doesn't exist, but the extra macros upset the hard coded indexing
    # @test_throws MethodError @deriveMeasure(MeterT2(1)=Millimeter(5000), "mmT") # too many macros, messing up the hard-coded indexing
    # @deriveMeasure MeterT2(1) = MillimeterT(5000) "mmT" # works; this should be rejected as MeterT2 doesn't exist, but the extra macros upset the hard coded indexing

    @deriveMeasure MeterT(1) = MillimeterT2(1000)
    @test MillimeterT2(1.2).unit == "NoUnit"

    @test mT(3.4) ≈ mmT(3400)
  end

  """
    $TYPEDSIGNATURES
    Returns a string representing measure `m` in the format "1.23mm".
  """
  function measure2String(m::AbstractMeasure)::String
    # return @sprintf("%3.3f []", m)
    return "$(m.value)$(m.unit)"
  end

  """
    @show functionality for Measures via `measure2String()`.
  """
  function Base.show(io::IO, m::AbstractMeasure)
    print(io, measure2String(m))
  end
  
  ```
    $TYPEDSIGNATURES
    Returns measure `m` as a float in the base unit.
  ``` 
  function toBaseFloat(m::AbstractMeasure) :: Float64
    return m.value * m.toBase
  end
  @testitem "Measure measure2string()" begin
    @makeDerivedMeasure TestMeasure "tm" 1.0 AbstractMeasure 
    @test UnitTypes.Measure.measure2String(TestMeasure(3.4)) == "3.4tm"
    @test string(TestMeasure(3.4)) == "3.4tm"
  end

  """
    $TYPEDSIGNATURES
    Adds a multiplicative relationship between the left and right sides of the equation, allowing units to be multiplied and divided with consistent units.
    All types must already be defined and only one * is supported on the left side, while the right should the resultant type.
    ```julia
      @relateMeasures Meter*Newton = NewtonMeter
    ```
  """
  macro relateMeasures(relation)
    # an alternate format would be: @relateMeasures Meter(1)*Centimeter(100)=Meter2(1), adding conversion...
    if length(relation.args) == 2# && isa(relation.args[2], Expr)
      operator = relation.args[1].args[1] # *
      type1 = relation.args[1].args[2] # TN
      type2 = relation.args[1].args[3] # TM
      type12 = relation.args[2].args[2] # TNM
      return esc(
        quote
          if Symbol($operator) == Symbol("*") # @relateMeasures Newton*Meter = NewtonMeter
            Base.:*(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = $type12( toBaseFloat(x) * toBaseFloat(y) )
            Base.:/(x::T, y::U) where {T<:supertype($type12), U<:supertype($type1)} = $type2( toBaseFloat(x) / toBaseFloat(y) )
            if supertype($type1) != supertype($type2) # add inverse only for when supertypes differ
              Base.:*(x::T, y::U) where {T<:supertype($type2), U<:supertype($type1)} = $type12( toBaseFloat(x) * toBaseFloat(y) )
              Base.:/(x::T, y::U) where {T<:supertype($type12), U<:supertype($type2)} = $type1( toBaseFloat(x) / toBaseFloat(y) )
            else # type1 == type2
              Base.sqrt(x::T) where T<:supertype($type12) = $type1( sqrt(toBaseFloat(x)) ) # I can define sqrt(m^2) -> m, but I cannot define x^0.5 b/c the exponent might not lead to a known or integer unit..
            end
          else
            throw(ArgumentError("Operator $($operator) unknown, @relateMeasures accepts only multiplicative measures in the format: @relateMeasures Meter*Newton=NewtonMeter"))
          end
        end
      )
    end
  end

  @testitem "relateMeasures" begin
    @makeBaseMeasure TestNM NewtonMeterT "tnm"
    @makeBaseMeasure TestN NewtonT "tn"
    @makeBaseMeasure TestM MeterT "tm"
    @relateMeasures NewtonT*MeterT = NewtonMeterT

    @makeBaseMeasure TestMM Meter2T "tmm"
    @relateMeasures MeterT*MeterT = Meter2T

    @test NewtonT(1) * MeterT(1) ≈ NewtonMeterT(1)
    @test MeterT(1) * NewtonT(1) ≈ NewtonMeterT(1)
    @test MeterT(1) * MeterT(1) ≈ Meter2T(1)

    @test NewtonMeterT(1) / MeterT(1) ≈ NewtonT(1)
    @test NewtonMeterT(1) / NewtonT(1) ≈ MeterT(1)
    @test Meter2T(1) / MeterT(1) ≈ MeterT(1)
    @test sqrt(Meter2T(4)) ≈ MeterT(2)

    @test tn(1)*tm(2) ≈ tnm(2)

    # @makeBaseMeasure TestMMM Meter3T "tm^3"
    # @show eval(Symbol("tm^3"))(3.4)
  end

  """
    Macro to provide the Unitful-like 1.2u"cm" inline unit assignment.
    ```julia
    a = 1.2u"cm" 
    ```
    This function relies on cm(1.2) existing as an alias for CentiMeter(1.2).
    
    The macro works by converting the unit string into a function, which is called on (1) and returned.
    This return is implicitly multiplied/concatenated with the rest of the source expression, calling the defined multiply method.
  """
  macro u_str(unit::String)
    usym = Symbol(unit) # convert string into a Symbol
    return esc(:($usym(1))) # return unit(1) which is implicitly * with the leading number
  end

  @testitem "u_str" begin
    @makeBaseMeasure TestNM NewtonMeterT "tnm"
    @makeBaseMeasure TestN NewtonT "tn"
    @makeBaseMeasure TestM MeterT "tm"
    @relateMeasures NewtonT*MeterT = NewtonMeterT

    @test 1.2u"tm" ≈ MeterT(1.2)
    @test 1.0u"tm" * 2.0u"tn" ≈ NewtonMeterT(2.0)
    @test 2.0u"tnm" / 1.0u"tn" ≈ MeterT(2.0)
  end



end