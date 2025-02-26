using DocStringExtensions
using TestItems 

export AbstractMeasure, @makeBaseMeasure, @makeMeasure, @relateMeasures, toBaseFloat, @u_str
abstract type AbstractMeasure end

unitAbbreviations = [] # ("m", :Meter), a list of defined abbreviations for uniqueness checking

"""
  `macro makeBaseMeasure(quantityName, unitName, unitSymbol::String, isAffine=false)`

  Make a new base measure which has no relationship to an existing unit.
  For example, in `@makeBaseMeasure Length Meter "m"`:
  * `quantityName` is the name of the measure, 'Length' above.
  * `unitName` is the name of the unit which will be used to make measures bearing that unit, 'Meter' above.
  * `unitSymbol` is the abbreviation of the unit name, used in all string presentations of the measure.
  * `isAffine` is normally false, if true the +-/* operations are not added for this and derived units and need to be added by hand.
  The macro will introduce `AbstractLength <: AbstractMeasure` and `Meter()` into the current scope.
  
  Measures created by the macro have fields:
  * `value::Number` raw value of the measure
  * `toBase::Number` == 1 for base measures
  * `unit::String` the unit to be displayed

  To get the measure's value in the base unit as a float, see toBaseFloat().
"""
macro makeBaseMeasure(quantityName, unitName, unitSymbol::String, isAffine=false)
  # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName unitSymbol:$unitSymbol")
  abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength

  global unitAbbreviations = push!(unitAbbreviations, (unitSymbol, Symbol(unitName)))  # ("m", :Meter)

  qts = [quote
      abstract type $abstractName <: AbstractMeasure end
      export $abstractName #AbstractLength

      """
        This UnitType represents a basic measure of $($unitName) with units $($unitSymbol).
      """
      struct $unitName <: $abstractName
        value::Number # the value on creation as measured in the unit
        toBase::Number # the conversion factor to the base unit, s.t. value * toBase = value in base unit
        unit::String # string represenation of the unit
        $unitName(x::Number) = new(x,1.0,$unitSymbol) # 1.0 b/c this is @makeBaseMeasure
      end
      $unitName(x::T where T<:$abstractName) = convert($unitName, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
      export $unitName
      Base.convert(::Type{$unitName}, x::U) where {U<:$abstractName} = $unitName(x.value*x.toBase) # supply the convert

      # enable range
      Base.zero(x::T) where T<:$abstractName = T(0) #zero() seems to be required for _colon()
      Base._colon(start::T, step::U, stop::V) where {T<:$abstractName, U<:$abstractName, V<:$abstractName} = T.(start.value : convert(T,step).value : convert(T,stop).value)

      # enable iteration&broadcasting
      Base.broadcastable(x::T) where T<:$abstractName = Ref(x) # If a type is intended to act like a "0-dimensional scalar" (a single object) rather than as a container for broadcasting, then the following method should be defined:

      # enable comparisons
      Base.isequal(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = convert(T,x).value == convert(T,y).value
      Base.:<(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = x.value < convert(T,y).value # other <> ops are defined from this
      Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?
      # Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:Number} removed in order to discover what more specific defs are needed = isapprox(x.value, y, atol=atol, rtol=rtol) # when comparing to number, do not convert to base units; 
    end
  ]
  
  if isAffine == false
    push!( qts, quote
      # math
      Base.:+(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = T(x.value+convert(T,y).value) #result returned in the unit of the first measure
      Base.:-(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = T(x.value-convert(T,y).value)

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
  return esc( Expr(:block, qts...))
end

@testitem "@makeBaseMeasure" begin
  @makeBaseMeasure MeterTest MeterT "metT" 
  @makeMeasure MeterT(1) = MilliMeterT(1000) "miliT" # u_str needs symbol uniqueness! when this was "mmT", it seemed to conflict with the @makeMeasure test definition of "mmT" and this conflict showed up as LoadError: UndefVarError: `MiliT` not defined in `Main.var"##233"`

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

  @testset "ensure prevention of mixed base measures" begin
    # @show @macroexpand @makeBaseMeasure DensityTest DensityT "dennyT"
    @makeBaseMeasure DensityTest DensityT "dennyT"
    @test_throws MethodError MeterT(1.2)*DensityT(3.4)
  end

  @testset "math" begin
    @test isapprox(MeterT(1.2)+MeterT(0.1), MeterT(1.3), rtol=1e-3)
    @test isapprox(MeterT(1.2)-MeterT(0.1), MeterT(1.1), rtol=1e-3)
    @test MeterT(1) + MilliMeterT(1000) ≈ MeterT(2)
    @test MeterT(2) - MilliMeterT(1000) ≈ MeterT(1)
  end

  @testset "*/Number" begin
    @test isa(MeterT(1.2)*0.1, MeterT)
    @test isapprox(MeterT(1.2)*0.1, MeterT(0.12), rtol=1e-3)
    @test isapprox(0.1*MeterT(1.2), MeterT(0.12), rtol=1e-3)
    @test isapprox(MeterT(1.2)/0.1, MeterT(12), rtol=1e-3)
  end

  @testset "unit constructor" begin
    @test 1.2u"metT" ≈ MeterT(1.2)
  end
end

Base.:-(x::T where T<:AbstractMeasure) = x * -1 # leading negation
@testitem "leadingNegation" begin
  a = Meter(1)
  @test -a ≈ Meter(-1)
end

"""
  `macro makeMeasure(relation, unit="NoUnit", defineConverts=true)`

  Creates a new Measure from an existing base measure.
  The left hand side of the equation must already exist, while the right hand side should be undefined, with the trailing string providing the unit symbol.

  ```
  @makeMeasure Meter(1) = MilliMeter(1000) "mm" 
  ```

  The resulting types are defined in the containing module, not in UnitTypes, as seen by `println(names(MyModule, all=true))`.
"""
macro makeMeasure(relation, unit="NoUnit", defineConverts=true)
  # println("makeMeasure($(string(relation)), $unit)")
  # display(dump(relation))
  
  # Make the new type
  rhs = skipSymbolBlock(relation.args[2])
  # @show dump(rhs)

  # rhs will always have the same format (unless a symbol or variable is used for the argument)
  rhsSymbol = rhs.args[1]
  rhsFactor = rhs.args[2]
  rhsUnit = unit

  if isdefined(__module__, rhsSymbol) 
    @warn "$rhsSymbol is already defined, cannot re-define"
    return
  end

  for abb in unitAbbreviations
    if abb[1] == unit && abb[1] != "NoUnit"
      if relation.args[2].args[1] isa LineNumberNode
        @warn "$unit is already used by $(abb[2]), suggest choosing a unique unit at $(relation.args[2].args[1].file):$(relation.args[2].args[1].line)"
      else
        @warn "$unit is already used by $(abb[2]), suggest choosing a unique unit"
      end
    end
  end

  # lhs has variable structure depending on variable/symbol usage:
  lhs = skipSymbolBlock(relation.args[1])

  if length(lhs.args) == 2 # Meter(10) => [Symbol Meter][Int64 10]
    # @show dump(lhs) #:
    # Expr
    #   head: Symbol call
    #   args: Array{Any}((2,))
    #     1: Symbol Meter
    #     2: Int64 10

    lhsSymbol = lhs.args[1]
    lhsFactor = lhs.args[2]

    if !( isdefined(__module__, lhsSymbol) || isdefined(@__MODULE__, lhsSymbol) ) # lhs must be defined in UnitTypes or caller
      throw(ArgumentError("Cannot derive $rhsSymbol from undefined $lhsSymbol in [$relation]"))
      return
    end

    global unitAbbreviations = push!(unitAbbreviations, (rhsUnit, rhsSymbol))  # ("m", :Meter) append to list

    # Make the new type
    qts = [ quote 
      # abstract type $rhsAbstractName <: AbstractMeasure end # this is wrong, needs to be supertype(lhs) if simple
      # export $rhsAbstractName #AbstractLength
      lhsAbstract = supertype($lhsSymbol)

      """
      #   UnitType `$($rhsSymbol)` is derived from `$($lhsSymbol)`, related by `$($lhsFactor)/$($rhsFactor)`, with supertype `$(supertype($lhsSymbol))`, and unit `$($unit)`.
      # """
      struct $rhsSymbol <: lhsAbstract # how does the new type relate to other types? is it just, and only <:AbstractMeasure?
        value::Number
        toBase::Number
        unit::String
        $rhsSymbol(x::Number) = new(x, $lhsFactor/$rhsFactor, $rhsUnit)
      end
      export $rhsSymbol

      $rhsSymbol(x::T where T<:lhsAbstract) = convert($rhsSymbol, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
    end ]

    if defineConverts
      push!(qts, quote
        Base.convert(::Type{$rhsSymbol}, x::U) where {U<:lhsAbstract} = $rhsSymbol(x.value*x.toBase * $rhsFactor/$lhsFactor) # convert(MilliMeter, 1in) = 1in*.0254m/in * 1000mm/1m
        Base.convert(::Type{$lhsSymbol}, x::$rhsSymbol) = $lhsSymbol(x.value*($lhsFactor/$rhsFactor)) # convert(Meter, MilliMeter(3))
      end)
    end

    # display(qts)
    return esc( Expr(:block, qts...))
  else
    throw(ArgumentError("Left hand side has unexpected format, cannot alias $rhsSymbol, try simplifying."))
  end
end

@testitem "makeMeasure" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @makeMeasure MeterT(1) = MilliMeterT(1000) "mmT"
  @makeMeasure MeterT(1) = MilliMeterT2(1000)
  @makeMeasure MeterT(0.0254) = InchT(1) "inT"

  @testset "simple relations" begin
    @test MeterT(1) + MilliMeterT(1000) ≈ MeterT(2)

    @test MeterT(1.2) ≈ MilliMeterT(1200)
    @test MilliMeterT(1200) ≈ MeterT(1.2)
    @test typeof(1.4u"mmT") <: AbstractLengthT

    @test MilliMeterT2(1.2).unit == "NoUnit"
  end

  @testset "convert" begin
    @test convert(MeterT, MilliMeterT(1000)) ≈ MeterT(1)
    @test convert(MilliMeterT, MeterT(1)) ≈ MilliMeterT(1000)
    @test MilliMeterT(MeterT(1.2)) ≈ MilliMeterT(1200)
    @test MeterT(MilliMeterT(1200)) ≈ MeterT(1.200)
    @test convert(MeterT, InchT(10)) ≈ MeterT(0.254)
  end

  @testset "erroneous definition" begin
    # re macroexpand() see https://github.com/JuliaLang/julia/issues/56733
    @test_warn "MilliMeterT is already defined, cannot re-define" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MilliMeterT(5000) "mmT"  ))
    @test_warn "mmT is already used by MilliMeterT, suggest choosing a unique unit" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  ))
    @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterTNot(1) = MilliMeterT3(5000) "mmT3"  )) # error on LHS not existing
    @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterT(1)*Seconds(3) = MilliMeterTS(5000) "mmTS"  )) # error on compound relations
  end
end

function skipSymbolBlock(eexp)
  if eexp.head == :block
    return eexp.args[2]
  else
    return eexp
  end
end

"""
  `function measure2String(m::AbstractMeasure)::String`

  Returns a string representing measure `m` in the format "1.23mm".
"""
function measure2String(m::AbstractMeasure)::String
  # return @sprintf("%3.3f []", m)
  return "$(m.value)$(m.unit)"
end

"""
  `function Base.show(io::IO, m::AbstractMeasure)`

  @show functionality for Measures via `measure2String()`.
"""
function Base.show(io::IO, m::AbstractMeasure)
  print(io, measure2String(m))
end

"""
  `function toBaseFloat(m::AbstractMeasure) :: Float64`

  Returns measure `m` as a float in the base unit.
""" 
function toBaseFloat(m::AbstractMeasure) :: Float64
  return m.value * m.toBase
end
@testitem "Measure measure2string()" begin
  @test UnitTypes.measure2String(Meter(3.4)) == "3.4m"
  @test string(Meter(3.4)) == "3.4m"
end

"""
  `macro relateMeasures(relation)`

  Adds a multiplicative relationship between the left and right sides of the equation, allowing units to be multiplied and divided with consistent units.
  All types must already be defined and only one * is supported on the left side, while the right should the resultant type.

  ```
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

    qts = [ quote end ] # build expressions in quotes
    if operator == :*
      push!(qts, quote 
        Base.:*(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = $type12( toBaseFloat(x) * toBaseFloat(y) )
        Base.:/(x::T, y::U) where {T<:supertype($type12), U<:supertype($type1)} = $type2( toBaseFloat(x) / toBaseFloat(y) )
        if supertype($type1) != supertype($type2) # add inverse only for when supertypes differ
            Base.:*(x::T, y::U) where {T<:supertype($type2), U<:supertype($type1)} = $type12( toBaseFloat(x) * toBaseFloat(y) )
            Base.:/(x::T, y::U) where {T<:supertype($type12), U<:supertype($type2)} = $type1( toBaseFloat(x) / toBaseFloat(y) )
        else # type1 == type2
            Base.sqrt(x::T) where T<:supertype($type12) = $type1( sqrt(toBaseFloat(x)) ) # I can define sqrt(m^2) -> m, but I cannot define x^0.5 b/c the exponent might not lead to a known or integer unit..
        end
      end)
    elseif operator == :/
      push!(qts, quote
        Base.:/(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = $type12( toBaseFloat(x) / toBaseFloat(y) ) # F/mm2 = AForce/AArea = Pressure(/)
        Base.:*(x::T, y::U) where {T<:supertype($type12), U<:supertype($type2)} = $type1( toBaseFloat(x) * toBaseFloat(y) ) # Pa*mm2 = F
        Base.:*(x::T, y::U) where {T<:supertype($type2), U<:supertype($type12)} = y*x # mm2*Pa = F
      end)
    else
      throw(ArgumentError("Operator $operator unknown, @relateMeasures accepts only multiplicative measures in the format: @relateMeasures Meter*Newton=NewtonMeter"))
    end

    return esc( Expr(:block, qts...))
  else
    throw(ArgumentError("@relateMeasures given incorrect format"))
  end
end

@testitem "relateMeasures" begin
  # multiplicative same
  @makeBaseMeasure TestM MeterT "tm"
  @makeBaseMeasure TestMM Meter2T "tmm"
  @relateMeasures MeterT*MeterT = Meter2T
  @test Meter2T(1) / MeterT(1) ≈ MeterT(1)
  @test sqrt(Meter2T(4)) ≈ MeterT(2)

  # multiplicative different
  @makeBaseMeasure TestNM NewtonMeterT "tnm"
  @makeBaseMeasure TestN NewtonT "tn"
  @relateMeasures NewtonT*MeterT = NewtonMeterT
  @test NewtonT(1) * MeterT(1) ≈ NewtonMeterT(1)
  @test MeterT(1) * NewtonT(1) ≈ NewtonMeterT(1)
  @test MeterT(1) * MeterT(1) ≈ Meter2T(1)
  @test NewtonMeterT(1) / MeterT(1) ≈ NewtonT(1)
  @test NewtonMeterT(1) / NewtonT(1) ≈ MeterT(1)

  # division 
  @makeBaseMeasure TestPa PascalT "tPa"
  @relateMeasures NewtonT / Meter2T = PascalT
  @test NewtonT(1)/Meter2T(1) ≈ PascalT(1)
  @test PascalT(1)*Meter2T(1) ≈ NewtonT(1)
  @test Meter2T(1)*PascalT(1) ≈ NewtonT(1)

  #non-operator
  @makeBaseMeasure TestFigN TestFigNM "fnm"
  @test_throws ArgumentError macroexpand(@__MODULE__, :( @relateMeasures TestN%TestM = TestFigNM  )) 
end

"""
  `macro u_str(unit::String)`

  Macro to provide the 1.2u"cm" inline unit assignment.
  ```
  a = 1.2u"cm" 
  ```
  This function relies on cm(1.2) existing as an alias for CentiMeter(1.2).
  
  The macro works by converting the unit string into a function, which is called on (1) and returned.
  This return is implicitly multiplied/concatenated with the rest of the source expression, calling the defined multiply method.
"""
macro u_str(unit::String)
  for abb in unitAbbreviations
    if abb[1] == unit
      return __module__.eval(:($(abb[2])(1)))
    end
  end
  @warn "did not find $unit in unitAbbreviations, returning 0"
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