module Measure
  using DocStringExtensions
  using TestItems 

  export AbstractMeasure, @makeBaseMeasure, @deriveMeasure, @relateMeasures, toBaseFloat, @u_str
  abstract type AbstractMeasure end


  unitAbbreviations = [] # ("m", :Meter)

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

    global unitAbbreviations = push!(unitAbbreviations, (unitSymbol, Symbol(unitName)))  # ("m", :Meter)

    return esc(
      quote
        abstract type $abstractName <: AbstractMeasure end
        export $abstractName #AbstractLength

        """
          This UnitType represents a basic measure of $($unitName) with units $($unitSymbol).
        """
        struct $unitName <: $abstractName
          value::Number
          toBase::Number
          unit::String
          $unitName(x::Number) = new(x,1.0,$unitSymbol)
        end
        $unitName(x::T where T<:$abstractName) = convert($unitName, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
        export $unitName
        Base.convert(::Type{$unitName}, x::U) where {U<:$abstractName} = $unitName(x.value*x.toBase/1.0) # supply the convert



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
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?
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

    @testset "ensure prevention of mixed base measures" begin
      # @show @macroexpand @makeBaseMeasure DensityTest DensityT "dennyT"
      @makeBaseMeasure DensityTest DensityT "dennyT"
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
      @test 1.2u"mT" ≈ MeterT(1.2)
    end
  end

  Base.:-(x::T where T<:AbstractMeasure) = x * -1 # leading negation
  @testitem "leadingNegation" begin
    a = Meter(1)
    @test -a ≈ Meter(-1)
  end

  """
    `macro deriveMeasure(relation, unit="NoUnit")`
    $TYPEDSIGNATURES

    Derives a new Measure from an existing base measure.
    The left hand side of the equation must already exist, while the right hand side should be undefined, with the trailing string providing the unit symbol.

    ```julia
    @deriveMeasure Meter(1) = MilliMeter(1000) "mm" 
    @deriveMeasure KiloGram(1)*Meter(1)/Second(1)^2 = Newton(1) "N"
    ```

    The resulting types are defined in the containing module, not in UnitTypes, as seen by `println(names(MyModule, all=true))`.
  """
  macro deriveMeasure(relation, unit="NoUnit")
    # println("deriveMeasure($(string(relation)), $unit)")
    # display(dump(relation))

  #   # &= doesn't do short-circuit evaluation, need &&
  #   isSimple = isa(relation, Expr)
  #   isSimple = isSimple && isa(relation.args[1],Expr)
  #   isSimple = isSimple && isa(relation.args[1].args[1], Symbol) # Gram
  #   isSimple = isSimple && string(relation.args[1].args[1]) != "/" # but / is also a Symbol!
  #   isSimple = isSimple && string(relation.args[1].args[1]) != "*" 
  #   isSimple = isSimple && string(relation.args[1].args[1]) != "+" 
  #   isSimple = isSimple && string(relation.args[1].args[1]) != "-" 

  #   isSimple = isSimple && isa(relation.args[2],Expr) 
  #   isSimple = isSimple && isa(relation.args[2].args[2].args[1], Symbol)  # MegaGram
  #   # isSimple = isSimple && typeof(relation.args[2].args[2].args[2]) <: Number # 1e-6
  # # @show isSimple

  #   # A compound relation looks like
  #   #  :((KiloGram(1) * Meter(1)) / Second(1) ^ 2 = FigNewton(1)
  #   isCompound = !isSimple && length(relation.args) == 2 
  #   isCompound = isCompound && isa(relation.args[1],Expr) 
  #   isCompound = isCompound && isa(relation.args[1].args[2].args[2].args[1],Symbol) # rhs should always be a symbol
  #   # isCompound &= isa(relation.args[2],Expr) 
  #   # isCompound &= isa(relation.args[2].args[2].args[1], Symbol) 
  #   # isCompound = length(relation.args) == 2 && isa(relation.args[1],Expr) && isa(relation.args[1].args[2],Expr) && isa(relation.args[1].args[2].args[1], Symbol) 

    isdef = 0
    isun = 0
    for em in eachmatch(r"(?<name>[A-Za-z0-9]*)\(.*\)", string(relation))
      if isdefined(__module__, Symbol(em["name"]) )
        isdef += 1
      else
        isun += 1
      end
      # @show em["name"] isdef isun
    end
    isSimple = isdef == 1 && isun == 1
    isCompound = isdef > 1 && isun == 1

    if isSimple
      lhsType = relation.args[1].args[1]
      lhsFactor = relation.args[1].args[2]
      rhsType = relation.args[2].args[2].args[1]
      rhsFactor = relation.args[2].args[2].args[2]

      global unitAbbreviations = push!(unitAbbreviations, (unit, rhsType))  # ("m", :Meter)

      if !( isdefined(__module__, lhsType) || isdefined(@__MODULE__, lhsType) ) # lhs must be defined in UnitTypes or caller
        # throw(MethodError(@deriveMeasure, "Cannot derive $rhsType from undefined $lhsType in [$relation]")) #..this doesn't work..
        throw(ArgumentError("Cannot derive $rhsType from undefined $lhsType in [$relation]"))
        return
      end

      if !isdefined(__module__, rhsType) 
        return esc(
          quote
              """
                UnitType [`$($rhsType)`](@ref) is a compound unit created by [`$($lhsType)`](@ref), related by [`$($lhsFactor/$rhsFactor)`](@ref), with supertype [`$(supertype($lhsType))`](@ref), and symbol `$($unit)`.
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
          end
        )
      else
        @warn "$rhsType is already defined, cannot re-define"
      end
    elseif isCompound
      rhsSymbol = relation.args[2].args[2].args[1] # FigNewton
      rhsFactor = relation.args[2].args[2].args[2] # 1.2

      abstractName = Symbol("Abstract"*String(rhsSymbol)) #AbstractLength
      
      # constructing the unit function from the full type names
      underFunctionString = replace(string(relation.args[1]), " "=>"", r"\(\d\)"=>"", "("=>"", ")"=>"", "/"=>"_per_", "*"=>"_", "^"=>"_power")  # args[1] = :((KiloGram * Meter) / Second ^ 2) => KiloGram_Meter_per_Second_power2
      underFunctionSymbol = Symbol(underFunctionString)

      ufs = replace(string(relation.args[1]), " "=>"", r"\(\d\)"=>"", "("=>"", ")"=>"")
      for subs in split(underFunctionString, "_")
        # if isdefined(parentmodule(@__MODULE__), Symbol(subs)) || isdefined(@__MODULE__, Symbol(subs)) #|| isdefined(__module__, Symbol(subs)) 
        if isdefined(__module__, Symbol(subs))
          # println("$subs is defined")
          inst = __module__.eval(Meta.parse(subs)) # get the type of str, within the enclosing module #cf https://discourse.julialang.org/t/metaprogramming-obtain-actual-type-from-symbol-for-field-inheritance/84912/3?u=bcon
          ufs = replace(ufs, subs=>inst(1).unit)
        # else
        #   println("$subs is not defined")
        end
      end # ufs is correct, but since this is in the quote how can I get it interpolated into a function name?
      global unitAbbreviations = push!(unitAbbreviations, (ufs, rhsSymbol))  # "m"
      global unitAbbreviations = push!(unitAbbreviations, (unit, rhsSymbol))  # "m"

      if !isdefined(__module__, rhsSymbol)
        return esc(
          quote # Make the new type

            abstract type $abstractName <: AbstractMeasure end
            export $abstractName #AbstractLength

            # """
            #   UnitType [`$($rhsSymbol)`](@ref) is derived from [`$($lhsType)`](@ref), related by [`$($lhsFactor/$rhsFactor)`](@ref), with supertype [`$(supertype($lhsType))`](@ref), and symbol `$($unit)`.
            # """
            struct $rhsSymbol <: $abstractName # how does the new type relate to other types? is it just, and only <:AbstractMeasure?
              value::Number
              toBase::Number
              unit::String
              $rhsSymbol(x::Number) = new(x, 1/$rhsFactor, $unit)
            end
            export $rhsSymbol

            # make the compound conversion function:
            $underFunctionSymbol(n::Number) = $rhsSymbol(n)  # KiloGram_Meter_per_Second_power2(n::Number) = FigNewton(n)

            # isapprox between AbstractFigNewtons
            Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?

            Base.:*(x::T, y::U) where {T<:$abstractName, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T; this enables @show fn = FN(1.3)*3
            Base.:*(x::T, y::U) where {T<:Number, U<:$abstractName} = U(x*y.value) # enables @show fn = 4*FN(1.3)
          end
        )
      else
        @warn "$rhsSymbol is already defined, cannot re-define"
      end
    else
      @warn "$relation is neither simple or compound"
    end
  end
  @testitem "deriveMeasure" begin
    @makeBaseMeasure LengthT MeterT "mT"

    # simple relations
    @deriveMeasure MeterT(1) = MillimeterT(1000) "mmT"
    @test MillimeterT(MeterT(1.2)) ≈ MillimeterT(1200)
    @test MeterT(MillimeterT(1200)) ≈ MeterT(1.200)
    @test typeof(1.4u"mmT") <: AbstractLengthT

    @deriveMeasure MeterT(1) = MillimeterT2(1000)
    @test MillimeterT2(1.2).unit == "NoUnit"

    @deriveMeasure MeterT(π) = DegreeT(1) "dt"
    @test typeof(DegreeT(1)) <: AbstractLengthT

    # re macroexpand() see https://github.com/JuliaLang/julia/issues/56733
    @test_warn "MillimeterT is already defined, cannot re-define" macroexpand(@__MODULE__, :( @deriveMeasure MeterT(1) = MillimeterT(5000) "mmT"  ))
    @test_throws ArgumentError macroexpand(@__MODULE__, :( @deriveMeasure MeterT2(1) = MillimeterT(5000) "mmT") )

    # compound relation
    # @testset "compound relation" begin # if I surround these with this begin/end, kgT, mT, sT aren't added to Main.var##236##, so it's like @__MODULE__ can't see out of the testset s.t. isdefined(kgT) = false above. feels like a bug
    @makeBaseMeasure MassT KiloGramT "kgT"
    @makeBaseMeasure TimeT SecondT "sT"
    @deriveMeasure KiloGramT(1)*MeterT(1)/SecondT(1)^2 = FigNewton(1) "FN"
    # display(UnitTypes.Measure.unitAbbreviations)
    @test KiloGramT_MeterT_per_SecondT_power2(1.2) ≈ FigNewton(1.2)
    @test isapprox(FigNewton(1.3)*3, FigNewton(3.9), atol=1e-3)
    @test 4*FigNewton(1.3) ≈ FigNewton(5.2)
    @test 1.5u"FN" ≈ FigNewton(1.5)
    @test 1.5u"kgT*mT/sT^2" ≈ FigNewton(1.5)

    # because the lhs has a variable number of */, the depth of the expression tree is not known.

    @deriveMeasure MeterT(1)*MeterT(1) = MeterSq(1) "msq"
    @show 1u"msq"

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
    @test UnitTypes.Measure.measure2String(Meter(3.4)) == "3.4m"
    @test string(Meter(3.4)) == "3.4m"
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
  end

  """
    Macro to provide the 1.2u"cm" inline unit assignment.
    ```julia
    a = 1.2u"cm" 
    ```
    This function relies on cm(1.2) existing as an alias for CentiMeter(1.2).
    
    The macro works by converting the unit string into a function, which is called on (1) and returned.
    This return is implicitly multiplied/concatenated with the rest of the source expression, calling the defined multiply method.
  """
  macro u_str(unit::String)
    # # if contains(unit, r"[*/^]")
    #   usym = Symbol(unit) # convert string into a Symbol
    # # else
    # #   unit = replace(unit, "*"=>"_", "/"=>"_per_", "^"=>"_power") # assumes functions like Kg_m_per_s_power2()::Newton exist, these are added by relateMeasures
    # # end
    # return esc(:($usym(1))) # return unit(1) which is implicitly * with the leading number

    for abb in unitAbbreviations
      if abb[1] == unit
        return __module__.eval(:($(abb[2])(1)))
      end
    end
    @warn "did not find $unit in unitAbbreviations, returning 0"
    return 0
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