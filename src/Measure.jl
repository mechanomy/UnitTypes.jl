module Measure
  using DocStringExtensions
  using TestItems 

  export AbstractMeasure, @makeBaseMeasure, @makeDerivedMeasure, @deriveMeasure, @relateMeasures, toBaseFloat, @u_str
  export @deriveMeasure_new, @w_str
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
        abstract type $abstractName <: AbstractMeasure end
        export $abstractName #AbstractLength

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
          # export $usym # I'm getting 'syntax: malformed "export" statement' from this...
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
    if length(relation.args) == 2 # is there a single = sign? maybe there's a better initial error check?
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
                # export $usym
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

    # @deriveMeasure KiloGram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"
  end

  unitAbbreviations = []
  """
  @deriveMeasure_new KiloGramT(1)*MeterT(1)/SecondT(1)^2 = FigNewton(1) "FN"
  """
  macro deriveMeasure_new(relation, unit="NoUnit")
    # println("deriveMeasure_new($(string(relation)), $unit)")
    # @show a = Meta.parse("KiloGram*Meter/Second^2 = FigNewton(1.2)")
    # display(relation)

    isSimple = isa(relation, Expr)
    isSimple = isSimple && isa(relation.args[1],Expr)
    isSimple = isSimple && isa(relation.args[1].args[1], Symbol) # Gram
    isSimple = isSimple && typeof(relation.args[1].args[2]) <: Number # 1.2
    isSimple = isSimple && isa(relation.args[2],Expr) 
    isSimple = isSimple && isa(relation.args[2].args[2].args[1], Symbol)  # MegaGram
    isSimple = isSimple && typeof(relation.args[2].args[2].args[2]) <: Number # 1e-6

    #  :((KiloGram(1) * Meter(1)) / Second(1) ^ 2 = FigNewton(1)
    isCompound = !isSimple && length(relation.args) == 2 
    isCompound = isCompound && isa(relation.args[1],Expr) # &= doesn't do short-circuit evaluation
    isCompound = isCompound && isa(relation.args[1].args[2].args[2].args[1],Symbol) 
    # isCompound &= isa(relation.args[2],Expr) 
    # isCompound &= isa(relation.args[2].args[2].args[1], Symbol) 
    # isCompound = length(relation.args) == 2 && isa(relation.args[1],Expr) && isa(relation.args[1].args[2],Expr) && isa(relation.args[1].args[2].args[1], Symbol) 

    if isSimple
      # println("simple")
      lhsType = relation.args[1].args[1]
      lhsFactor = relation.args[1].args[2]
      rhsType = relation.args[2].args[2].args[1]
      rhsFactor = relation.args[2].args[2].args[2]

      usym = Symbol(unit) # convert from string into symbol

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

              # use the unit label as a funcional unit label: mm(1) => MilliMeter(1)
              $usym(num::Number) = $rhsType(num)
              # export $usym
          end
        )
      else
        @warn "$rhsType is already defined, cannot re-define"
      end
    elseif isCompound
      println("derived compound")
      rhsSymbol = relation.args[2].args[2].args[1] # FigNewton
      rhsFactor = relation.args[2].args[2].args[2] # 1.2

      abstractName = Symbol("Abstract"*String(rhsSymbol)) #AbstractLength
      usym = Symbol(unit) # convert from string into symbol
      
      # constructing the unit function from the full type names
      underFunctionString = replace(string(relation.args[1]), " "=>"", r"\(\d\)"=>"", "("=>"", ")"=>"", "/"=>"_per_", "*"=>"_", "^"=>"_power")  # args[1] = :((KiloGram * Meter) / Second ^ 2) => KiloGram_Meter_per_Second_power2
      underFunctionSymbol = Symbol(underFunctionString)

      # @show relation.args[1] #:((KiloGramT(1) * MeterT(1)) / SecondT(1) ^ 2) 
      ufs = replace(string(relation.args[1]), " "=>"", r"\(\d\)"=>"", "("=>"", ")"=>"")
      for subs in split(underFunctionString, "_")
        # if isdefined(parentmodule(@__MODULE__), Symbol(subs)) || isdefined(@__MODULE__, Symbol(subs)) #|| isdefined(__module__, Symbol(subs)) 
        if isdefined(__module__, Symbol(subs))
          println("$subs is defined")
          # inst = eval(Meta.parse(subs)) # get the type of str
          inst = __module__.eval(Meta.parse(subs)) # get the type of str
          @show inst(1.2)
          ufs = replace(ufs, subs=>inst(1).unit)
        else
          println("$subs is not defined")
        end
      end # ufs is correct, but since this is in the quote how can I get it interpolated into a function name?
      @show ufs
      global unitAbbreviations = push!(unitAbbreviations, (ufs, rhsSymbol)) 
      println(names(__module__))
      # println(names(@__MODULE__))

      # the problem is that the user knows the correlation between kg and KiloGram, but julia doesn't until the types are instantiated
      # unitFunctionString = underFunctionString

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

            # # make unit alias of the compound conversion function
            # ufs = $unitFunctionString
            # for subs in split($underFunctionString, "_")
            #   if isdefined(parentmodule(@__MODULE__), Symbol(subs)) || isdefined(@__MODULE__, Symbol(subs)) #|| isdefined(__module__, Symbol(subs)) 
            #     # println("$subs is defined, now replace in ufs")
            #     inst = eval(Meta.parse(subs)) # get the type of str
            #     global ufs = replace(ufs, subs=>inst(1).unit)
            #   end
            # end # ufs is correct, but since this is in the quote how can I get it interpolated into a function name?
            # # UnitTypes.Measure.unitAbbreviations = push!(UnitTypes.Measure.unitAbbreviations, (ufs, $rhsSymbol)) # fully specifying the list's name is key to sidestepping global and other scoping challenges

            # also make the abbreviated unit conversion: FN(num)
            $usym(num::Number) = $rhsSymbol(num)
            
            # can't convert to/from a compound type:
            # Base.convert(::Type{$rhsSymbol}, x::U) where {U<:supertype($lhsType)} = $rhsType(x.value*x.toBase/($lhsFactor/$rhsFactor)) # convert(MilliMeter, Meter()) 
            # Base.convert(::Type{$lhsType}, x::$rhsSymbol) = $lhsType(x.value*($lhsFactor/$rhsFactor)) # convert(Meter, MilliMeter(3))

            # isapprox between AbstractFigNewtons
            Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?

            Base.:*(x::T, y::U) where {T<:$abstractName, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T; this enables @show fn = FN(1.3)*3
            Base.:*(x::T, y::U) where {T<:Number, U<:$abstractName} = U(x*y.value) # enables @show fn = 4*FN(1.3)
          end
        )
      else
        @warn "$rhsSymbol is already defined, cannot re-define"
      end
    #   @show KiloGram_Meter_per_Second_power2(1.2) # testing
    else
      println("neither")
    end
  end

  @testitem "deriveMeasure_new" begin
    @testset "simple relations" begin
      @deriveMeasure_new Meter(1) = Milli(1000) "milli"
      @test Milli(Meter(1.2)) ≈ Milli(1200)
      @test Meter(Milli(1200)) ≈ Meter(1.200)
      # @test isa(milli(1.3), UnitTypes.Meter) # this is too strong, since a test-created type is not exactly the same as one defined in-package...?  work around by:
      @test typeof(milli(1.3)) <: AbstractLength
      @test typeof(1.4w"milli") <: AbstractLength
    end

    # @testset "compound relation" begin # if I surround these with this begin/end, kgT, mT, sT aren't added to Main.var##236##, so it's like @__MODULE__ can't see out of the testset s.t. isdefined(kgT) = false above. feels like a bug
      display(fullname(@__MODULE__))
      @makeBaseMeasure MassT KiloGramT "kgT"
      @makeBaseMeasure LengthT MeterT "mT"
      @makeBaseMeasure TimeT SecondT "sT"
      @deriveMeasure_new KiloGramT(1)*MeterT(1)/SecondT(1)^2 = FigNewton(1) "FN"
      # display(names(UnitTypes))
      # display(@__MODULE__)
      # display(names(@__MODULE__))
      @test KiloGramT_MeterT_per_SecondT_power2(1.2) ≈ FigNewton(1.2)
      @show FN(1.3)*3 ≈ FigNewton(3.9)
      @show 4*FN(1.3) ≈ FigNewton(5.2)
      @show 1.5u"FN" ≈ FigNewton(1.5)
      @show 1.5w"kg*m/s^2" ≈ FigNewton(1.5)
    # end
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
    # # if contains(unit, r"[*/^]")
    #   usym = Symbol(unit) # convert string into a Symbol
    # # else
    # #   unit = replace(unit, "*"=>"_", "/"=>"_per_", "^"=>"_power") # assumes functions like Kg_m_per_s_power2()::Newton exist, these are added by relateMeasures
    # # end
    # return esc(:($usym(1))) # return unit(1) which is implicitly * with the leading number

    for abb in unitAbbreviations
      @show abb
      if abb[1] == unit
        return __module__.eval(:($(abb[2])(1)))
      end
    end
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

  macro w_str(unit::String)
    isCompound = contains(unit, r"[*/^]")
    isSimple = !isCompound
    if isSimple
      println("$unit isSimple")
      usym = Symbol(unit) # convert string into a Symbol
      return esc(:($usym(1))) # return unit(1) which is implicitly * with the leading number
    end
    if isCompound
  # this is not used as UnitTypes already provides it
      println("$unit isCompound")
      @show underFunctionString = replace(string(unit), " "=>"", r"\(\d\)"=>"", "("=>"", ")"=>"", "/"=>"_per_", "*"=>"_", "^"=>"_power")  # args[1] = :((KiloGram * Meter) / Second ^ 2) => KiloGram_Meter_per_Second_power2
      @show underFunctionSymbol = Symbol(underFunctionString)
      return esc(:($underFunctionSymbol(1))) # return unit(1) which is implicitly * with the leading number
    end
    # else
    #   unit = replace(unit, "*"=>"_", "/"=>"_per_", "^"=>"_power") # assumes functions like Kg_m_per_s_power2()::Newton exist, these are added by relateMeasures
    # end
  end



end