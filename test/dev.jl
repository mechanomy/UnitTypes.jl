module UnitTypesDev
  using UnitTypes

  # make unit labels functions? mm(1.2) is /functional/...  this is an easy add to the existing macros
  # ..and it makes it easy to avoid a list of units and instead do return mm(1) to get Meter(1)
  # but this will fail in the literal mapping of m/s => m/s(3.4), unless having m(), /, and s(3.4) defined it then does the math between the units...
  # how about making lengthy function names,  Kg*m/s^2 -> Kg_m_per_s_power2? It breaks if the units are reordered, but that may be able to be gotten around.  The nice thing is that this is much more likely to be unique and by not relying on the order of operations rules I can directly go from unit to function

  function dev241111()
    # Expanding deriveMeasure to include compound expressions requires taking KiloGram(1)*Meter(1)/Second(1)^2 = FigNewton(1) and
    # ] identify new unit: FigNewton "FN"
    # ] parse and construct the lhs
    # ] create the unit

    # @show a = Meta.parse("KiloGram(1)*Meter(1)/Second(1)^2 = FigNewton(1) \"FN\"")
    @show a = Meta.parse("KiloGram*Meter/Second^2 = FigNewton(1)")
    ## deriveMeasure
    @show a.args[1]
    @show underFunctionName = replace(string(a.args[1]), " "=>"", "("=>"", ")"=>"", "/"=>"_per_", "*"=>"_", "^"=>"_power")
    @show fn = Symbol(underFunctionName)
    eval( :( $fn(n::Number) = Newton(n) )) # establish the function
    @show KiloGram_Meter_per_Second_power2(1.2) # testing
    # so if the compound string to function name is stable, then @deriveMeasure KiloGram*Meter/Second^2 goes to K...()
    # but how do I expand u"Kg*m/s^2" into KiloGram?
    # I think I have to create a second Kg_m_per_s_power2() = KiloGram_m_per_s_power2() when I have access to the unit strings...
    # While I could walk through ast, safer is to stick to the string:
    ## u_str
    @show b = split(underFunctionName, "_")
    cunit = ""
    for str in b
      if isdefined(UnitTypes, Symbol(str)) # is str currently defined? omit per, power, handle later
        ut = eval(Meta.parse(str)) # get the type of str
        if ut <: AbstractMeasure # is the type a UnitType::AbstractMeasure?
          global cunit = cunit * ut(1).unit * "*" # get the unit string from the UnitType, assume multiplication
        end
      else
        if str == "per"
          global cunit = cunit[1:length(cunit)-1] # remove *
          global cunit = cunit * "/"
        elseif occursin("power",str)
          global cunit = cunit[1:length(cunit)-1] # remove *
          m = match(r"\d+", str)
          global cunit = cunit * "^" * m.match
        else
          println("$str not understood")
        end
      end
    end
    @show cunit

    # KiloGramMeterPerSecondPower2(n) = FigNewton(1)
  end
  # dev241111()


  #=
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

      # construct an alias with the unit abbreviations:
      # this is inelegant: split the underFunctionString by _ and if the substring is <: abstractMeasure, instantiate to get the unit and add that to the unitFunctionString
      unitFunctionString = underFunctionString
      for subs in split(underFunctionString, "_")
        println("got $subs")
        # @show Symbol(subs)
        # @show getfield(UnitTypesDev, Symbol(subs))
        if isdefined(UnitTypesDev, Symbol(subs))
          println("is defined, now replace")
          inst = eval(Meta.parse(subs)) # get the type of str
          # @show inst(1.5).unit
          unitFunctionString = replace(unitFunctionString, subs=>inst(1).unit)
        end
      end
      @show unitFunctionSymbol = Symbol(unitFunctionString)

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
            $unitFunctionSymbol(n::Number) = $underFunctionSymbol(n)

            # also make the abbreviated unit conversion: FN(num)
            $usym(num::Number) = $rhsSymbol(num)
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

  @deriveMeasure_new KiloGram(1) = MegaGram(1e-3) "MG"
  @deriveMeasure_new KiloGram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"
  @show names(UnitTypesDev, all=true) # checking that AbstractFigNewton is created

  Base.:*(x::T, y::U) where {T<:AbstractFigNewton, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T; this enables @show fn = FN(1.3)*3
  Base.:*(x::T, y::U) where {T<:Number, U<:AbstractFigNewton} = U(x*y.value) # enables @show fn = 4*FN(1.3)
  function dev241112()
    # @show fn = MegaGram(1.2) # testing
    # @show KiloGram(MegaGram(1.2))
    # @show MG(1.3)
    # @show 1.4u"MG"

    # @show fn = KiloGram_Meter_per_Second_power2(1.2) 
    # @show fn = FN(1.3)*3
    # @show fn = 4*FN(1.3)
    # @show 1.5u"FN"
    @show 1.5w"kg*m/s^2"

  end
  # dev241112()
  =#

  # abstract type expression is not at top level if these are made in a function
  @makeBaseMeasure MassT KiloGramT "kgT"
  @makeBaseMeasure LengthT MeterT "mT"
  @makeBaseMeasure TimeT SecondT "sT"

  @deriveMeasure_new KiloGramT(1)*MeterT(1)/SecondT(1)^2 = FigNewton(1) "FN"
  function dev241125()
    @show FigNewton(1.2)
    # @show kg_m_per_s_power2(2.3) # not defined
    @show KiloGramT_MeterT_per_SecondT_power2(1.3)
    # @show kgT_mT_per_sT_power2(1.4) not defined
    # so did it make anything? where is kgT_mT_per_sT_power2()?
    # println(names(UnitTypesDev, all=true)) 
    # println(names(UnitTypes, all=true))
    # @show kgT_mT_per_sT_power2(1.4)
    @show UnitTypes.Measure.unitAbbreviations 
    @show 1.2u"kgT*mT/sT^2"
    @show 1.6u"km"

  end
  dev241125()


end
;