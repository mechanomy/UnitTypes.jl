module UnitTypesDev
  using UnitTypes

  struct DefinedUnit # this is not very different from the actual macro definition, only that this doesn't need to be instantiated to be accessed..?
    name::String
    abbreviation::String
    unit::Type
  end
  definedUnits = [DefinedUnit("Meter","m",Meter), DefinedUnit("Second","s", Second)]#,  DefinedUnit("Gram","g",Gram) ]

  function isDefined(unit::Symbol)
    # @show definedUnits
    for du in definedUnits
      if Symbol(du.name) == unit
        return true
      # else
      #   println("$unit is un")
      end
    end
    return false
  end

  macro deriveDFS(relation, unit="NoUnit")
    println("\n")
    # display(dump(relation))

    function recursiveIsdef(mod, rel, ndef, nund) # walk through an Expr, counting defined & undefineds to determine isCompound
      println("")
      if rel isa Symbol && Base.isidentifier(rel)
        # if isdefined(mod, rel) # this is true for π, when what I really want isdefined as a UnitTypes 
        # if  <: AbstractMeasure , how to make the symbol into a type name? https://discourse.julialang.org/t/parse-string-to-datatype/7118/7 recommends a name->type table to bypass eval(parse), and this would allow me to skip the eval() in Measure.jl where I get the units.  I already have this in unitAbbreviations?
        # if rel in names(mod)
        @show rel
        if isDefined(rel)
          ndef+=1
          println("$rel is defined Symbol, ndef $ndef")
        else
          nund+=1
          println("$rel is undefined Symbol, nund $nund")
        end
        return (ndef, nund)
      elseif rel isa Expr
        for arg in rel.args
          a = recursiveIsdef(mod, arg, ndef, nund)
          ndef += a[1]
          nund += a[2]
          # println("recursive return, ndef $ndef, nund $nund")
        end
        return (ndef, nund)
      else
        println("else $rel is " * string(typeof(rel)))
        return (0,0)
      end
    end

    if relation.head == Symbol("=")
      @show lhs = recursiveIsdef(__module__, relation.args[1],0,0)
      @show rhs = recursiveIsdef(__module__, relation.args[2],0,0)
    end
  end
  # @deriveDFS Meter(1)*Meter(π) = MeterSq(1) "msq"
  # @deriveDFS Gram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"
  # @deriveDFS Meter(mPerIn*inPerFt) = Foot(1) "ft"

  macro deriveExprs(relation, unit="NoUnit")
    println("\n")
    # @show dump(relation) # the parsed Expr


    rhs = skipSymbolBlock(relation.args[2])
    # @show dump(rhs)
    # rhs will always have the same format (unless a symbol or variable is used for the argument)
    rhsSymbol = rhs.args[1]
    rhsFactor = rhs.args[2]
    rhsAbstractName = Symbol("Abstract"*String(rhsSymbol)) #AbstractLength
    rhsUnit = unit

    qts = [ quote # Make the new type
        abstract type $rhsAbstractName <: AbstractMeasure end
        export $rhsAbstractName #AbstractLength

        # """
        #   UnitType [`$($rhsSymbol)`](@ref) is derived from [`$($lhsType)`](@ref), related by [`$($lhsFactor/$rhsFactor)`](@ref), with supertype [`$(supertype($lhsType))`](@ref), and symbol `$($unit)`.
        # """
        struct $rhsSymbol <: $rhsAbstractName # how does the new type relate to other types? is it just, and only <:AbstractMeasure?
          value::Number
          toBase::Number
          unit::String
          $rhsSymbol(x::Number) = new(x, 1/$rhsFactor, $unit)
        end
        export $rhsSymbol

        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$rhsAbstractName, U<:$rhsAbstractName} = isapprox(convert(T,x).value, convert(T,y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?
        Base.:*(x::T, y::U) where {T<:$rhsAbstractName, U<:Number} = T(x.value*y) # * inside T because x*T(y) = Meter^2; toBaseFloat not needed since x.value is already T; this enables @show fn = FN(1.3)*3
        Base.:*(x::T, y::U) where {T<:Number, U<:$rhsAbstractName} = U(x*y.value) # enables @show fn = 4*FN(1.3)
      end ]
    println("rhs new unit has symbol[$rhsSymbol] factor[$rhsFactor] unit[$rhsUnit] and abstract[$rhsAbstractName]")

    # lhs has variable structure depending on the derivation
    lhs = skipSymbolBlock(relation.args[1])
    @show dump(lhs)

    @show op = lhs.args[1]
    @show lhsSymbolA = lhs.args[2].args[1]
    @show lhsFactorA = lhs.args[2].args[2]
    @show lhsSymbolB = lhs.args[3].args[1]
    @show lhsFactorB = lhs.args[3].args[2]

    # if lhsSymbolA isdefined or is in unitAbbreviations...
    # @show op, typeof(op), op == :*
    if op == :*
      push!(qts, quote 
        Base.:*(x::$lhsSymbolA, y::$lhsSymbolB) = $rhsSymbol(x.value/$lhsFactorA * y.value/$lhsFactorB * $rhsFactor)
        Base.:*(x::$lhsSymbolB, y::$lhsSymbolA) = $rhsSymbol(x.value/$lhsFactorA * y.value/$lhsFactorB * $rhsFactor)
      end) 
    else
      println("noop, $op")
    end

    # display(qts)
    return esc( Expr(:block, qts...))

    # @deriveExprs MilliMeterT(25.4) = InchT(1) "inT"
    # @deriveExprs Meter(1)*Meter(1) = MeterSq(1) "msq"


    if relation isa Expr
      # defineExpr(relation)
      # :(@defineExpr $(esc(relation)))
      eexp = relation
      println("defineExpr($(eexp.head) isa $(typeof(eexp.head))")
      
      if eexp.head == :(=)
        println("yes is = ")

        # the rhs must not be defined, define it here before creating the creating conversions on the lhs:
        println("\ndefining rhs:")
        rhs = skipSymbolBlock(eexp.args[2])
        # println("defining $(rhs.args[1])")


        @show dump(rhs)
        rhsSymbol = rhs.args[1]
        rhsFactor = rhs.args[2]
        abstractName = Symbol("Abstract"*String(rhsSymbol)) #AbstractLength
        unit = "justMade"

        #now I can't just return with the rhs definition because in the same quote I also need to define any lhs additions
        println("\nchecking lhs:")
        # working on the lhs, I need to ensure that all Exprs within it exist and have */
        lhs = skipSymbolBlock(eexp.args[1])
        # @show dump(lhs)
        # defineExpr(lhs)

      elseif eexp.head == :call # if not = but a call, we're on the lhs and working on the unit's construction
        # Expr
        #   head: Symbol call
        #   args: Array{Any}((3,))
        #     1: Symbol *
        #     2: Expr
        #       head: Symbol call
        #       args: Array{Any}((2,))
        #         1: Symbol Meter
        #         2: Int64 1
        #     3: Expr
        #       head: Symbol call
        #       args: Array{Any}((2,))
        #         1: Symbol Meter
        #         2: Int64 1

        @show isTerminalExprA = eexp.args[2].head == :call && eexp.args[2].args[1] isa Symbol && typeof(eexp.args[2].args[2])<:Number
        @show isTerminalExprB = eexp.args[3].head == :call && eexp.args[3].args[1] isa Symbol && typeof(eexp.args[3].args[2])<:Number
        @show isLeaf = eexp.args[1] == :(*) && isTerminalExprA && isTerminalExprB
        if isLeaf
          @show typea = __module__.eval(eexp.args[2].args[1]) # avoid eval?
          @show typeb = __module__.eval(eexp.args[3].args[1])

          # hasmethod(*, Tuple{Meter,Meter}) = true,
          # hasmethod(*, Tuple{:Meter,:Meter}) = false,
          hasMultiply = hasmethod(*, Tuple{typea, typeb})
          hasDivide = hasmethod(/, Tuple{typea, typeb})
          hasExponent = hasmethod(^, Tuple{typea, typeb})

          if eexp.args[1] === :(*) && !hasMultiply
            println("defining * for $typea, $typeb")

          elseif eexp.args[1] === :(/) && !hasDivide
            println("defining / for $typea, $typeb")

          elseif eexp.args[1] === :(^) && !hasExponent
            println("defining ^ for $typea, $typeb")

          else
            println("operation already defined for $typea, $typeb")
          end
          #the lhs is now defined 
        else
          println("else not leaf")
        end
      else
        println("else not =")
      end

      else
        println("Unhandled relation type: $(typeof(relation))")
        display(dump(relation))
      end
  end

  function skipSymbolBlock(eexp)
    if eexp.head == :block
      return eexp.args[2]
    else
      return eexp
    end
  end

  # @deriveExprs CentiMeter(1)*Meter(π) = MeterSq(1) "msq"
  # @deriveExprs Gram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"

  # @deriveExprs Meter(1)*Degree(1) = MeterSq(1) "msq"
  # @show MeterSq(1.2)
  # @show Meter(1)*Degree(2)
  
  @deriveExprs Gram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"
  @deriveExprs Meter(1)*Meter(1) = MeterSq(1) "msq"
  @deriveExprs MeterSq(1)*Meter(1) = MeterTri(1) "mtr"
  # @deriveExprs MeterTri(1)*Meter(1) = MeterQuad(1) "mqu"
  # @show MeterQuad(1.2)
  # @show MeterSq(1)*Meter(1)
  # @show Meter(1)*MeterSq(1)
  # @deriveExprs MeterSq(1)*MeterSq(1) = MeterQuad(1) "mqu"



  macro defineLHS( expr )
    # since macros have one return and @deriveExprs() wants to define every missing expr on the lhs, call this from deriveExprs
    
    @show dump(expr)

  end
  # @defineLHS Meter(1)*KiloGram(1.2)
  # @defineLHS(Expr(*, Expr(:call,Meter,1), Expr(:call,KiloGram,1.2)) )


end
;