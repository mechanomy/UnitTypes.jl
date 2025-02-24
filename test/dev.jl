module UnitTypesDev
  using UnitTypes

  macro relateMeasures(relation, unit="NoUnit")
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

    if length(lhs.args) == 2 # Meter(10) => [Symbol Meter][Int64 10]
      # @show dump(lhs) #:
      # Expr
      #   head: Symbol call
      #   args: Array{Any}((2,))
      #     1: Symbol Meter
      #     2: Int64 10

      lhsSymbol = lhs.args[1]
      lhsFactor = lhs.args[2]

      push!(qts, quote 
        Base.convert(::Type{$rhsSymbol}, x::U) where {U<:supertype($lhsSymbol)} = $rhsSymbol(x.value*x.toBase/($lhsFactor/$rhsFactor)) # convert(MilliMeter, Meter())
        Base.convert(::Type{$lhsSymbol}, x::$rhsSymbol) = $lhsSymbol(x.value*($lhsFactor/$rhsFactor)) # convert(Meter, MilliMeter(3))
      end)

      return esc(Expr(:block, qts...))
    end

    if length(lhs.args) == 3 # CentiMeter(1)*KiloNewton(1) => [Symbol *][Expr ..][Expr ..]
      # @show dump(lhs) #:
      # Expr
      #   head: Symbol call
      #   args: Array{Any}((3,))
      #     1: Symbol *
      #     2: Expr
      #       head: Symbol call
      #       args: Array{Any}((2,))
      #         1: Symbol KiloNewton
      #         2: Int64 1
      #     3: Expr
      #       head: Symbol call
      #       args: Array{Any}((2,))
      #         1: Symbol CentiMeter
      #         2: Int64 1
      @show op = lhs.args[1]
      @show lhsSymbolA = lhs.args[2].args[1]
      @show lhsFactorA = lhs.args[2].args[2]
      @show lhsSymbolB = lhs.args[3].args[1]
      @show lhsFactorB = lhs.args[3].args[2]

      # if lhsSymbolA isdefined or is in unitAbbreviations...
      # @show op, typeof(op), op == :*
      if op == :*
        push!(qts, quote 
          Base.:*(a::$lhsSymbolA, b::$lhsSymbolB) = $rhsSymbol(a.value*a.toBase * b.value*b.toBase) # 2kN * 3cm = 2*1000 * 3*0.01 = 60 Nm
          Base.:*(b::$lhsSymbolB, a::$lhsSymbolA) = a * b
        end) 
      # elseif op == :/
      #   push!(qts, quote 
      #     Base.:/(a::$lhsSymbolA, b::$lhsSymbolB) = $rhsSymbol(a.value/$lhsFactorA / b.value/$lhsFactorB * $rhsFactor)
      #     Base.:/(b::$lhsSymbolB, a::$lhsSymbolA) = $rhsSymbol(b.value/$lhsFactorB / a.value/$lhsFactorB * $rhsFactor)
      #   end) 
      else
        println("noop, $op")
      end

      # display(qts)
      return esc( Expr(:block, qts...))
    end

    # @relateMeasures MilliMeterT(25.4) = InchT(1) "inT"
    # @relateMeasures Meter(1)*Meter(1) = MeterSq(1) "msq"


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

  @relateMeasures KiloNewton(1)*CentiMeter(1) = FigNewtonMeter(10) "fNm"
  @show KiloNewton(1)*CentiMeter(1) ≈ FigNewtonMeter(10)
  @show CentiMeter(1)*KiloNewton(1) ≈ FigNewtonMeter(10)

  @relateMeasures Meter(10) = DeciMeter(1) "dm" # decimeter is new
  # @show DeciMeter(3) + Meter(1)

  # @relateMeasures CentiMeter(1)*Meter(π) = MeterSq(1) "msq"
  # @relateMeasures Gram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"

  # @relateMeasures Meter(1)*Degree(1) = MeterSq(1) "msq"
  # @show MeterSq(1.2)
  # @show Meter(1)*Degree(2)
  
  # not implemented @relateMeasures Gram(1)*Meter(1)/Second(1)^2 = FigNewton(1) "FN"


  # @relateMeasures KiloNewton(2)*CentiMeter(3) = FigNewtonMeter(2000*(3/100)) "fNm" I'd like this to work...
  # @relateMeasures Meter(1)*Meter(1) = MeterSq(1) "msq"
  # @relateMeasures MeterSq(1)*Meter(1) = MeterTri(1) "mtr"
  # @relateMeasures MeterSq(1)/Second(1) = MeterSqPerSec(1) "msq/s"
  
  # @relateMeasures MeterTri(1)*Meter(1) = MeterQuad(1) "mqu"
  # @show MeterQuad(1.2)
  # @show MeterSq(1)*Meter(1)
  # @show Meter(1)*MeterSq(1)
  # @relateMeasures MeterSq(1)*MeterSq(1) = MeterQuad(1) "mqu"



end
;
