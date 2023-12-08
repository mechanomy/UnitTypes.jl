module UnitTypesDev

  abstract type AbsNum end
  struct MyNum <: AbsNum
    value::T
  end
  abstract type AbsWrap end
  struct WrapA{T<:AbsNum} <: AbsWrap
    value::T
  end
  struct WrapB{T<:AbsNum} <: AbsWrap
    value::T
  end

  macro addConverts( TypeA, TypeB )
    return esc(
      quote
        Base.:+(x::T,y::U) where {T<:supertype($TypeA), U<:AbsNum}
      end
    )
  end

end

#=
module UnitTypesCompound
  using UnitTypes

  newton = :(Kilogram * Meter / Second ^ 2)
  @show newton # = newton = :((Kilogram * Meter) / Second ^ 2)
  @show newton.args # = newton.args = Any[:/, :(Kilogram * Meter), :(Second ^ 2)]
  # @show isa(newton.args[2], Symbol)
  # @show typeof(newton.args[2])
  # @show newton.args[2]
  # @show newton.args[2].args

  kgpm3 = :(Kilogram / Meter ^ 3)
  @show kgpm3 # = kgpm3 = :(Kilogram / Meter ^ 3)
  @show kgpm3.args # = kgpm3.args = Any[:/, :Kilogram, :(Meter ^ 3)]
  struct WrapA{T<:AbsNum} 
    value::T
  end

  macro addConverts

end

#=
module UnitTypesCompound
  using UnitTypes

  newton = :(Kilogram * Meter / Second ^ 2)
  @show newton # = newton = :((Kilogram * Meter) / Second ^ 2)
  @show newton.args # = newton.args = Any[:/, :(Kilogram * Meter), :(Second ^ 2)]
  # @show isa(newton.args[2], Symbol)
  # @show typeof(newton.args[2])
  # @show newton.args[2]
  # @show newton.args[2].args

  kgpm3 = :(Kilogram / Meter ^ 3)
  @show kgpm3 # = kgpm3 = :(Kilogram / Meter ^ 3)
  @show kgpm3.args # = kgpm3.args = Any[:/, :Kilogram, :(Meter ^ 3)]
  
  # @show isa(kgpm3.args[1], Symbol)
  # @show kgpm3.args[1] == :/

  # so in args I have the construction in prefix notation, which I need to map into addUnitOperations
  println("\nstart parsing")
  function recurseCompound(com)
    for ia in eachindex(com.args)
      if com.args[ia] == :/
        if typeof(com.args[ia+1]) <: AbstractMeasure && com.args[ia+2] <: AbstractMeasure
          println("is /, run @unitDivide $(com.args[ia+1]) $(com.args[ia+2])")
        else
          println("is /, sort out $(com.args[ia+1]), $(com.args[ia+2])")
          # recurseCompound(com[ia+1])
          # recurseCompound(com[ia+2])
          #...and then how to add the /? somehow I need to get a * b
        end
      elseif com.args[ia] == :*
        if typeof(com.args[ia+1]) <: AbstractMeasure && com.args[ia+2] <: AbstractMeasure
          println("is /, run @unitProduct $(com.args[ia+1]) $(com.args[ia+2])")
        else
          println("is /, sort out $(com.args[ia+1]), $(com.args[ia+2])")
        end
      elseif isa(com.args[ia], Expr)
        println("is Expr $(com.args[ia]), recurse")
      end
    end
  end
  recurseCompound(newton)
end
#=#

#=
#print the type tree
  using UnitTypes
  import InteractiveUtils
  import AbstractTrees
  AbstractTrees.children(d::DataType) = InteractiveUtils.subtypes(d)
  AbstractTrees.print_tree(AbstractMeasure)
=#


#=
module UnitTypesAlternatives
  abstract type AbstractLength end

  struct Meter <: AbstractLength
    value::Number
    toBase::Number
    unit::String
    Meter(x::Number) = new(x, 1, "m")
  end
  @show Meter(1.23)
  @show typeof(Meter(1.23))

  struct Centimeter <: AbstractLength
    value::Number
    toBase::Number
    unit::String
    Centimeter(x::Number) = new(x, 1e-2, "cm")
  end
  @show Centimeter(1.23)
  @show typeof(Centimeter(1.23))
  
  
  ### alternate of unique constructors
  struct Length <: AbstractLength
    value::Number
    toBase::Number
    unit::String
  end 

  Millimeter(x::Number) = Length(x, 1e-3, "mm")
  @show Millimeter(1.23)
  @show typeof(Millimeter(1.23))

  ### alternate of embedded types
  struct Len <: AbstractLength
    value::Number
    toBase::Any
    unit::String
  end 
  Nanometer(x::Number) = Len(x, Meter(1e-9), "nm")
  @show Nanometer(1.23)
  @show typeof(Nanometer(1.23))

  ### alternates of parametric types
end ;
=#
;