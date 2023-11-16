module ExchangeUnitful
  using TestItemRunner
  using ..Measure

  import Unitful


  # Base.convert(::Type{T}, x::U) where {T<:Unitful.Unitlike, U<:AbstractMeasure} = x.value * Unitful.uparse(x.unit) # Unitful.Unitlike is for Unitful.m
  # Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:Unitful.Unitlike} = T(Unitful.uconvert(Unitful.uparse(T(1.0).unit), x))
  Base.convert(::Type{T}, x::U) where {T<:Unitful.AbstractQuantity, U<:AbstractMeasure} = x.value * Unitful.uparse(x.unit) # Unitful.AbstractQuantity is for typeof(1u"m")
  Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:Unitful.AbstractQuantity} = T(Unitful.ustrip(Unitful.uparse(T(1.0).unit), x))

  
  # NewtonMeter(uf::T) where T<:Unitful.AbstractQuantity = convert(NewtonMeter, uf) #because it is more specific this should take precedence over NewtonMeter(::Number(Unitful))
  # export NewtonMeter

  @testitem "ExchangeUnitful" begin
    using Unitful

    #assemble a list of concrete types
    utNames = names(UnitTypes, all=false, imported=false)
    filter!(n->Base.isconcretetype(eval(Symbol(string(n)))), utNames) #filter abstract types
    for ut in utNames
    # ut = utNames[10]
      unitType = eval(ut)# eval needed to convert the symbol into the type
      a = unitType(1.2) 
      try 
        Unitful.uparse(a.unit)
      catch err
        println("Unitful.uparse unable to parse UnitType symbol [$(a.unit)] for measure $(typeof(a)), skipping test.")
      else
        @testset "Convert $ut to Unitful" begin
          b = 1.2 * Unitful.uparse(a.unit)
          @test convert(typeof(b), a) ≈ b
        end
        @testset "Convert Unitful to $ut" begin
          b = 1.2 * Unitful.uparse(a.unit)
          @test convert(unitType, b) ≈ a
        end
        @testset "Constructing $ut from Unitful" begin
          wrong = unitType( 1.2 * Unitful.uparse(a.unit) )
          @test !(typeof(wrong.value) <: Unitful.AbstractQuantity)
          @test typeof(wrong.value) <: Real
          @test typeof(wrong.value) <: Float64
        end
      end
    end
  end

  @testitem "check double units" begin
    using Unitful
    # I'm seeing:
    # julia> using UnitTypes, Unitful
    # julia> a = 1.2u"N*m"
    # 1.2 m N
    # julia> b = NewtonMeter(a)
    # 1.2 m NN*m
    # where UnitTypes.NewtonMeter(Unitful.Nm) should convert rather than just m NN*m.
    # This is due to Unitful subtyping Number
    uf = 1.2u"N*m"
    # @show typeof(uf)
    # @show supertype(typeof(uf))
    # ut = NewtonMeter(1.2)
    # wrong = NewtonMeter(uf)
    # @show typeof(wrong.value)
    # @show typeof(wrong.value) == typeof(uf)
    # @test !(typeof(wrong.value) <: Unitful.AbstractQuantity)
    # @test typeof(wrong.value) <: Real
    # @test typeof(wrong.value) <: Float64
    # imho Unitful shouldn't subtype Number; I could restrict value::Real because I don't think I can value::Number(exceptUnitful)...
    # and because the problem is in the struct rather than convert(), I'd have to add Unitful type checking there..?  Could I make additional constructors for everything here?
    # UTNM(uf::T) where T<:Unitful.AbstractQuantity = convert(NewtonMeter, uf)
    # utnm = UTNM(uf)
    # @show utnm
    # @show typeof(utnm)
    # @show typeof(utnm.value)
  end

end