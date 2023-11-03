module AbsDimension
  using TestItems 

  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths
  using ..AbsExtent
  
  ## CONCEPTS - These terms describe the type of measurement. A CONCEPT necessarily includes a MEASURE, as an undimensional Diameter makes no sense.
  export AbstractDimension, @makeDimension
  abstract type AbstractDimension <: AbstractMeasure end 

  # `@makeDimension Diameter AbstracDiameter` will create:
  # struct Diameter{T <:AbstractExtent} <: AbstractDiameter
  #   value::T
  # end
  macro makeDimension(name, abstractType) # a dimension is inherently a contextualized Extent
    esc(
      quote
        struct $name{T <: AbstractExtent } <: $abstractType
          value::T
        end
      end
    )
  end

  # I'd like a 'permute operation' to do the drudgery of below..?
  # macro permuteOperation(operation, ::AbsA, ::AbsB)
  #   esc(
  #     quote
  #       $operation(a::AA, b::BB) where {AA<:AbsA, BB<:AbsB} = operation( AA.value?, BB.value?) # is there a way around this?
  #     end
  #   )
  # end
  # @macroexpand @permuteOperation( Base.:+, AbstractDimension, Number)

  Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value+y )
  Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value-y )
  Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y-x.value )
  Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value*y )
  Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value/y )
  Base.:/(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y/x.value )

  Base.:+(x::T, y::T) where T<:AbstractDimension = T( x.value + y.value)
  # Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractMeasure} = x + T(y) 
  Base.:-(x::T, y::T) where T<:AbstractDimension = T( x.value - y.value)
  # Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractMeasure} = x - T(y) 
  # Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:AbstractMeasure} = T(y) - x

  #what sense does this make?
  Base.:*(x::T, y::T) where T<:AbstractDimension = T( x.value * y.value)
  Base.:/(x::T, y::T) where T<:AbstractDimension = T( x.value / y.value)

  # concepts = [Diameter, Radius, Length, Height, Width, Depth]
  # measures = [Meter, MilliMeter, Inch, Foot]
  # @testset "Dimensions" begin
  #   for C in concepts
  #     for M in measures
  #       @testset "checking $C{$M}" begin
  #         d = C{M}(3.4)
  #         @test typeof(d) <: AbstractDimension
  #         @test typeof(d) <: C
  #         @test convert(Float64, d) ≈ 3.4
  #         @testset "Dimension + Number" begin
  #           @test isapprox( C{M}(1.2) + 0.1, 1.3, rtol=1e-3)
  #           @test isapprox( C{M}(1.2) - 0.1, 1.1, rtol=1e-3)
  #           @test isapprox( 0.1 - C{M}(1.2), -1.1, rtol=1e-3)
  #           @test isapprox( C{M}(1.2) * 0.1, 0.12, rtol=1e-3)
  #           @test isapprox( C{M}(1.2) / 0.1, 12, rtol=1e-3)
  #           @test isapprox( 0.1 / C{M}(1.2), 0.08333, rtol=1e-3)
  #         end
  #         @testset "Dimension + Dimension" begin
  #           @test isapprox( C{M}(1.2) + C{M}(0.1), C{M}(1.3), rtol=1e-3)
  #           @test isapprox( C{M}(1.2) - C{M}(0.1), C{M}(1.1), rtol=1e-3)
  #           @test isapprox( C{M}(1.2) * C{M}(0.1), C{M}(0.12), rtol=1e-3)
  #           @test isapprox( C{M}(1.2) / C{M}(0.1), C{M}(12), rtol=1e-3)
  #         end
  #       end
  #     end
  #   end




  Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:AbstractDimension} = typeof(x).parameters[1](x.value) # if T is a Diameter, it already has an AbstractMeasure; this convert simply returns that # D.M -> M
  Base.convert(::Type{T}, x::U) where {T<:AbstractDimension, U<:AbstractMeasure} = throw(ErrorException("UnitTypes.jl cannot convert() Measure [$U] into Dimension [$T], use the Dimension's constructor directly.")) # M -> D.M should fail
  Base.convert(::Type{T}, x::U) where {T<:AbstractDimension, U<:AbstractDimension} = T( convert(T.parameters[1], x)) # D.M -> D.MM
  # Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = T(x.value) ambiguous with above float64,
  # Base.convert(::Type{Float64}, x::U) where {U<:AbstractDimension} = convert(Float64,x.value)  #works
  # Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = convert(T, x.value) 
  # Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = convert(T, x) 
  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = convert(T, convert(U.parameters[1], x) ) #convert first to Measure, then to number...this works fine in terminal
  # Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = T(convert(U.parameters[1], x) ) #
  
  @testitem "AbsDimension convert()s" begin
    @testset "Dimension to Measure " begin
      d = Diameter{Meter}(3.4)
      @test convert(Meter, d) ≈ Meter(3.4)
      @test convert(MilliMeter, d) ≈ MilliMeter(3400)
      @test isapprox( d, Meter(3.4), rtol=1e-3 )
      @test isapprox( Meter(3.4), d, rtol=1e-3 )
    end

    @testset "Measure units within a Dimension" begin
      @test convert(Diameter{MilliMeter}, Diameter(Meter(3.4))) ≈ MilliMeter(3400) # convert underlying Meter to MilliMeter
    end

    # @testset "do not convert() Measure to Dimension" begin
    #   @test_throws ErrorException convert(UnitTypes.Diameter{UnitTypes.Meter}, UnitTypes.Meter(3.4))
    # end
  end
  


  # is there a clean lower() function, stripping the concept from the extent?

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractDimension} = isapprox(x.value, convert(T,y).value, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractExtent} = isapprox(x.value, convert(T.parameters[1],y).value, atol=atol, rtol=rtol)

  """
  """
  function concept2string(c::T)::String where T<:AbstractDimension
    return "$(split(string(T),"{")[1])($(convert(T.parameters[1],c)))" #ugly parsing of typestring...but not seeing an alternative
  end

  """
  """
  function Base.show(io::IO, c::T) where T<:AbstractDimension
    print(io, concept2string(c))
  end



  export AbstractDiameter, Diameter, Radius
  abstract type AbstractDiameter <: AbstractDimension end # things relating to circular diameter
  @makeDimension Diameter AbstractDiameter
  @makeDimension Radius AbstractDiameter
  # Base.convert(::Type{Diameter}, x::Radius) = Diameter{typeof(x).parameters[1]}(x.value*2) 
  Base.convert(::Type{Diameter}, x::Radius) = Diameter(typeof(x).parameters[1](x.value*2))
  Base.convert(::Type{Radius}, x::Diameter) = Radius{typeof(x).parameters[1]}(x.value/2) 
  @testitem "AbsDimension: DiameterRadius conversion" begin
    d = Diameter(Meter(3.4))
    @test convert(Radius, d) ≈ Meter(1.7)
    # @test Diameter(Meter(3.4)) ≈ Radius(Meter(1.7))

    r = Radius(Meter(1.7))
    @test convert(Diameter, r) ≈ Meter(3.4)
    
    @test convert(Diameter{Inch}, Diameter(MilliMeter(25.4))).value ≈ 1.0
    # @test convert(Inch, Diameter(MilliMeter(25.4))).value ≈ 1.0

    # down-convert Diameter to Length?
  # rod = convert(Float64,s.outside.value/2) # make a strip()?
 
    # @testset "things I want to do" begin
    #   @test isapprox( Inch(Foot(1.2)), Inch(14.4), atol=1e-3) #conversion by constructor
    # end
  end


  

  
  export AbstractLength, Length, Height, Width, Depth
  abstract type AbstractLength <: AbstractDimension end
  @makeDimension Length AbstractLength
  @makeDimension Height AbstractLength
  @makeDimension Width AbstractLength
  @makeDimension Depth AbstractLength

end
