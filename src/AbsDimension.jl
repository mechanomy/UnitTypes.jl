module AbsDimension
  using TestItems 

  using ..AbsMeasure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths
  using ..AbsExtent
  
  ## CONCEPTS - These terms describe the type of measurement. A CONCEPT necessarily includes a MEASURE, as an undimensional Diameter makes no sense.
  export AbstractDimension, @makeDimension
  # abstract type AbstractDimension <: AbstractMeasure end 
  abstract type AbstractDimension end 

  # `@makeDimension Diameter AbstracDiameter` will create:
  # struct Diameter{T <:AbstractExtent} <: AbstractDiameter
  #   value::T
  # end
  # @makeDimension Diameter ⌀ AbstractDiameter
  # show() => ⌀3.4mm
  macro makeDimension(name, abstractType) # a dimension is inherently an AbstractExtent, particularized to a certain dimensional measurement
    esc(
      # if type is abstract ... else 
      quote
        struct $name{T <: AbstractExtent } <: $abstractType
          value::T
        end
      end
    )
  end

  @testitem "makeDimension" begin
    @makeDimension TestDim1 AbstractDimension
    tdm1 = TestDim1{Meter}(3.4)
    @test typeof(tdm1) <: AbstractDimension
    @test typeof(tdm1) <: TestDim1
    @test !(typeof(tdm1) <: AbstractMeasure)
    @test isa(tdm1.value, Meter)
  end

  Base.convert(::Type{T}, x::U) where {T<:AbstractDimension, U<:AbstractDimension} = T( x.value) # there's no reason for me to do the Measure conversion here when T's constructor will
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractDimension} = isapprox(x.value, convert(T,y).value, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractExtent} = isapprox(x.value, convert(T.parameters[1],y).value, atol=atol, rtol=rtol)

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) #convert to base?
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Number, U<:AbstractDimension} = isapprox(x, y.value, atol=atol, rtol=rtol) 

  @testitem "Dimension isapprox()" begin
    @makeMeasure TestExtent1 "te1" 1.0 Meter
    @makeMeasure TestExtent2 "te2" 2.0 Meter
    @makeDimension TestDim1 AbstractDimension
    @makeDimension TestDim2 AbstractDimension

    d11 = TestDim1{TestExtent1}(1.2)
    d22 = TestDim2{TestExtent2}(0.6)
    @test d11 ≈ d22
    @test d22 ≈ d11
    @test convert(TestDim1, d22) ≈ d11
    @test !(d11 ≈ TestDim2{TestExtent2}(1.2))
    @test convert(TestDim2, d11) ≈ d22

    @test d11 ≈ 1.2
    @test d22 ≈ 0.6
  end

  Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value+y )
  Base.:+(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x+y.value )
  Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value-y )
  Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y-x.value )
  Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value*y )
  Base.:*(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x*y.value )
  Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value/y )
  Base.:/(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y/x.value )

  Base.:+(x::T, y::T) where T<:AbstractDimension = T( x.value + y.value)
  Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x + convert(T, y)
  Base.:-(x::T, y::T) where T<:AbstractDimension = T( x.value - y.value)
  Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x - convert(T, y)

  Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractExtent} = x + T(y) 
  Base.:+(y::U, x::T) where {T<:AbstractDimension, U<:AbstractExtent} = x + T(y) 
  Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractExtent} = x - T(y) 
  Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:AbstractExtent} = T(y) - x

  # prevent these operations:
  Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to multiply Dimensions"))
  Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to divide Dimensions"))

  @testitem "AbsDimension overloads" begin
    @makeMeasure TestExtent1 "te1" 1.0 Meter
    @makeMeasure TestExtent2 "te2" 2.0 Meter
    @makeDimension TestDim1 AbstractDimension
    @makeDimension TestDim2 AbstractDimension

    d11 = TestDim1{TestExtent1}(1.2)
    @testset "Dimension +-*/ Number" begin
      @test d11 + 2 ≈ 3.2
      @test 2 + d11 ≈ 3.2
      @test d11 + 2 ≈ TestExtent1(3.2)
      @test d11 - 2 ≈ -0.8
      @test 2 - d11 ≈  0.8
      @test d11 * 2 ≈ 2.4
      @test 2 * d11 ≈ 2.4
      @test d11 / 2 ≈ 0.6
      @test 2 / d11 ≈ 5/3
    end

    d22 = TestDim2{TestExtent2}(0.6)
    @testset "Dimension +-*/ Dimension" begin
      @test d11 + d22 ≈ TestExtent1(2.4)
      @test d11 + d22 ≈ TestExtent2(1.2)
      @test d11 - d22 ≈ 0
      @test d22 - d11 ≈ 0
      @test_throws ArgumentError d11 * d22 
      @test_throws ArgumentError d11 / d22 
    end

    @testset "Dimension +-*/ Measure" begin
      m1 = Meter(1)
      @test d11 + m1 ≈ 2.2
      @test m1 + d11 ≈ 2.2
      @test isapprox(d11 - m1,  0.2, atol=1e-6)
      @test isapprox(m1 - d11, -0.2, atol=1e-6)
      @test_throws MethodError d11 / m1
      @test_throws MethodError m1 / d11
      @test_throws MethodError d11 + Radian(2) # does not <:AbstractExtent
    end
  end

  Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = convert(T, x.value.value) #just drill through the Dimension and the Measure
  Base.convert(::Type{T}, x::U) where {T<:AbstractExtent, U<:AbstractDimension} = T( x.value )
 
  @testitem "AbsDimension convert()s" begin
    @makeMeasure TestExtent1 "te1" 1.0 Meter
    @makeMeasure TestExtent2 "te2" 2.0 Meter
    @makeDimension TestDim1 AbstractDimension
    @makeDimension TestDim2 AbstractDimension
    d11 = TestDim1{TestExtent1}(1.2)
    d22 = TestDim2{TestExtent2}(0.6)

    @test convert(Float64, d11) ≈ 1.2
    @test convert(TestExtent2, d11) ≈ TestExtent2(0.6)
    @test_throws MethodError convert(TestDim1, TestExtent1(1.2)) # don't convert, must use constructor


    @testset "Measure units within a Dimension" begin
      d12 = convert(TestDim1{TestExtent2}, TestDim1(TestExtent1(1.2))) 
      @test isa(d12.value, TestExtent2)
    end
  end


  
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




  # I'd like a 'permute operation' to do the drudgery of below..?
  # macro permuteOperation(operation, ::AbsA, ::AbsB)
  #   esc(
  #     quote
  #       $operation(a::AA, b::BB) where {AA<:AbsA, BB<:AbsB} = operation( AA.value?, BB.value?) # is there a way around this?
  #     end
  #   )
  # end
  # @macroexpand @permuteOperation( Base.:+, AbstractDimension, Number)


  # is there a clean lower() function, stripping the concept from the extent?

end
