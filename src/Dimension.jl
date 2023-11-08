module Dimension
  using TestItems 

  using ..Measure #the .. indicates a local, sibling package, https://docs.julialang.org/en/v1/manual/modules/#Submodules-and-relative-paths
  using ..Length
  
  ## CONCEPTS - These terms describe the type of measurement. A CONCEPT necessarily includes a MEASURE, as an undimensional Diameter makes no sense.
  export tractDimension, @makeDimension
  abstract type tractDimension end 

  # `@makeDimension Diameter tracDiameter` will create:
  # struct Diameter{T <:tractLength} <: tractDiameter
  #   value::T
  # end
  macro makeDimension(name, abstractName) # a dimension is inherently an tractLength, particularized to a certain dimensional measurement
    esc(
      quote
        struct $name{T <: tractLength } <: $abstractName
          value::T
        end
      end
    )
  end

  # If I wanted to make subtype Dimensions, perhaps:
  # @makeDimension Diameter 1.0 tractDiameter ⌀
  # @makeDimension Radius 0.5 Diameter
  # show() => ⌀3.4mm

  @testitem "makeDimension" begin
    @makeMeasure TestMeas "tm" 1.0 tractLength
    @makeDimension TestDim1 tractDimension
    tdm1 = TestDim1{TestMeas}(3.4)
    @test typeof(tdm1) <: tractDimension
    @test typeof(tdm1) <: TestDim1
    @test !(typeof(tdm1) <: tractMeasure)
    @test isa(tdm1.value, TestMeas)
  end

  Base.convert(::Type{T}, x::U) where {T<:tractDimension, U<:tractDimension} = T( x.value) # there's no reason for me to do the Measure conversion here when T's constructor will
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractDimension, U<:tractDimension} = isapprox(x.value, convert(T,y).value, atol=atol, rtol=rtol)
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractDimension, U<:tractLength} = isapprox(x.value, convert(T.parameters[1],y).value, atol=atol, rtol=rtol)

  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:tractDimension, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) #convert to base?
  Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Number, U<:tractDimension} = isapprox(x, y.value, atol=atol, rtol=rtol) 

  @testitem "Dimension isapprox()" begin
    @makeMeasure TestLength1 "te1" 1.0 Meter
    @makeMeasure TestLength2 "te2" 2.0 Meter
    @makeDimension TestDim1 tractDimension
    @makeDimension TestDim2 tractDimension

    d11 = TestDim1{TestLength1}(1.2)
    d22 = TestDim2{TestLength2}(0.6)
    @test d11 ≈ d22
    @test d22 ≈ d11
    @test convert(TestDim1, d22) ≈ d11
    @test !(d11 ≈ TestDim2{TestLength2}(1.2))
    @test convert(TestDim2, d11) ≈ d22

    @test d11 ≈ 1.2
    @test d22 ≈ 0.6
  end

  Base.:+(x::T, y::U) where {T<:tractDimension, U<:Number} = T( x.value+y )
  Base.:+(x::T, y::U) where {T<:Number, U<:tractDimension} = U( x+y.value )
  Base.:-(x::T, y::U) where {T<:tractDimension, U<:Number} = T( x.value-y )
  Base.:-(y::U, x::T) where {T<:tractDimension, U<:Number} = T( y-x.value )
  Base.:*(x::T, y::U) where {T<:tractDimension, U<:Number} = T( x.value*y )
  Base.:*(x::T, y::U) where {T<:Number, U<:tractDimension} = U( x*y.value )
  Base.:/(x::T, y::U) where {T<:tractDimension, U<:Number} = T( x.value/y )
  Base.:/(y::U, x::T) where {T<:tractDimension, U<:Number} = T( y/x.value )

  Base.:+(x::T, y::T) where T<:tractDimension = T( x.value + y.value)
  Base.:+(x::T, y::U) where {T<:tractDimension, U<:tractDimension} = x + convert(T, y)
  Base.:-(x::T, y::T) where T<:tractDimension = T( x.value - y.value)
  Base.:-(x::T, y::U) where {T<:tractDimension, U<:tractDimension} = x - convert(T, y)

  Base.:+(x::T, y::U) where {T<:tractDimension, U<:tractLength} = x + T(y) 
  Base.:+(y::U, x::T) where {T<:tractDimension, U<:tractLength} = x + T(y) 
  Base.:-(x::T, y::U) where {T<:tractDimension, U<:tractLength} = x - T(y) 
  Base.:-(y::U, x::T) where {T<:tractDimension, U<:tractLength} = T(y) - x

  # prevent these operations:
  Base.:*(x::T, y::U) where {T<:tractDimension, U<:tractDimension} = throw(ArgumentError("It is nonsensical to multiply Dimensions"))
  Base.:/(x::T, y::U) where {T<:tractDimension, U<:tractDimension} = throw(ArgumentError("It is nonsensical to divide Dimensions"))

  @testitem "Dimension overloads" begin
    @makeMeasure TestLength1 "te1" 1.0 Meter
    @makeMeasure TestLength2 "te2" 2.0 Meter
    @makeDimension TestDim1 tractDimension
    @makeDimension TestDim2 tractDimension

    d11 = TestDim1{TestLength1}(1.2)
    @testset "Dimension +-*/ Number" begin
      @test d11 + 2 ≈ 3.2
      @test 2 + d11 ≈ 3.2
      @test d11 + 2 ≈ TestLength1(3.2)
      @test d11 - 2 ≈ -0.8
      @test 2 - d11 ≈  0.8
      @test d11 * 2 ≈ 2.4
      @test 2 * d11 ≈ 2.4
      @test d11 / 2 ≈ 0.6
      @test 2 / d11 ≈ 5/3
    end

    d22 = TestDim2{TestLength2}(0.6)
    @testset "Dimension +-*/ Dimension" begin
      @test d11 + d22 ≈ TestLength1(2.4)
      @test d11 + d22 ≈ TestLength2(1.2)
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
      @test_throws MethodError d11 + Radian(2) # does not <:tractLength
    end
  end

  Base.convert(::Type{T}, x::U) where {T<:Number, U<:tractDimension} = convert(T, x.value.value) #just drill through the Dimension and the Measure
  Base.convert(::Type{T}, x::U) where {T<:tractLength, U<:tractDimension} = T( x.value )
  
  @testitem "Dimension convert()s" begin
    @makeMeasure TestLength1 "te1" 1.0 Meter
    @makeMeasure TestLength2 "te2" 2.0 Meter
    @makeDimension TestDim1 tractDimension
    @makeDimension TestDim2 tractDimension
    d11 = TestDim1{TestLength1}(1.2)
    d22 = TestDim2{TestLength2}(0.6)

    @test convert(Float64, d11) ≈ 1.2
    @test convert(TestLength2, d11) ≈ TestLength2(0.6)
    @test_throws MethodError convert(TestDim1, TestLength1(1.2)) # don't convert, must use constructor

    @testset "Measure units within a Dimension" begin
      d12 = convert(TestDim1{TestLength2}, TestDim1(TestLength1(1.2))) 
      @test isa(d12.value, TestLength2)
    end
  end
  
  """
  """
  function dimension2String(c::T)::String where T<:tractDimension
    return "$(split(string(T),"{")[1])($(convert(T.parameters[1],c)))" #ugly parsing of typestring...but not seeing an alternative
  end
  @testitem "dimension2String" begin
    @makeMeasure TestLength1 "te1" 1.0 Meter
    @makeDimension TestDim1 tractDimension
    d11 = TestDim1{TestLength1}(1.2)

    # @test UnitTypes.Dimension.dimension2String(d11) === "Main.var\"##300\".TestDim1(1.2te1)" #this #300 is execution specific from TestItemRunner
    @test occursin("TestDim1(1.2te1)",UnitTypes.Dimension.dimension2String(d11))
    @test occursin("TestDim1(1.2te1)",string(d11))
  end


  """
  """
  function Base.show(io::IO, c::T) where T<:tractDimension
    print(io, dimension2String(c))
  end


  # I'd like a 'permute operation' to do the drudgery of the isapprox and converts..?
  # macro permuteOperation(operation, ::A, ::B)
  #   esc(
  #     quote
  #       $operation(a::AA, b::BB) where {AA<:A, BB<:B} = operation( AA.value?, BB.value?) # is there a way around this?
  #     end
  #   )
  # end
  # @macroexpand @permuteOperation( Base.:+, tractDimension, Number)
end
