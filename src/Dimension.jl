# Dimensions - These terms describe the type of measurement. A Dimension necessarily includes a Measure, as an undimensional Diameter makes no sense but at the same time can be expressed through multiple Measures
export AbstractDimension, @makeDimension
abstract type AbstractDimension end 

"""
  Make a new dimension `dimName` of `measure`; also creates 'Abstract`dimName`'
"""
macro makeDimension(dimName, measure) # a dimension is inherently an AbstractLength, particularized to a certain dimensional measurement
  abstractName = Symbol("Abstract"*String(dimName)) #AbstractDiameter
  esc(
    quote
      #create an abstractType of this dimension
      abstract type $abstractName <: AbstractDimension end
      export $abstractName #AbstractDiameter

      #and the dimension itself
      struct $dimName{T <: AbstractLength } <: $abstractName
        value::T
      end
      export $dimName
    end
  )
end
@testitem "makeDimension" begin
  @makeDerivedMeasure TestMeas "tm" 1.0 Meter
  @makeDimension TestDim1 TestMeas 
  tdm1 = TestDim1{TestMeas}(3.4)
  @test isa(tdm1, TestDim1)
  @test isa(TestDim1(TestMeas(4.5)), TestDim1) #nested constructor

  @test typeof(tdm1) <: AbstractDimension
  @test !(typeof(tdm1) <: AbstractMeasure)
  @test isa(tdm1.value, TestMeas)
  @test tdm1 ≈ TestMeas(3.4)
end

Base.convert(::Type{T}, x::U) where {T<:AbstractDimension, U<:AbstractDimension} = T( x.value) # there's no reason for me to do the Measure conversion here when T's constructor will
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractDimension} = isapprox(x.value, convert(T,y).value, atol=atol, rtol=rtol)
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractLength} = isapprox(x.value, convert(T.parameters[1],y).value, atol=atol, rtol=rtol)

Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:Number} = isapprox(x.value, y, atol=atol, rtol=rtol) #convert to base?
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Number, U<:AbstractDimension} = isapprox(x, y.value, atol=atol, rtol=rtol) 

@testitem "Dimension isapprox()" begin
  @makeDerivedMeasure TestLength1 "te1" 1.0 Meter
  @makeDerivedMeasure TestLength2 "te2" 2.0 Meter
  @makeDimension TestDim1 TestLength1
  @makeDimension TestDim2 TestLength2

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

Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value+y )
Base.:+(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x+y.value )
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value-y )
Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y-x.value )
Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value*y )
Base.:*(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x*y.value )
Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.value/y )
# Base.:/(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y/x.value ) #nonsense

Base.:+(x::T, y::T) where T<:AbstractDimension = T( x.value + y.value)
Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x + convert(T, y)
Base.:-(x::T, y::T) where T<:AbstractDimension = T( x.value - y.value)
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x - convert(T, y)

Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractLength} = x + T(y) 
Base.:+(y::U, x::T) where {T<:AbstractDimension, U<:AbstractLength} = x + T(y) 
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractLength} = x - T(y) 
Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:AbstractLength} = T(y) - x

# prevent these operations:
Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to multiply Dimensions"))
Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to divide Dimensions"))

@testitem "Dimension overloads" begin
  @makeDerivedMeasure TestLength1 "te1" 1.0 Meter
  @makeDerivedMeasure TestLength2 "te2" 2.0 Meter
  @makeDimension TestDim1 TestLength1
  @makeDimension TestDim2 TestLength2

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
    # @test 2 / d11 ≈ 5/3 #nonsense
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
    @test_throws MethodError d11 + Radian(2) # does not <:AbstractLength
  end
end

# Base.convert(::Type{T}, x::U) where {T<:Number, U<:AbstractDimension} = convert(T, x.value.value) #not necessary? reduce to Number just drill through the Dimension and the Measure
Base.convert(::Type{T}, x::U) where {T<:AbstractLength, U<:AbstractDimension} = T( x.value ) # reduce to Measure

@testitem "Dimension convert()s" begin
  @makeDerivedMeasure TestLength1 "te1" 1.0 Meter
  @makeDerivedMeasure TestLength2 "te2" 2.0 Meter
  @makeDimension TestDim1 AbstractDimension
  @makeDimension TestDim2 AbstractDimension
  d11 = TestDim1{TestLength1}(1.2)
  d22 = TestDim2{TestLength2}(0.6)

  # @test convert(Float64, d11) ≈ 1.2 # I don't think this is necessary
  @test_throws MethodError convert(Float64, d11) ≈ 1.2 
  @test convert(TestLength2, d11) ≈ TestLength2(0.6)
  @test_throws MethodError convert(TestDim1, TestLength1(1.2)) # don't convert, must use constructor

  @testset "Measure units within a Dimension" begin
    d12 = convert(TestDim1{TestLength2}, TestDim1(TestLength1(1.2))) 
    @test isa(d12.value, TestLength2)
    @test d12.value.value ≈ 0.6
  end
end

"""
  create a string from Dimension `c` with format Module.DimensionName(value unit)
"""
function dimension2String(c::T)::String where T<:AbstractDimension
  return "$(split(string(T),"{")[1])($(convert(T.parameters[1],c)))" #ugly parsing of typestring...but not seeing an alternative
end
@testitem "dimension2String" begin
  @makeDerivedMeasure TestLength1 "tl1" 1.0 Meter
  @makeDimension TestDim1 TestLength1
  d11 = TestDim1{TestLength1}(1.2)

  @test occursin("TestDim1(1.2tl1)",UnitTypes.dimension2String(d11))
  @test occursin("TestDim1(1.2tl1)",string(d11))
end

"""
  @show functionality via `dimension2String()`.
"""
function Base.show(io::IO, c::T) where T<:AbstractDimension
  print(io, dimension2String(c))
end
