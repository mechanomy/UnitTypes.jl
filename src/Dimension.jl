# Dimensions - These terms describe the type of measurement. A Dimension necessarily includes a Measure, as an undimensional Diameter makes no sense but at the same time can be expressed through multiple Measures
export AbstractDimension, @makeDimension
abstract type AbstractDimension end 

"""
Make a new dimension `dimName` of `measure`; also creates 'Abstract`dimName`'
"""
macro makeDimension(dimName, measure) # a dimension is a measurement applied to a certain context
  abstractName = Symbol("Abstract"*String(dimName)) #AbstractDiameter
  esc(
    quote
      #create an abstractType of this dimension
      abstract type $abstractName <: AbstractDimension end
      export $abstractName #AbstractDiameter

      #and the dimension itself
      abstractMeas = supertype($measure) # AbstractLength = supertype(Meter)
      struct $dimName{T <: abstractMeas } <: $abstractName # Diamter{T<:AbstractLength} <: AbstractDiameter
        measure::T #Meter
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
  @test isa(tdm1.measure, TestMeas)
  @test tdm1 ≈ TestMeas(3.4)
end

Base.convert(::Type{T}, x::U) where {T<:AbstractDimension, U<:AbstractDimension} = T( x.measure) # there's no reason for me to do the Measure conversion here when T's constructor will
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractDimension} = isapprox(x.measure, convert(T,y).measure, atol=atol, rtol=rtol)
Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:AbstractMeasure} = isapprox(x.measure, convert(T.parameters[1],y).value, atol=atol, rtol=rtol)

# these are too dangerous, screwing up isapprox and other things
# Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:AbstractDimension, U<:Number} = isapprox(x.measure, y, atol=atol, rtol=rtol) #convert to base?
# Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:Number, U<:AbstractDimension} = isapprox(x, y.measure, atol=atol, rtol=rtol) 

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
end

Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.measure+y )
Base.:+(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x+y.measure )
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.measure-y )
Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y-x.measure )
Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.measure*y )
Base.:*(x::T, y::U) where {T<:Number, U<:AbstractDimension} = U( x*y.measure )
Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:Number} = T( x.measure/y )
# Base.:/(y::U, x::T) where {T<:AbstractDimension, U<:Number} = T( y/x.measure ) #nonsense

Base.:+(x::T, y::T) where T<:AbstractDimension = T( x.measure + y.measure)
Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x + convert(T, y)
Base.:-(x::T, y::T) where T<:AbstractDimension = T( x.measure - y.measure)
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = x - convert(T, y)

Base.:+(x::T, y::U) where {T<:AbstractDimension, U<:AbstractMeasure} = x + T(y) 
Base.:+(y::U, x::T) where {T<:AbstractDimension, U<:AbstractMeasure} = x + T(y) 
Base.:-(x::T, y::U) where {T<:AbstractDimension, U<:AbstractMeasure} = x - T(y) 
Base.:-(y::U, x::T) where {T<:AbstractDimension, U<:AbstractMeasure} = T(y) - x

# prevent these operations:
Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to multiply Dimensions"))
Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to divide Dimensions"))

@testitem "Dimension overloads" begin
  @makeDerivedMeasure TestLength1 "te1" 1.0 Meter
  @makeDerivedMeasure TestLength2 "te2" 2.0 Meter
  @makeDimension TestDim1 TestLength1
  @makeDimension TestDim2 TestLength2

  d11 = TestDim1{TestLength1}(1.2)
  d22 = TestDim2{TestLength2}(0.6)
  @testset "Dimension +-*/ Dimension" begin
    @test d11 + d22 ≈ TestLength1(2.4)
    @test d11 + d22 ≈ TestLength2(1.2)
    @test d11 - d22 ≈ TestLength1(0)
    @test d22 - d11 ≈ TestLength1(0)
    @test_throws ArgumentError d11 * d22 
    @test_throws ArgumentError d11 / d22 
  end

  @testset "Dimension +-*/ Measure" begin
    m1 = Meter(1)
    @test d11 + m1 ≈ TestDim1(TestLength1(2.2))
    @test m1 + d11 ≈ TestDim1(TestLength1(2.2))
    @test isapprox(d11 - m1,  TestDim1(TestLength1(0.2)), atol=1e-6)
    @test isapprox(m1 - d11, TestDim1(TestLength1(-0.2)), atol=1e-6)
    @test_throws MethodError d11 / m1
    @test_throws MethodError m1 / d11
    @test_throws MethodError d11 + Radian(2) # does not <:AbstractMeasure
  end
end

"""
Reduce the Dimension to its Measure
"""
Base.convert(::Type{T}, x::U) where {T<:AbstractMeasure, U<:AbstractDimension} = T( x.measure ) # reduce to Measure
@testitem "Dimension convert()s" begin
  @makeDerivedMeasure TestLength1 "te1" 1.0 Meter
  @makeDerivedMeasure TestLength2 "te2" 2.0 Meter
  @makeDimension TestDim1 AbstractDimension
  @makeDimension TestDim2 AbstractDimension
  d11 = TestDim1{TestLength1}(1.2)
  d22 = TestDim2{TestLength2}(0.6)

  @test_throws MethodError convert(Float64, d11) ≈ 1.2 
  @test convert(TestLength2, d11) ≈ TestLength2(0.6)
  @test_throws MethodError convert(TestDim1, TestLength1(1.2)) # don't convert, must use constructor

  @testset "Measure units within a Dimension" begin
    d12 = convert(TestDim1{TestLength2}, TestDim1(TestLength1(1.2))) 
    @test isa(d12.measure, TestLength2)
    @test d12.measure.value ≈ 0.6
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
