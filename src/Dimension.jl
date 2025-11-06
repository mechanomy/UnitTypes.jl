# Dimensions - These terms describe the type of measurement. A Dimension necessarily includes a Measure, as an undimensional Diameter makes no sense but at the same time can be expressed through multiple Measures
export AbstractDimension, @makeDimension, @relateDimensions
abstract type AbstractDimension end 

"""
  `macro makeDimension(dimName, measure)`

  Make a new dimension `dimName` of `measure`; also creates 'Abstract`dimName`'

  ```
    @makeDimension Diameter Meter 

    d = Diameter(MilliMeter(3.4))
    r = Radius(d)
  ```
"""
macro makeDimension(dimName, measure) # a dimension is a measurement applied to a certain context
  abstractName = Symbol("Abstract"*String(dimName)) #AbstractDiameter

  esc(
    quote
      #create an abstractType of this dimension
      abstract type $abstractName <: AbstractDimension end
      export $abstractName #AbstractDiameter

      abstractMeas = supertype($measure) # AbstractLength = supertype(Meter)
      struct $dimName{T <: abstractMeas } <: $abstractName # Diamter{T<:AbstractLength} <: AbstractDiameter
        measure::T #eg Meter
      end
      $dimName(x::Number) = $dimName($measure(x)) # converting constructor Diameter(3.4) = Diameter(Meter(3.4))
      $dimName{$measure}(x::Number) = $dimName($measure(x)) # 
      export $dimName
      
      # Base.convert(::Type{T}, x::U) where {T<:abstractMeas, U<:AbstractDimension} = T(x.measure) # this should be convert(Meter, Diameter.measure), but just use .measure instead
      Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(x.measure, y.measure, atol=atol, rtol=rtol)
      Base.:<(x::T, y::U) where {T<:$abstractName, U<:$abstractName} = x.measure < convert(T,y).measure # can compare the same Dimension via the Measure; other <> ops are defined from this

      Base.zero(x::T) where T<:$abstractName = T(0) # required for LinRange()

      # Base._colon(start::T, step::U, stop::V) where {T<:$abstractName, U<:$abstractName, V<:$abstractName} = T.(start.measure:step.measure:stop.measure) # start may have a convert issue...
      # Base.broadcastable(x::T) where T<:$abstractName = Ref(x) # If a type is intended to act like a "0-dimensional scalar" (a single object) rather than as a container for broadcasting, then the following method should be defined:

      #these all return a Measure, discarding the Dimension because...we can't dispatch on return type...
      Base.:+(x::$abstractName, y::$abstractName) = x.measure + y.measure
      Base.:+(x::$abstractName, y::supertype($measure)) = x.measure + y #return Measure, leave Dimension behind
      Base.:+(x::supertype($measure), y::$abstractName) = x + y.measure
      Base.:-(x::$abstractName, y::$abstractName) = x.measure - y.measure
      Base.:-(x::$abstractName, y::supertype($measure)) = x.measure - y
      Base.:-(x::supertype($measure), y::$abstractName) = x - y.measure
      Base.:/(x::$abstractName, y::Real) = x.measure/y 
      Base.:*(x::$abstractName, y::Real) = x.measure*y 
      Base.:*(x::Real, y::$abstractName) = x*y.measure
    end
  )
end
@testitem "makeDimension" begin
  @makeMeasure Meter = MetT "met" 1
  @makeDimension DimT MetT 

  @test isa(DimT(MetT(3.4)), DimT) # default constructor 
  @test isa(DimT(3.4), DimT) # $dimName(x::Number) = $dimName($measure(x)) 
  @test isa(DimT{MetT}(3.4), DimT)

  tdm1 = DimT(3.4)
  @test typeof(tdm1) <: AbstractDimension
  @test !(typeof(tdm1) <: AbstractMeasure)
  @test typeof(tdm1.measure) <: AbstractMeasure

  # now make sure that I can't convert between Dimensions
  @makeMeasure Meter = MetT2 "met2" 2
  @makeDimension TestDim2 MetT2 

  @test isa(convert(MetT,MetT2(3.4)), MetT)
  @test isa(DimT(MetT2(3.4)), DimT)  # if it can convert from MetT2 to MetT, this should work since both are based on Meter

  @makeMeasure Second = MetT3 "met3" 3 #these should fail, showing that the <:AbstractLength is working
  @test_throws MethodError DimT(MetT3(3.4))

  @test DimT(3.4) ≈ DimT(3.4) #Base.isapprox(x::T, y::U, atol::Real=0, rtol::Real=atol) where {T<:$abstractName, U<:$abstractName} = isapprox(x.measure, y.measure, atol=atol, rtol=rtol)
  @test DimT(3.4) ≈ DimT(MetT(3.4))
  @test DimT(3.4) ≈ DimT(MetT2(1.7)) # having different internal Measures
  @test isapprox( 3.400, 3.405, atol=0.1)
  @test isapprox( DimT(3.400), DimT(3.405), atol=0.1 )

  @test_throws MethodError TestDim2(1.7) ≈ DimT(3.4) # there is no conversion between testDim2 and DimT
  @test_throws MethodError convert(MetT, DimT(3.4)) # just use .measure instead of convert
  @test_throws MethodError MetT(DimT(3.4)) # just use .measure instead of convert

  @test DimT(1.7) < DimT(3.4)
  @test DimT(3.5) > DimT(3.4)
  @test DimT(convert(MetT,MetT2(1.7))) > DimT(MetT(3.0))
  @test DimT(MetT2(1.7)) > DimT(convert(MetT2,MetT(3.0)))

  #disable for now
  # # td12 > td11 isn't working:
  # @show td11 = DimT(MetT(3.0)) # 3.0tm1
  # @show td12 = DimT(MetT2(1.7)) # 1.7tm2
  # # @show td12 > td11 #error, cannot convert DimT{MetT2} to DimT{MetT}
  # @show td12.measure > td11.measure # true, 3.4 > 3.0
  # @show td12.measure > convert(DimT, td11).measure # this should be l27 above...

  # @test isa([1,2,3] .* DimT(MetT(4)), Vector{DimT(MetT)})

  # for m in DimT.([1,2,3])
  #   @test m≈DimT(MetT(1)) || m≈DimT(MetT(2)) || m≈DimT(MetT(3))
  # end
  # b = DimT(MetT(1)) : DimT(MetT(0.3)) : DimT(MetT(2))
  # @test b[1] ≈ DimT(MetT(1))
  # @test b[2] ≈ DimT(MetT(1.3))
  # @test last(b) ≈ DimT(MetT(1.9))

  # @show c = LinRange(DimT(MetT(10)), DimT(MetT(20)), 4)
  # @test c[1] ≈ DimT(MetT(10))
  # @test c[2] ≈ DimT(MetT(13+1/3))
  # @test last(c) ≈ DimT(MetT(20))

  @test DimT(MetT(1)) + DimT(MetT2(2)) ≈ MetT(5)
  @test DimT(MetT(1)) + MetT(2) ≈ MetT(3)
  @test MetT(2) + DimT(MetT(1)) ≈ MetT(3)
  @test DimT(MetT(5)) - DimT(MetT2(2)) ≈ MetT(1)
  @test DimT(MetT(1)) - MetT(2) ≈ MetT(-1)
  @test MetT(2) - DimT(MetT(1)) ≈ MetT(1)
  @test DimT(MetT(1)) / 2 ≈ MetT(1/2)
  @test DimT(MetT(1)) * 2 ≈ MetT(2)
  @test 2 * DimT(MetT(1)) ≈ MetT(2)
end

"""
  `macro relateDimensions(relation)`

  Defines various Base.: functions that facilitate the given (linear) relationship.
  All types must already be defined and written in the form `type1 = factor * type2`, as in:
  ```
    @relateDimensions Diameter = 2.0*Radius
  ```
"""
macro relateDimensions(relation)
  # @relatedDimensions DiameterT = 2*RadiusT becomes relation= :(DiameterT = 2RadiusT) ,  .args= Any[:DiameterT, :(2RadiusT)]
  # for now just map this literally
  if length(relation.args) == 2 && isa(relation.args[2], Expr)
    type1 = relation.args[1] # DiameterT, AbstractDiameterT
    type2 = relation.args[2].args[3] #RadiusT, AbstractRadiusT
    operator = relation.args[2].args[1] # *
    factor = relation.args[2].args[2] # 2.0
    
    return esc(
      quote

        #converting constructor Radius(Diameter(3.4))
        $type1(r::supertype($type2)) = $type1( eval(Expr(:call, $operator, r.measure, $factor)) ) # DiameterT( r::AbstractRadiusT ) = DiameterT(r.measure*2)
        $type2(d::supertype($type1)) = $type2( eval(Expr(:call, $operator, d.measure, 1/$factor)) ) #note that this 1/$factor assumes $operator==*; RadiusT(d::AbstractDiameterT) = RadiusT(d.measure/2)
        #regular convert
        Base.convert(::Type{$type2}, y::supertype($type1)) = $type2(y) # Base.convert(::Type{RadiusT}, y::AbstractDiameterT) = RadiusT(y) 
        Base.convert(::Type{$type1}, y::supertype($type2)) = $type1(y) # Base.convert(::Type{DiameterT}, y::AbstractRadiusT) = DiameterT(y)

        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:supertype($type1), U<:supertype($type2)} = isapprox(x.measure, eval(Expr(:call, $operator, y.measure, $factor)), atol=atol, rtol=rtol) # Base.isapprox(x::T, y::U, atol::Real=0, rtol::Real=atol) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = isapprox(x.measure, y.measure*2, atol=atol, rtol=rtol)
        Base.isapprox(x::T, y::U; atol::Real=0, rtol::Real=atol) where {T<:supertype($type2), U<:supertype($type1)} = isapprox(x.measure, eval(Expr(:call, $operator, y.measure, 1/$factor)), atol=atol, rtol=rtol) # Base.isapprox(x::T, y::U, atol::Real=0, rtol::Real=atol) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = isapprox(x.measure, y.measure/2, atol=atol, rtol=rtol)

        Base.:<(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = x.measure < eval(Expr(:call, $operator, y.measure, $factor))  # only need eval(Expr to interpolate the $operator and $factor, # Base.:<(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = x.measure < y.measure*2
        Base.:<(x::T, y::U) where {T<:supertype($type2), U<:supertype($type1)} = x.measure < eval(Expr(:call, $operator, y.measure, 1/$factor))  # Base.:<(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = x.measure < y.measure* 1/2

        Base.:+(x::T, y::U) where {T<:supertype($type1), U<:supertype($type1)} = T(x.measure + y.measure)
        Base.:+(x::T, y::U) where {T<:supertype($type2), U<:supertype($type2)} = T(x.measure + y.measure) #this is where an OR subtyping <:{AbsDiam, AbsRad} would be convenient, could also define supertype AbsDiameterRadius via a makeBaseDimension 
        Base.:+(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = T(x.measure + eval(Expr(:call, $operator, y.measure, $factor)) ) # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = T(x.measure + y.measure*2)
        Base.:+(x::T, y::U) where {T<:supertype($type2), U<:supertype($type1)} = T(x.measure + eval(Expr(:call, $operator, y.measure, 1/$factor)) ) # Base.:+(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = T(x.measure + y.measure/2) #maintain Radius

        Base.:-(x::T, y::U) where {T<:supertype($type1), U<:supertype($type1)} = T(x.measure - y.measure) # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractDiameterT} = T(x.measure - y.measure)
        Base.:-(x::T, y::U) where {T<:supertype($type2), U<:supertype($type2)} = T(x.measure - y.measure) # Base.:-(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractRadiusT} = T(x.measure - y.measure)
        Base.:-(x::T, y::U) where {T<:supertype($type1), U<:supertype($type2)} = T(x.measure - eval(Expr(:call, $operator, y.measure, $factor))) # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = T(x.measure - y.measure*2)
        Base.:-(x::T, y::U) where {T<:supertype($type2), U<:supertype($type1)} = T(x.measure - eval(Expr(:call, $operator, y.measure, 1/$factor))) # Base.:-(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = T(x.measure - y.measure/2)

        AbsMeasure = supertype(typeof($type1(3.4).measure)) # determine the supertype of the measure; is there a way to eval this without adding to the package namespace?
        Base.:+(x::T, y::U) where {T<:supertype($type1), U<:AbsMeasure} = T(x.measure+y) # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure+y)
        Base.:+(x::T, y::U) where {T<:AbsMeasure, U<:supertype($type1)} = U(x+y.measure) # Base.:+(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x+y.measure)
        Base.:+(x::T, y::U) where {T<:supertype($type2), U<:AbsMeasure} = T(x.measure+y) # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure+y)
        Base.:+(x::T, y::U) where {T<:AbsMeasure, U<:supertype($type2)} = U(x+y.measure) # Base.:+(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x+y.measure)

        Base.:-(x::T, y::U) where {T<:supertype($type1), U<:AbsMeasure} = T(x.measure-y) # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure-y)
        Base.:-(x::T, y::U) where {T<:AbsMeasure, U<:supertype($type1)} = U(x-y.measure) # Base.:-(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x-y.measure)
        Base.:-(x::T, y::U) where {T<:supertype($type2), U<:AbsMeasure} = T(x.measure-y) # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure-y)
        Base.:-(x::T, y::U) where {T<:AbsMeasure, U<:supertype($type2)} = U(x-y.measure) # Base.:-(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x-y.measure)
      end
    )
  else
    throw(ArgumentError("@relateDimensions is unable to parse [$relation]"))
  end

end
@testitem "relateDimensions" begin
  @makeBaseMeasure LengthTest MeterT "mt"
  @makeDimension DiameterT MeterT
  @makeDimension RadiusT MeterT
  @relateDimensions DiameterT = 2*RadiusT

  dm = DiameterT(MeterT(1.2))
  rm = RadiusT(MeterT(0.6))

  @test RadiusT(dm).measure ≈ MeterT(0.6)
  @test DiameterT(rm).measure ≈ MeterT(1.2)

  # Base.convert(::Type{RadiusT}, y::AbstractDiameterT) = RadiusT(y) # RadiusT(DiameterT)
  @test convert(RadiusT, dm) ≈ rm
  # Base.convert(::Type{DiameterT}, y::AbstractRadiusT) = DiameterT(y)
  @test convert(DiameterT, rm) ≈ dm

  @test isapprox(dm, dm)
  @test isapprox(rm, rm)
  # Base.isapprox(x::T, y::U, atol::Real=0, rtol::Real=atol) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = isapprox(x.measure, y.measure*2, atol=atol, rtol=rtol)
  @test isapprox(dm, rm)
  # Base.isapprox(x::T, y::U, atol::Real=0, rtol::Real=atol) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = isapprox(x.measure, y.measure/2, atol=atol, rtol=rtol)
  @test isapprox(rm, dm)

  # Base.:<(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = x.measure < y.measure*2
  @test DiameterT(MeterT(1.2)) < RadiusT(MeterT(0.7))
  @test !(RadiusT(MeterT(0.6)) > RadiusT(MeterT(0.7)))
  @test RadiusT(MeterT(0.5)) <= DiameterT(MeterT(1.2))
  # Base.:<(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = x.measure*2 < y.measure
  @test DiameterT(MeterT(1.2)) > RadiusT(MeterT(0.4))
  @test DiameterT(MeterT(1.2)) >= RadiusT(MeterT(0.5))

  # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractDiameterT} = T(x.measure + y.measure)
  @test dm + dm ≈ DiameterT(MeterT(2.4))

  # Base.:+(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractRadiusT} = T(x.measure + y.measure)
  @test rm + rm ≈ RadiusT(MeterT(1.2))

  # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = T(x.measure + y.measure*2) #maintain Diameter
  @test dm + rm ≈ DiameterT(MeterT(2.4))

  # Base.:+(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = T(x.measure + y.measure/2) #maintain Radius
  @test rm + dm ≈ RadiusT(MeterT(1.2))

  # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractDiameterT} = T(x.measure - y.measure)
  @test dm - dm ≈ DiameterT(MeterT(0))

  # Base.:-(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractRadiusT} = T(x.measure - y.measure)
  @test rm - rm ≈ RadiusT(MeterT(0))

  # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractRadiusT} = T(x.measure - y.measure*2) #maintain Diameter
  @test dm - rm ≈ DiameterT(MeterT(0))

  # Base.:-(x::T, y::U) where {T<:AbstractRadiusT, U<:AbstractDiameterT} = T(x.measure - y.measure/2) #maintain Radius
  @test rm - dm ≈ RadiusT(MeterT(0))

  @makeMeasure MeterT = MillimeterT "mmt" 1e-3
  m = MillimeterT(1200)

  # # Base.:+(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure+y)
  @test dm + m ≈ DiameterT(MeterT(2.4))
  # # Base.:+(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x+y.measure)
  @test m + dm ≈ DiameterT(MeterT(2.4))
  # # Base.:-(x::T, y::U) where {T<:AbstractDiameterT, U<:AbstractLengthTest} = T(x.measure-y)
  @test dm - m ≈ DiameterT(MeterT(0))
  # # Base.:-(x::T, y::U) where {T<:AbstractLengthTest, U<:AbstractDiameterT} = U(x-y.measure)
  @test m - dm ≈ DiameterT(MeterT(0))

  @test isapprox(rm + m, RadiusT(MeterT(1.8)), atol=1e-3)
  @test isapprox(m + rm, RadiusT(MeterT(1.8)), atol=1e-3)
  @test isapprox(rm - m, RadiusT(MeterT(-0.6)), atol=1e-3)
  @test isapprox(m - rm, RadiusT(MeterT(0.6)), atol=1e-3)
end

# prevent these operations:
Base.:*(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to multiply Dimensions")) # what about Length * Width * Height = Volume?...
Base.:/(x::T, y::U) where {T<:AbstractDimension, U<:AbstractDimension} = throw(ArgumentError("It is nonsensical to divide Dimensions"))
@testitem "prevent */" begin
  @makeBaseMeasure LengthTest MeterT "mt"
  @makeDimension DiameterT MeterT
  @makeDimension RadiusT MeterT
  DiameterT( r::AbstractRadiusT ) = DiameterT(r.measure*2)
  RadiusT(d::AbstractDiameterT) = RadiusT(d.measure/2)

  dm = DiameterT{MeterT}(1.2)
  rm = RadiusT{MeterT}(0.6)

  @test_throws ArgumentError dm * rm 
  @test_throws ArgumentError dm / rm 
end

"""
  `function dimension2String(c::T)::String where T<:AbstractDimension`

  Returns a string representing dimension `c` with format Module.DimensionName(value unit).
"""
function dimension2String(c::T)::String where T<:AbstractDimension
  return "$(split(string(T),"{")[1])($(c.measure))" # 
end
@testitem "dimension2String" begin
  @makeMeasure Meter = TestLength1 "tl1" 10
  @makeDimension DimT TestLength1
  d11 = DimT{TestLength1}(1.2)

  @test occursin("DimT(1.2tl1)",UnitTypes.dimension2String(d11))
  @test occursin("DimT(1.2tl1)",string(d11))
end

"""
  `function Base.show(io::IO, c::T) where T<:AbstractDimension`

  @show functionality for Dimensions via `dimension2String()`.
"""
function Base.show(io::IO, c::T) where T<:AbstractDimension
  print(io, dimension2String(c))
end
