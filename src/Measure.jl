export AbstractMeasure, @makeBaseMeasure, @makeMeasure, @relateMeasures, toBaseFloat, abbreviation, @u_str, displayUnitTypes
abstract type AbstractMeasure end

struct UnitTypeAttributes
  abstract::DataType # the abstract type of this type, say AbstractLength for MilliMeter
  base::DataType # Meter
  toBase::Function # function to numerically convert to the base unit; this would be overkill except for handling affine units
  fromBase::Function # function to numerically convert from the base unit
  abbreviation::String # "mm"
  isAffine::Bool 
end
allUnitTypes = Dict{DataType, UnitTypeAttributes}()

"""
  `abbreviation(m::AbstractMeasure)::String`

  Returns the unit string for `m`.
"""
function abbreviation(m::AbstractMeasure)::String
  return UnitTypes.allUnitTypes[typeof(m)].abbreviation
end
@testitem "abbreviation()" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @test UnitTypes.abbreviation(MeterT(3.4)) == "mT"
end

function displayUnitTypes()
  println("allUnitTypes=")
  display(sort(collect(allUnitTypes), by=x->string(x[1]))) # alphabetize and display
end

"""
  `macro makeBaseMeasure(quantityName, unitName, abbreviation::String, isAffine=false)`

  Make a new base measure which has no relationship to an existing unit.
  For example, in `@makeBaseMeasure Length Meter "m"`:
  * `quantityName` is the name of the measure, 'Length' above.
  * `unitName` is the name of the unit which will be used to make measures bearing that unit, 'Meter' above.
  * `abbreviation` is the abbreviation of the unit name, used in all string presentations of the measure.
  The macro will introduce `AbstractLength <: AbstractMeasure` and `Meter()` into the current scope.
  
  To get the measure's value in the base unit as a float, see toBaseFloat().
"""
macro makeBaseMeasure(quantityName, unitName, abbreviation::String)
  # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName abbreviation:$abbreviation")
  abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength

  if unitName in keys(UnitTypes.allUnitTypes)
    @warn "$unitName is already defined, skipping"
    return
  end

  qts = [quote
      abstract type $abstractName <: AbstractMeasure end
      export $abstractName #AbstractLength

      """
        This UnitType represents a basic measure of $($unitName) with units $($abbreviation).
      """
      struct $unitName <: $abstractName
        value::Float64 # the value on creation as measured in the unit
      end
      $unitName(x::T where T<:$abstractName) = $unitName( UnitTypes.allUnitTypes[typeof(x)].toBase(x.value) ) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
      export $unitName

      UnitTypes.allUnitTypes[$unitName] = UnitTypes.UnitTypeAttributes($abstractName, $unitName, x->x, x->x, $abbreviation, false) # A base unit cannot be converted into another base unit, so its to/fromBase functions are =

      UnitTypes.makeSelfConversion($unitName) 
      # UnitTypes.makeJointConversions($unitName) # define operations on itself, as there cannot be any children measures yet
    end
  ]
  return esc( Expr(:block, qts...))
end
@testitem "@makeBaseMeasure" begin
  @makeBaseMeasure MeterTest MeterT "metT" 
  # @makeMeasure MeterT(1) = MilliMeterT(1000) "miliT" # u_str needs symbol uniqueness! when this was "mmT", it seemed to conflict with the @makeMeasure test definition of "mmT" and this conflict showed up as LoadError: UndefVarError: `MiliT` not defined in `Main.var"##233"`

  @testset "did the macro create the definitions we expect" begin
    @test isdefined(@__MODULE__, :AbstractMeterTest)
    @test isdefined(@__MODULE__, :MeterT)

    @makeBaseMeasure DensityTest DensityT "dennyT"
    @test_throws MethodError MeterT(1.2)*DensityT(3.4) #method error because these cannot be * yet
  end
  
  @testset "constructor" begin
    @test MeterT(3.4).value == 3.4
    # @show MeterT(2im).value # should imaginary error?
    @test MeterT(MeterT(3.4)).value == 3.4
  end

  @testset "allUnitTypes is populated" begin
    @test haskey(UnitTypes.allUnitTypes, MeterT)
    @test UnitTypes.allUnitTypes[MeterT].abstract == AbstractMeterTest
    @test UnitTypes.allUnitTypes[MeterT].base == MeterT
    @test UnitTypes.allUnitTypes[MeterT].toBase(10) ≈ 10
    @test UnitTypes.allUnitTypes[MeterT].fromBase(10) ≈ 10
    @test UnitTypes.allUnitTypes[MeterT].abbreviation == "metT"
  end
end

"""
  `macro makeMeasure(relation, newAbbreviation, toBase, fromBase=missing)

  Creates a new Measure from an existing base measure.
  The left hand side of the equation must already exist, while the right hand side should be undefined, with the string providing the unit symbol and the number the multiplicative factor to the base unit.
  ```
  @makeMeasure Meter = Inch "in" 0.0254
  ```
  Here, an Inch * 0.0254 = Meter.

  Affine units (like Temperature) can provide anonymous functions converting to and from the base unit.
  ```
  @makeMeasure Kelvin = Fahrenheit "F" f->(f+459.67)*5/9 k->k*9/5-459.67 
  ```
"""
macro makeMeasure(relation, newAbbreviation, newToBase, newFromBase=missing, isAffine=false)
  # @makeMeasure Kelvin = Fahrenheit "F" f->(f+459.67)*5/9 k->k*9/5-459.67 
  # display(dump(relation)) # Kelvin = Fahrenheit
  existingType = relation.args[1] # Kelvin
  newType = relation.args[2] # Fahrenheit
  
  # accept base factor
  toBase = newToBase
  fromBase = newFromBase
  if ismissing(newFromBase) 
    global toBase = :(x->x*$newToBase) # quote to match format
    global fromBase = :(x->x/$newToBase)
  end
  isAffine |= contains(string(toBase), "+") || contains(string(fromBase), "+") # check if the expr has +-, making it an affine conversion, which means that we need to restrict what +- functions are added; - is also used for x->x, so just check for + in to&from


  qts = [quote
    existingSupertype = supertype($existingType)

    """
      This UnitType represents a basic measure of $($newType) with units $($newAbbreviation).
    """
    struct $newType <: existingSupertype
      value::Float64 # the value on creation as measured in the unit
    end
    $newType(x::T where T<:existingSupertype) = $newType( $fromBase(UnitTypes.allUnitTypes[typeof(x)].toBase(x.value) )) # conversion constructor
    export $newType

    UnitTypes.allUnitTypes[$newType] = UnitTypes.UnitTypeAttributes(existingSupertype, $existingType, $toBase, $fromBase, $newAbbreviation, $isAffine) 

    UnitTypes.makeSelfConversion($newType)
    UnitTypes.makeJointConversions($newType)
  end]
  return esc( Expr(:block, qts...))
end
@testitem "makeMeasure" begin
  @makeBaseMeasure LengthTest MeterT "mT"
  @makeBaseMeasure TemperatureTest KelvinT "KT"

	@makeMeasure KelvinT = FahrenheitT "FT" f->(f+459.67)*5/9 k->k*9/5-459.67 
	@makeMeasure MeterT = CentiMeterT "cmT" x->x/100 x->x*100
  # @makeMeasure MeterT = InchT "inT" x->x*0.0254 x->x/0.0254
  @makeMeasure MeterT = InchT "inT" 25.4/1000 # 0.0254
  @makeMeasure MeterT = KiloMeterT "KmT" 1/1000

  @makeMeasure InchT = FootT "ft" 12

  @testset "subtyped" begin
    @test KelvinT <: AbstractMeasure
    @test KelvinT <: AbstractTemperatureTest
    @test FahrenheitT <: AbstractTemperatureTest
  end

  @testset "to/fromBase" begin # check that the functions work
    @test UnitTypes.allUnitTypes[FahrenheitT].toBase(32) ≈ 273.15
    @test UnitTypes.allUnitTypes[FahrenheitT].fromBase(273.15) ≈ 32
    @test UnitTypes.allUnitTypes[CentiMeterT].toBase(123) ≈ 1.23
    @test UnitTypes.allUnitTypes[CentiMeterT].fromBase(1.23) ≈ 123
    @test UnitTypes.allUnitTypes[InchT].toBase(1) ≈ 0.0254 
    @test UnitTypes.allUnitTypes[InchT].fromBase(1) ≈ 39.3700787
  end

  @testset "conversion constructor" begin
    @test isapprox(InchT(CentiMeterT(10)), InchT(3.937007), atol=1e-5)
  end

  # @testset "erroneous/re-definition" begin
    # re macroexpand() see https://github.com/JuliaLang/julia/issues/56733
    # @test_warn "MilliMeterT is already defined, cannot re-define" macroexpand(@__MODULE__, :( @makeMeasure2 MeterT = CentiMeterT "cmT" x->x/100 x->x*100  ))

    # @test_warn "mmT is already used by MilliMeterT, suggest choosing a unique unit" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  ))
    # @test_warn "mmT is already used by Main.var\"###236\".MilliMeterT, suggest choosing a unique unit" macroexpand(@__MODULE__, :( @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  )) # 236 can change, there should be a better reference.. disabling check now
    # @test true # @makeMeasure MeterT(1) = MMeterT(5000) "mmT"  

    # @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterTNot(1) = MilliMeterT3(5000) "mmT3"  )) # error on LHS not existing
    # @test_throws ArgumentError macroexpand(@__MODULE__, :( @makeMeasure MeterT(1)*Seconds(3) = MilliMeterTS(5000) "mmTS"  )) # error on compound relations
  # end


end

"""
  makeSelfConversions(newType) 
  
  Defines function overloads for common operations like `convert`, `isapprox`, `isequal`, `isless`, +-/* between instances of a new UnitType.
"""
function makeSelfConversion(newType)
  uta = allUnitTypes[newType]
  if uta.isAffine
    # if affine, we can only +- within the type, can't Kelvin+Celsius without messing around with zero points.
    UnitTypes.eval( quote 
      if !hasmethod(Base.:+, ($newType, $newType))
        Base.:+(x::$newType, y::$newType) = $newType(x.value + y.value) 
      end
      if !hasmethod(Base.:-, ($newType, $newType))
        Base.:-(x::$newType, y::$newType) = $newType(x.value - y.value) 
      end
    end)
  end

  UnitTypes.eval( quote 
    if !hasmethod(Base.isapprox, ($newType, $newType) )
      Base.isapprox(x::$newType, y::$newType; atol::Real=0, rtol::Real=atol) = isapprox( x.value, y.value, atol=atol, rtol=rtol) 
    end

    if !hasmethod(Base.isless, ($newType, $newType) )
      Base.isless(x::$newType, y::$newType) = x.value < y.value 
    end

    if !hasmethod(Base.broadcastable, ($newType))
      Base.broadcastable(x::$newType) = Ref(x) 
    end

    if !hasmethod(Base.zero, ($newType))
      Base.zero(x::$newType) = $newType(0) #zero() seems to be required for _colon()
    end

    if !hasmethod(Base.:+, (Number, $newType))
      Base.:+(x::Number, y::$newType) = $newType(x+y.value) 
    end
    if !hasmethod(Base.:+, ($newType, Number))
      Base.:+(x::$newType, y::Number) = $newType(x.value+y) 
    end
    if !hasmethod(Base.:-, (Number, $newType))
      Base.:-(x::Number, y::$newType) = $newType(x-y.value) 
    end
    if !hasmethod(Base.:-, ($newType, Number))
      Base.:-(x::$newType, y::Number) = $newType(x.value-y) 
    end

    if !hasmethod(Base.:+, ($newType, $newType))
      Base.:+(x::$newType, y::$newType) = $newType(x.value+y.value) 
    end
    if !hasmethod(Base.:-, ($newType, $newType))
      Base.:-(x::$newType, y::$newType) = $newType(x.value-y.value) 
    end
    if !hasmethod(Base.:-, ($newType))
      Base.:-(x::$newType) = x * -1 # leading negation
    end

    if !hasmethod(Base.:*, ($newType, Number))
      Base.:*(x::$newType, y::U) where U<:Number = $newType(x.value*y) 
      Base.:*(x::T, y::$newType) where T<:Number = $newType(x*y.value)
    end

    if !hasmethod(Base.:/, ($newType, Number))
      Base.:/(x::$newType, y::U) where U<:Number = $newType(x.value/y)
    end

    if !hasmethod(Base.rem, ($newType, $newType))
      Base.rem(x::$newType, y::$newType) = $newType(rem(x.value, y.value, RoundNearest)) # required for _colon
      # Base.rem(x::$newType, y::$newType, r::RoundingMode) = $newType(rem(x.value, y.value, r)) ..define others? https://github.com/JuliaLang/julia/blob/d665f8980f2bada7cd87fd79610ab769e44e95f7/base/div.jl#L114
    end

    # 251105 - implementing colon in v1.11+ causes all sorts of mucking around in Base, omit
    # if !hasmethod(Base._colon, ($newType, $newType, $newType))
    #   Base._colon(start::$newType, step::$newType, stop::$newType) = $newType.(start.value : step.value : stop.value)
    # end
    # julia1.11 is complaining about the internals of range...:
    # the point of the range function is for 1m:5m to have a iterand as a Meter...
    # if !hasmethod(Base.div, ($newType, $newType, RoundingMode))
    #   Base.div(x::$newType, y::$newType, r::RoundingMode) = $newType(div(x.value, y.value, r))
    # end
    # Return a multiplicative identity for x: a value such that one(x)*x == x*one(x) == x.  
    # Alternatively one(T) can take a type T, in which case one returns a multiplicative identity for any x of type T.
    # If possible, one(x) returns a value of the same type as x, and one(T) returns a value of type T. However, this may not be the case for types representing dimensionful quantities (e.g. time in days), since the multiplicative identity must   be dimensionless. In that case, one(x) should return an identity value of the same    precision (and shape, for matrices) as x.
    # Base.one(x::$newType) = $newType(1.0)
    # Base.Integer(x::$newType) = $newType(Integer(x.value))
    #Expression: b[1] ≈ MeterT(1) MethodError: no method matching unchecked_oneto(::Main.var"##239".MeterT)
    # but looking higher in the error:   [8] getindex(v::StepRange{Main.var"##239".MeterT, Main.var"##239".MeterT}, i::Int64)      @ Base .\array.jl:3077
    # Base.getindex(v::StepRange{$newType}, x::$newType, i::Int64) = 
    # Base.StepRange(start::$newType, step::S, stop::$newType) where S = Base.StepRange(start, step, $newType(Base.steprange_last(start.value, step, stop.value)))

  end)

end
@testitem "makeSelfConversion" begin
  @makeBaseMeasure LengthTest MeterT "mtT" 
  @makeBaseMeasure SoundTest GrowlT "gT" 
  @makeBaseMeasure TemperatureTest KelvinT "KT"

  @testset "convert" begin
    @test isa( convert(MeterT, MeterT(1.2)), MeterT)
    @test_throws MethodError convert(GrowlT, MeterT(1.2)) #  MethodError: Cannot `convert` an object of type Main.var"##238".MeterT to an object of type Main.var"##238".GrowlT
  end

  @testset "isapprox" begin
    @test MeterT(1.2) ≈ MeterT(1.2)
  end

  @testset "isequal" begin
    @test MeterT(3.4) == MeterT(3.4)
    @test MeterT(3.4) != MeterT(1.2)
    @test MeterT(3.4) != GrowlT(3.4) 
  end

  @testset "less than" begin
    @test MeterT(1.2) < MeterT(3.4)
    @test MeterT(1.2) <= MeterT(3.4)
    @test MeterT(3.4) > MeterT(1.2)
    @test MeterT(3.4) >= MeterT(1.2)
    @test_throws MethodError MeterT(1.2) < GrowlT(3.4)
  end

  @testset "addition" begin
    @test MeterT(1) + MeterT(10) ≈ MeterT(11)
    @test KelvinT(1) + KelvinT(2) ≈ KelvinT(3)
    @test length(methods(Base.:+, (MeterT, MeterT))) == 1
  end

  @testset "subtraction" begin
    @test MeterT(2) - MeterT(0.1) ≈ MeterT(1.9)
  end

  @testset "multiply divide" begin
    @test MeterT(3)*3 ≈ MeterT(9)
    @test 3*MeterT(3) ≈ MeterT(9)
    @test MeterT(3)/3 ≈ MeterT(1)

    @test -MeterT(3) ≈ MeterT(-3)
    @test MeterT(5) + MeterT(-3) ≈ MeterT(2)
    @test MeterT(5) + -MeterT(3) ≈ MeterT(2)
  end

  @testset "broadcasting" begin
    @test isa([1,2,3] .* MeterT(4), Vector{MeterT})
    for m in MeterT.([1,2,3])
      @test m≈MeterT(1) || m≈MeterT(2) || m≈MeterT(3)
    end
  end

  @testset "range" begin
    c = LinRange(MeterT(10), MeterT(20), 4)
    @test c[1] ≈ MeterT(10)
    @test c[2] ≈ MeterT(13+1/3)
    @test last(c) ≈ MeterT(20)
  end

  # @testset "range _colon" begin # 251105 - implementing colon in v1.11+ causes all sorts of mucking around in Base, omit
  #   b = MeterT(1) : MeterT(0.3) : MeterT(2)
  #   @test b[1] ≈ MeterT(1)
  #   @test b[2] ≈ MeterT(1.3)
  #   @test last(b) ≈ MeterT(1.9)

  #   @test_throws MethodError GrowlT(1) : MeterT(0.3) : MeterT(2) 
  #   @test_throws MethodError MeterT(1) : GrowlT(0.3) : MeterT(2) 
  #   @test_throws MethodError MeterT(1) : MeterT(0.3) : GrowlT(2) 
  # end
end

"""
  makeJointConversions(newType) 
  
  Defines function overloads for common operations like `convert`, `isapprox`, `isequal`, `isless`, +-/* between a new UnitType and all existing UnitTypes.
"""
function makeJointConversions(newType=nothing) # optional argument to only run on the newly created type, for makeMeasure()
  for a in Dict(newType=>allUnitTypes[newType]) # match structure of allUnitTypes
    for b in allUnitTypes 
      if supertype(a.first) == supertype(b.first) # can only convert/isapprox/+-*/ within the same measures
        UnitTypes.eval( quote
          if !hasmethod(Base.convert, (Type{$(a.first)}, $(b.first)) )
            Base.convert(::Type{$(a.first)}, y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(b.second.toBase)(y.value)) ) # convert b to its base, then a fromBase
          end
          if !hasmethod(Base.convert, (Type{$(b.first)}, $(a.first)) )
            Base.convert(::Type{$(b.first)}, y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(a.second.toBase)(y.value)) ) # 
          end

          if !hasmethod(Base.isapprox, ($(a.first), $(b.first)) )
            Base.isapprox(x::$(a.first), y::$(b.first); atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert($(a.first),y).value, atol=atol, rtol=rtol) # note this does not modify rtol or atol...but should it scale these in some way between the given unit and its base?
          end
          if !hasmethod(Base.isapprox, ($(b.first), $(a.first)) )
            Base.isapprox(x::$(b.first), y::$(a.first); atol::Real=0, rtol::Real=atol) = isapprox( x.value, convert($(b.first),y).value, atol=atol, rtol=rtol) 
          end

          if !hasmethod(Base.isequal, ($(a.first), $(b.first)) )
            Base.isequal(x::$(a.first), y::$(b.first)) = x.value == convert(($a.first),y).value
          end
          if !hasmethod(Base.isequal, ($(b.first), $(a.first)) )
            Base.isequal(x::$(b.first), y::$(a.first)) = x.value == convert(($b.first),y).value
          end

          if !hasmethod(Base.isless, ($(a.first), $(b.first)) )
            Base.isless(x::$(a.first), y::$(b.first)) = x.value < convert($(a.first),y).value # other <> ops are defined from this
          end
          if !hasmethod(Base.isless, ($(b.first), $(a.first)) )
            Base.isless(x::$(b.first), y::$(a.first)) = x.value < convert($(b.first),y).value # other <> ops are defined from this
          end
        end)
        # Base.:+(<:Number) not implemented to prevent random numbers from assuming UnitTypes, the point is to be explicit

        UnitTypes.eval( quote
          if !($(a.second.isAffine) || $(b.second.isAffine))
            if !hasmethod(Base.:+, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(a.second.toBase)(x.value) + $(b.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:+, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(b.second.toBase)(x.value) + $(a.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:-, ($(a.first),$(b.first))) 
              Base.:-(x::$(a.first), y::$(b.first)) = $(a.first)( $(a.second.fromBase)( $(a.second.toBase)(x.value) - $(b.second.toBase)(y.value))) 
            end
            if !hasmethod(Base.:-, ($(b.first),$(a.first))) 
              Base.:-(x::$(b.first), y::$(a.first)) = $(b.first)( $(b.second.fromBase)( $(b.second.toBase)(x.value) - $(a.second.toBase)(y.value))) 
            end
          end

          # if affine, we can only +- within the type, can't Kelvin+Celsius without messing around with zero points
          if $(a.second.isAffine) && $(b.second.isAffine) && $(a.first)==$(b.first) 
            if !hasmethod(Base.:+, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)(x.value + y.value)
            end
            if !hasmethod(Base.:+, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)(x.value + y.value)
            end
            if !hasmethod(Base.:-, ($(a.first),$(b.first))) 
              Base.:+(x::$(a.first), y::$(b.first)) = $(a.first)(x.value - y.value)
            end
            if !hasmethod(Base.:-, ($(b.first),$(a.first))) 
              Base.:+(x::$(b.first), y::$(a.first)) = $(b.first)(x.value - y.value)
            end
          end
        end)
      end
    end
  end
end
@testitem "makeJointConversions" begin
  @makeBaseMeasure LengthTest MeterT "mtT" 
  @makeBaseMeasure SoundTest GrowlT "gT" 
  @makeBaseMeasure TemperatureTest KelvinT "KT"

	@makeMeasure KelvinT = FahrenheitT "FT" x->(x+459.67)*5/9 k->k*9/5-459.67 
	@makeMeasure MeterT = CentiMeterT "cmT" 1e-2 # x->x/100 x->x*100
  @makeMeasure MeterT = InchT "inT" x->x*0.0254 x->x/0.0254

  @testset "convert" begin
    @test isa( convert(MeterT, MeterT(1.2)), MeterT)
    @test isa( convert(MeterT, CentiMeterT(1.2)), MeterT)
    @test isa( convert(CentiMeterT, MeterT(1.2)), CentiMeterT)

    @test isapprox( convert(CentiMeterT, MeterT(1.234)), CentiMeterT(123.4), atol=1e-3)

    @test convert(InchT, CentiMeterT(3.048)).value ≈ 1.2 
    @test convert(CentiMeterT, InchT(1.2)).value ≈ 3.048 

    @test_throws MethodError convert(GrowlT, MeterT(1.2)) #  MethodError: Cannot `convert` an object of type Main.var"##238".MeterT to an object of type Main.var"##238".GrowlT
  end

  @testset "isapprox" begin
    @test InchT(1.2) ≈ InchT(1.2)
    @test InchT(1.2) ≈ CentiMeterT(3.048)
    @test MeterT(0.03048) ≈ CentiMeterT(3.048)
    @test CentiMeterT(3.048) ≈ MeterT(0.03048)
    @test isapprox( FahrenheitT(32), KelvinT(273.15), atol=1e-3)
    @test isapprox( KelvinT(273.15), FahrenheitT(32), atol=1e-3)
  end

  @testset "isequal" begin
    @test MeterT(3.4) == MeterT(3.4)
    @test MeterT(3.4) != MeterT(1.2)
    @test MeterT(3.4) != GrowlT(3.4) 

    @test MeterT(1.2) ≈ CentiMeterT(120)
    @test MeterT(1).value == convert(MeterT, CentiMeterT(100)).value 
  end

  @testset "less than" begin
    @test MeterT(1.2) < MeterT(3.4)
    @test MeterT(1.2) <= MeterT(3.4)
    @test MeterT(3.4) > MeterT(1.2)
    @test MeterT(3.4) >= MeterT(1.2)

    @test CentiMeterT(1.2) < MeterT(1.2)
    @test CentiMeterT(1200) > MeterT(1.2)

    @test_throws MethodError MeterT(1.2) < GrowlT(3.4)
  end

  @testset "addition" begin
    @test MeterT(1) + CentiMeterT(10) ≈ MeterT(1.1)
    @test CentiMeterT(100) + MeterT(1) ≈ MeterT(2)

    @test KelvinT(1) + KelvinT(2) ≈ KelvinT(3)
    @test isapprox(FahrenheitT(1) + FahrenheitT(2), FahrenheitT(3), atol=1e-3)

    @test length(methods(Base.:+, (FahrenheitT, FahrenheitT))) == 1
    @test length(methods(Base.:+, (FahrenheitT, KelvinT))) == 0
    @test length(methods(Base.:+, (KelvinT, FahrenheitT))) == 0
    @test_throws MethodError KelvinT(100) + FahrenheitT(32) # no method matching +, can't add cross-bases by default
  end

  @testset "subtraction" begin
    @test MeterT(2) - CentiMeterT(100) ≈ MeterT(1)
    @test CentiMeterT(200) - MeterT(1) ≈ MeterT(1)
    @test KelvinT(3) - KelvinT(1) ≈ KelvinT(2)
  end

  @testset "multiply divide" begin
    @test MeterT(3)*3 ≈ MeterT(9)
    @test 3*MeterT(3) ≈ MeterT(9)
    @test MeterT(3)/3 ≈ MeterT(1)

    @test -MeterT(3) ≈ MeterT(-3)
    @test MeterT(5) + MeterT(-3) ≈ MeterT(2)
    @test MeterT(5) + -MeterT(3) ≈ MeterT(2)
  end

  @testset "broadcasting" begin
    @test isa([1,2,3] .* MeterT(4), Vector{MeterT})
    for m in MeterT.([1,2,3])
      @test m≈MeterT(1) || m≈MeterT(2) || m≈MeterT(3)
    end
  end

  @testset "range" begin
    c = LinRange(MeterT(10), MeterT(20), 4)
    @test c[1] ≈ MeterT(10)
    @test c[2] ≈ MeterT(13+1/3)
    @test last(c) ≈ MeterT(20)
  end

  # @testset "range _colon" begin # 251105 - implementing colon in v1.11+ causes all sorts of mucking around in Base, omit
  #   b = MeterT(1) : MeterT(0.3) : MeterT(2) 
  #   @test b[1] ≈ MeterT(1)
  #   @test b[2] ≈ MeterT(1.3)
  #   @test last(b) ≈ MeterT(1.9)

  #   # = MeterT(1) : CentiMeterT(0.3) : MeterT(2)

  #   @test_throws MethodError GrowlT(1) : MeterT(0.3) : MeterT(2) 
  #   @test_throws MethodError MeterT(1) : GrowlT(0.3) : MeterT(2) 
  #   @test_throws MethodError MeterT(1) : MeterT(0.3) : GrowlT(2) 
  # end
end

function skipSymbolBlock(eexp)
  if eexp.head == :block
    return eexp.args[2]
  else
    return eexp
  end
end

"""
  `function measure2String(m::AbstractMeasure)::String`

  Returns a string representing measure `m` in the format "1.23mm".
"""
function measure2String(m::AbstractMeasure)::String
  # return @sprintf("%3.3f []", m)
  return "$(m.value)$(abbreviation(m))"
end
@testitem "Measure measure2string()" begin
  @makeBaseMeasure LengthT MeterT "mT"
  @test UnitTypes.measure2String(MeterT(3.4)) == "3.4mT"
  @test string(MeterT(3.4)) == "3.4mT"
end

"""
  `function Base.show(io::IO, m::AbstractMeasure)`

  @show functionality for Measures via `measure2String()`.
"""
function Base.show(io::IO, m::AbstractMeasure)
  print(io, measure2String(m))
end

"""
  `function toBaseFloat(m::AbstractMeasure) :: Float64`

  Returns measure `m` as a float in the base unit.
""" 
function toBaseFloat(m::AbstractMeasure) :: Float64
  return UnitTypes.allUnitTypes[typeof(m)].toBase(m.value)
end
@testitem "toBaseFloat" begin
  @makeBaseMeasure LengthT MeterT "mT"
	@makeMeasure MeterT = CentiMeterT "cmT" x->x/100 x->x*100
	@makeMeasure MeterT = KiloMeterT "kmT" x->x*1000 x->x/1000

  @test toBaseFloat(CentiMeterT(100)) ≈ 1
  @test toBaseFloat(KiloMeterT(1)) ≈ 1000
end

"""
  `macro u_str(unit::String)`

  Macro to provide the 1.2u"cm" inline unit assignment.
  ```
  a = 1.2u"cm" 
  ```

  This works by looking up the unit string in `allUnitTypes` and returning the corresponding type.
  See https://docs.julialang.org/en/v1/manual/metaprogramming/#meta-non-standard-string-literals
"""
macro u_str(unit::String)
  aut = filter( pairKV -> last(pairKV).abbreviation == unit, UnitTypes.allUnitTypes) # last(pairKV) == value == UnitTypeAttributes).abbreviation ==unit
  if !isempty(aut)
    b = first(first(aut))(1) # MeterT(1)
    return b
  end
  @warn "did not find $unit in `allUnitTypes`, returning 0"
end
@testitem "u_str" begin
  @makeBaseMeasure UStrTest UsT "usT"
  @test isa(1u"usT", UsT)
end

"""
  `macro relateMeasures(relation)`

  Adds a multiplicative relationship between the left and right sides of the equation, allowing units to be multiplied and divided with consistent units.
  All types must already be defined and only one * is supported on the left side, while the right should the resultant type.

  ```
    @relateMeasures Meter*Newton = NewtonMeter
  ```
"""
macro relateMeasures(relation)
  # @relateMeasures Meter*Newton = NewtonMeter
  # an alternate format would be: @relateMeasures Meter(1)*Centimeter(100)=Meter2(1), adding conversion...
  if length(relation.args) == 2# && isa(relation.args[2], Expr)
    operator = relation.args[1].args[1] # *
    TM = relation.args[1].args[2] # TM
    TN = relation.args[1].args[3] # TN
    TNM = relation.args[2].args[2] # TNM

    qts = [ quote end ] # build expressions in quotes

    # iterate through all types with same supertype to add concrete operations
    if operator == :*
      push!(qts, quote 
        for m in filter(kv->kv.second.abstract == UnitTypes.allUnitTypes[$TM].abstract, UnitTypes.allUnitTypes) # loops have to be in quote for TM, TN, TNM to resolve
          for n in filter(kv->kv.second.abstract == UnitTypes.allUnitTypes[$TN].abstract, UnitTypes.allUnitTypes)
            # println("Base.:*(x::$(first(m)), y::$(first(n))) = $($TNM)( convert($($TM), x).value * convert($($TN),y).value ) ")
            if !hasmethod(Base.:*, (first(m), first(n)))
              Base.:*(x::first(m), y::first(n)) = $TNM( convert($TM, x).value * convert($TN,y).value ) # x <: Abstract, so convert everything to the types given in the relation
            end
            if !hasmethod(Base.:*, (first(n), first(m)))
              Base.:*(x::first(n), y::first(m)) = $TNM( convert($TN, x).value * convert($TM,y).value ) # swapped
            end
            if !hasmethod(Base.:/, ($TNM, first(m)))
              Base.:/(x::$TNM, y::first(m)) = $TN( convert($TNM,x).value / convert($TM,y).value )
            end
            if !hasmethod(Base.:/, ($TNM, first(n)))
              Base.:/(x::$TNM, y::first(n)) = $TM( convert($TNM,x).value / convert($TN,y).value )
            end
          end
        end
        if $TM == $TN
          Base.sqrt(x::$TNM) = $TM( sqrt(x.value) ) # I can define sqrt(m^2) -> m, but I cannot define x^0.5 b/c the exponent might not lead to a known or integer unit..
        end
      end)
    elseif operator == :/ # as in pressure: N/m^2 = Pa
      push!(qts, quote
        for m in filter(kv->kv.second.abstract == UnitTypes.allUnitTypes[$TM].abstract, UnitTypes.allUnitTypes)
          for n in filter(kv->kv.second.abstract == UnitTypes.allUnitTypes[$TN].abstract, UnitTypes.allUnitTypes)
            if !hasmethod(Base.:/, (first(m), first(n)))
              Base.:/(x::first(m), y::first(n)) = $TNM( convert($TM, x).value / convert($TN,y).value ) # F/m2 = Pa
            end
            if !hasmethod(Base.:*, (first(m), $TNM))
              Base.:*(x::first(m), y::$TNM) = $TN( convert($TM,x).value * y.value ) # m2 * Pa = N
            end
            if !hasmethod(Base.:*, ($TNM, first(m)))
              Base.:*(x::$TNM, y::first(m)) = $TN( x.value * convert($TM, y)) # Pa * m2 = N
            end

            if !hasmethod(Base.:*, (first(n), $TNM))
              Base.:*(x::first(n), y::$TNM) = $TM( convert($TN,x).value * y.value ) # N * Pa = m2
            end
            if !hasmethod(Base.:*, ($TNM, first(n)))
              Base.:*(x::$TNM, y::first(n)) = $TM( x.value * convert($TN,y).value ) # Pa * N = m2
            end
          end
        end
      end)
    else
      throw(ArgumentError("Operator $operator unknown, @relateMeasures accepts only multiplicative measures in the format: @relateMeasures Meter*Newton=NewtonMeter"))
    end
    return esc( Expr(:block, qts...))

  else
    throw(ArgumentError("@relateMeasures given incorrect format"))
  end
end
@testitem "relateMeasures" begin
  # multiplicative same
  @makeBaseMeasure LengthTest MeterT "mT"
  @makeBaseMeasure AreaTest Meter2T "m2T"
  @relateMeasures MeterT*MeterT = Meter2T
  @test MeterT(2)*MeterT(3) ≈ Meter2T(6)
  @test Meter2T(1) / MeterT(1) ≈ MeterT(1)

  # multiplicative different
  @makeBaseMeasure TorqueTest NewtonMeterT "NMT"
  @makeBaseMeasure ForceTest NewtonT "NT"
  @relateMeasures NewtonT*MeterT = NewtonMeterT
  @test NewtonT(2) * MeterT(3) ≈ NewtonMeterT(6) # as defined
  @test MeterT(2) * NewtonT(3) ≈ NewtonMeterT(6) # swapped
  @test NewtonMeterT(6) / MeterT(2) ≈ NewtonT(3)
  @test NewtonMeterT(6) / NewtonT(2) ≈ MeterT(3)
  
  # sqrt
  @test sqrt(Meter2T(4)) ≈ MeterT(2)

  # division 
  @makeBaseMeasure PressureTest PascalT "PaT"
  @relateMeasures NewtonT / Meter2T = PascalT
  @test NewtonT(6)/Meter2T(2) ≈ PascalT(3)
  @test PascalT(3)*Meter2T(2) ≈ NewtonT(6)
  @test Meter2T(2)*PascalT(3) ≈ NewtonT(6)

  #non-operator
  @test_throws ArgumentError macroexpand(@__MODULE__, :( @relateMeasures MeterT%Meter2T = NewtonT  )) 

  # coefficient of thermal expansion?, to check with affine unit:
end


