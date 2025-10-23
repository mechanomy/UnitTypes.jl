module UnitTypesDev

  # Can the @makeMeasure macro parse functions for toBase and fromBase?

  abstract type AbstractMeasure end
  struct UnitTypeAttributes
    abstract::DataType # the abstract type of this type, say AbstractLength for MilliMeter
    base::DataType # Meter
    toBase::Function
    fromBase::Function
    abbreviation::String # "mm"
  end
  allUnitTypes = Dict{DataType, UnitTypeAttributes}()

  macro makeBaseMeasure(quantityName, unitName, abbreviation::String, isAffine=false)
    # println("makeBaseMeasure: Module:$(__module__) quantityName:$quantityName unitName:$unitName abbreviation:$abbreviation")
    abstractName = Symbol("Abstract"*String(quantityName)) #AbstractLength

    if unitName in keys(UnitTypesDev.allUnitTypes)
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
        $unitName(x::T where T<:$abstractName) = convert($unitName, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
        export $unitName

        UnitTypesDev.allUnitTypes[$unitName] = UnitTypesDev.UnitTypeAttributes($abstractName, $unitName, f->f, f->f, $abbreviation) # add to the Dict

      end
    ]
    return esc( Expr(:block, qts...))
  end # makeBaseMeasure

  @makeBaseMeasure Length Meter "m"
  @makeBaseMeasure Temperature Kelvin "K"

  macro makeMeasure(relation, newAbbreviation, newToBase, newFromBase)
    # @makeMeasure Kelvin = Fahrenheit "F" f->(f+459.67)*5/9 k->k*9/5-459.67 
    # display(dump(relation)) # Kelvin = Fahrenheit
    existingType = relation.args[1] # Kelvin
    newType = relation.args[2] # Fahrenheit
    qts = [quote
      existingSupertype = supertype($existingType)

      """
        This UnitType represents a basic measure of $($newType) with units $($newAbbreviation).
      """
      struct $newType <: existingSupertype
        value::Float64 # the value on creation as measured in the unit
      end
      $newType(x::T where T<:existingSupertype) = convert($newType, x) # conversion constructor: MilliMeter(Inch(1.0)) = 25.4mm
      export $newType

      UnitTypesDev.allUnitTypes[$newType] = UnitTypesDev.UnitTypeAttributes(existingSupertype, $newType, $newToBase, $newFromBase, $newAbbreviation) 
    end]
    return esc( Expr(:block, qts...))

  end
	@makeMeasure Kelvin = Fahrenheit "F" f->(f+459.67)*5/9 k->k*9/5-459.67 
  @show allUnitTypes[Fahrenheit].toBase(32) # 273.15
  @show allUnitTypes[Fahrenheit].fromBase(273.15) # 32

	@makeMeasure Meter = CentiMeter "cm" x->x/100 x->x*100
  @show allUnitTypes[CentiMeter].toBase(123) # 1.23
  @show allUnitTypes[CentiMeter].fromBase(1.23) # 123

  # display(allUnitTypes)
end;
