module UnitTypesDev
  using UnitTypes

  # 251017 - given allUnitTypes, can I iterate through and make all of the cross definitions?
  # display(UnitTypes.allUnitTypes)

  function makeCrossDefinitions()

    aut = filter( pairKV->last(pairKV).abstract == UnitTypes.AbstractArea , UnitTypes.allUnitTypes) 
    display(aut)

    println("pre loop")
    display(methods(convert,UnitTypesDev)) 

    for a in aut # is there a nested iterator?
      for b in aut
        if a!=b
          eval( :( Base.convert(::Type{$(a.first)}, y::$(b.first)) = $(a.first)(y.value * b.second.toBaseFactor / a.second.toBaseFactor ) ))
        end
      end
    end

    println("post loop")
    display(methods(convert,UnitTypesDev)) 
  end


  makeCrossDefinitions()

end
;
