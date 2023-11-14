
module UnitTypesDev

using UnitTypes
using InteractiveUtils


# @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS "kg*m*m/s/s"


"""
  makes a new unit derived from some combination of more basic units.
  makeDerived(:Power, "W = kg * m / s^3")
  will create a new unit type "Power" with symbol "W" constructed by Kilogram * Meter / Seconds^3
  This new unit can be constructed by powerInputVar = 3 W or powerInputVar = Power(3)

"""
function makeDerived(name, unitString)
  @show name
  @show unitString


  # parse the string
  if occursin("+", unitString)
    throw(ArgumentError("cannot have + in unit string"))
  end

  # if occursin("-", unitString)
  #   throw(ArgumentError("cannot have - in unit string, use * to multiply units, that is N*m instead of N-m"))
  # end


  if occursin("=", unitString) # full format "W = kg * m / s^3"
    reLHS = r"\s*([A-z\*\/\^0-9\-\s]*)\s*=" 
    # reRHS = r"=\s*([A-z\*\/\^0-9\-\s]*)\s*"

    mLHS = match(reLHS, unitString)
    # @show mLHS.match
    # @show mLHS.captures[1]

    #now look up the unit types and concatenate their names to make the new name
    # this uses regex to parse everything manually and would allow matching symbols to full names: s -> Second
    # reRHS = r"([\*\/])?\s*([A-Za-z]+)" # grab * or / and the unit
    # numList = []
    # denList = []
    # for m in eachmatch(reRHS, unitString)
    #   @show m
    #   # @show m.captures[1]
    #   # @show m.captures[2]
    #   if m.captures[1] == "*" || isnothing(m.captures[1])
    #     push!(numList, m.captures[2])
    #   end
    #   if m.captures[1] == "/"
    #     push!(denList, m.captures[2])
    #   end
    # end
    # display(numList)
    # display(denList)

    # this simpler version just makes the input acceptable for a type name
    m = match(r"=", unitString) # trim the left hand side
    newName = titlecase(unitString[m.offset+1:length(unitString)] )
    newName = replace(newName, " "=>"")
    newName = replace(newName, "*"=>"")
    newName = replace(newName, "/"=>"Per")
    # @show newName
    # @show gensym(unitString) # creates a safe symbol, a fallback for complex things args?

    # @makeDerivedMeasure Symbol(newName) mLHS.captures[1] 1.0 Meter3
    # @makeBaseMeasure Symbol("Generalized"*newName) Symbol(newName) mLHS.captures[1]
    # @show Symbol("Generalized"*newName) Symbol(newName) mLHS.captures[1]
    # @show @macroexpand @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS "kg*m*m/s/s" #export not at top level

    # @eval @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS "kg*m*m/s/s" #works
    # @eval @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS String(strip(mLHS.captures[1])) #fails

    # @eval @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS eval(String(strip(mLHS.captures[1]))) 
    # @eval @makeBaseMeasure GeneralizedKgMMPerSPerS KgMMPerSPerS esc(String(strip(mLHS.captures[1]))) 
    # @eval @makeBaseMeasure(:Gerald, :Ford, String(strip(mLHS.captures[1])))
    # @eval @makeBaseMeasure("Gerald", "Ford", String(strip(mLHS.captures[1])))

    # @eval @makeBaseMeasure(newName, newName, "kgm")
    # @eval @makeBaseMeasure(Symbol(newName), newName, "kgm")
    # @show gsym = gensym("Generalized"*newName)
    # @show usym = gensym("Unit"*newName)
    # @eval @makeBaseMeasure(gsym, usym, "kgm")

    # display(names(Measure.@returnModuleName, imported=true, all=true))
    # @show(names(UnitTypes ))
    display(names(UnitTypesDev, imported=true, all=true))

    #looking at the addUnitOperations macro, add * and / rules to be able to construct the new type from primitive types



    
  else

  end

end

# makeDerived(:Power, "W = kg * m / s^3.5 * slug^-2 * F^3/2") #rather hard
# makeDerived(:Power, "W = kg * m * s^-3")
makeDerived(:Power, "W = kg * m * m * / s / s")

end ;