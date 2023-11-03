module UnitTypes
  include("AbsMeasure.jl")
  using .AbsMeasure # the . indicates a local, sub package
  export AbstractMeasure, @makeMeasure

  include("AbsExtent.jl")
  using .AbsExtent
  export AbstractExtent, Meter, MilliMeter, Inch, Foot #reexport

  include("AbsAngle.jl") # module
  using .AbsAngle 
  export AbstractAngle, Degree, Radian

  include("AbsDimension.jl")
  using .AbsDimension
  export AbstractDiameter, Diameter, Radius, Length, Width, Height

end

