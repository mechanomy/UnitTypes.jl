module UnitTypes
  include("AbsMeasure.jl")
  using .AbsMeasure # the . indicates a local, sub package
  export AbstractMeasure, @makeMeasure, @makeMeasureFromAbstract

  include("AbsExtent.jl")
  using .AbsExtent
  export AbstractExtent

  # include("SIBase.jl")
  # using .SIBase
  export Meter

  include("SI.jl")
  using .SI
  export MilliMeter
  
  include("Imperial.jl")
  using .Imperial
  export Inch, Foot #reexport?

  include("AbsAngle.jl")
  using .AbsAngle 
  export AbstractAngle, Degree, Radian


  include("AbsDimension.jl")
  using .AbsDimension
  export AbstractDiameter, Diameter, Radius, Length, Width, Height





end

