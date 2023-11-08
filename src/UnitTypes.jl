module UnitTypes
  include("AbsMeasure.jl")
  using .AbsMeasure # the . indicates a local, sub package
  export AbstractMeasure, @makeMeasure

  include("AbsExtent.jl")
  using .AbsExtent
  export AbstractExtent

  include("SIBase.jl")
  using .SIBase
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
  export @makeDimension, AbstractDimension

  include("Dimensions.jl")
  using .Dimensions
  export Length, Width, Height
  export AbstractDiameter, Diameter, Radius





end

