module UnitTypes
  using DocStringExtensions
  using Reexport
  using TestItems

  include("Measure.jl")
  @reexport using .Measure

  # SI Base: https://en.wikipedia.org/wiki/International_System_of_Units
  include("SIBase.jl")

  # derived measures
  include("SIDerived.jl")
  include("Imperial.jl")
  include("Angle.jl")

  #Dimensions built on Measures
  include("Dimension.jl")
  include("CommonDimensions.jl")
end

