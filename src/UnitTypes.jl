module UnitTypes
  # __precompile__(false)
  using DocStringExtensions
  using Reexport
  using TestItems

  include("Measure.jl")
  include("SI.jl")
  include("Imperial.jl")
  include("Angle.jl")
  include("Temperature.jl")

  #Dimensions built on Measures
  include("Dimension.jl")
  include("CommonDimensions.jl")
end

