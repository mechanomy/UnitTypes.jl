module UnitTypes
  using DocStringExtensions
  using Reexport
  using TestItems
  # using Unitful #need to import here because this is where macros evaluate

  # @template DEFAULT =
  #   """
  #   $(TYPEDSIGNATURES)
  #   $(DOCSTRING)
  #   """




  include("Measure.jl")
  @reexport using .Measure

  # # SI Base: https://en.wikipedia.org/wiki/International_System_of_Units
  include("SIBase.jl")

  # derived measures
  include("SIDerived.jl")
  include("Imperial.jl")
  include("Angle.jl")

  #Dimensions built on Measures
  include("Dimension.jl")

  include("CommonDimensions.jl")

  include("ExchangeUnitful.jl")
  # using .ExchangeUnitful

  # display(names(Measure.@returnModuleName, imported=true, all=true))


end

