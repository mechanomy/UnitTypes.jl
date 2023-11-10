module UnitTypes
  using Reexport
  using TestItems

  include("Measure.jl")
  @reexport using .Measure

  # # SI Base: https://en.wikipedia.org/wiki/International_System_of_Units
  include("SIBase.jl")

  # derived units
  include("SIDerived.jl")
  include("Imperial.jl")
  include("Angle.jl")



  # #Concepts involving units
  # include("Dimension.jl")
  # @reexport using .Dimension

  # include("Dimensions.jl")
  # @reexport using .Dimensions





end

