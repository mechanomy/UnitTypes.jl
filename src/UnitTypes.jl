module UnitTypes
  using Reexport

  include("Measure.jl")
  # using .Measure # the . indicates a local, sub package
  # export tractMeasure, @makeMeasure
  @reexport using .Measure

  # SI Base: https://en.wikipedia.org/wiki/International_System_of_Units
  include("Current.jl")
  @reexport using .Current

  include("Intensity.jl")
  @reexport using .Intensity

  include("Length.jl")
  @reexport using .Length

  include("Mass.jl")
  @reexport using .Mass

  include("Temperature.jl")
  @reexport using .Temperature

  include("Time.jl")
  @reexport using .Time

  # derived units
  include("SIDerived.jl")
  @reexport using .SIDerived
  
  include("Imperial.jl")
  @reexport using .Imperial

  # non-physical units:
  include("Angle.jl")
  @reexport using .Angle 



  #Concepts involving units
  include("Dimension.jl")
  @reexport using .Dimension

  include("Dimensions.jl")
  @reexport using .Dimensions





end

