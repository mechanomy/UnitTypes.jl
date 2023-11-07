
module Imperial
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent


  @makeMeasure Inch "in" 25.4/1000 Meter
  @makeMeasure Foot "ft" 12*25.4/1000 Meter
  export Inch, Foot

end