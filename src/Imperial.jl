
module Imperial
  using TestItems 
  using ..AbsMeasure
  using ..AbsExtent


  @makeMeasure Inch AbstractExtent 25.4/1000 "in"
  @makeMeasure Foot AbstractExtent 12*25.4/1000 "ft"
  export Inch, Foot

end