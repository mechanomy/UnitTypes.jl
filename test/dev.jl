module UnitTypesDev
  using UnitTypes

  # make unit labels functions? mm(1.2) is /functional/...  this is an easy add to the existing macros
  # ..and it makes it easy to avoid a list of units and instead do return mm(1) to get Meter(1)
  # but this will fail in the literal mapping of m/s => m/s(3.4), unless having m(), /, and s(3.4) defined it then does the math between the units...
  
  #parse unit into bits
  unit = "Kg*m/s^2" # -> Kg(1) * m(1) / s(1)^2

  Kg = KiloGram
  m = Meter
  s = Second


  @show a = Meta.parse(unit)
  @show a.args
  mul = a.args[1]
  num = a.args[2]
  den = a.args[3]

  @show num.args
  nkg = eval(num.args[2])
  @show nkg(1.2)
  nm = eval(num.args[3])
  
  @show den.args
  @show ds = eval(den.args[2])
  
  @show eval(a)
  eval()



end
;