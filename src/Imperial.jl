
module Imperial
  using TestItems 
  using ..Measure
  using ..Length
  # using ..SIBase

  @makeMeasure Inch "in" 25.4/1000 Meter
  @makeMeasure Foot "ft" 12*25.4/1000 Meter
  export Inch, Foot

  @testitem "Imperial" begin
    @testset "things I want to do" begin
      @test isapprox( Inch(Foot(1.2)), Inch(14.4), atol=1e-3) #conversion by constructor
    end
  end
end