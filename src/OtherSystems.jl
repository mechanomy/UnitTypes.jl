# Other unit systems 

# Other / natural units
@makeMeasure 1.66053906660e-27 KiloGram = 1 UnifiedAtomicMassUnit "u"
@makeMeasure 9.80665 MeterPerSecond2 = 1 EarthGravity "ge"

@testitem "OtherSystems" begin
  @test isapprox(EarthGravity(1), MeterPerSecond2(9.80665), atol=1e-5)
end


@makeBaseMeasure Percentage Percent "%" # not a physical unit...
@makeMeasure 1e-2 Percent = 1 BasisPoints "bps" # 100bps in 1%
@makeMeasure 1e-1 Percent = 1 Permille "‰"
@makeMeasure 1e-2 Percent = 1 Pertenthousand "‱"
@makeMeasure 1e-3 Percent = 1 Percentmille "pcm"
@makeMeasure 1e-4 Percent = 1 Permillion "ppm"
@makeMeasure 1e-7 Percent = 1 Perbillion "ppb"
@makeMeasure 1e-10 Percent = 1 Pertrillion "ppt"
@makeMeasure 1e-13 Percent = 1 Perquadrillion "ppq"


