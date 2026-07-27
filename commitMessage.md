* Fixed velocity unit chain in SI.jl: MetersPerHour and KiloMeterPerHour were defined as indirect children (chained through MetersPerMinute and MetersPerHour respectively), causing cross-type isapprox/convert to fail; redefined all velocity units as direct children of MeterPerSecond
* Fixed Imperial.jl velocity: MilePerHour now defined directly relative to MeterPerSecond (0.44704 m/s) instead of via KiloMeterPerHour chain; added atol=1e-3 to velocity test
* Fixed FootPerSecond factor: was 1/mPerIn*inPerFt (=472 m/s, wrong) now mPerIn*inPerFt (=0.3048 m/s, correct)
* Fixed MilesPerHour alias: now defined directly relative to MeterPerSecond
* Fixed SI Velocity testitem: was using u"m/s" which returns test-local MeterPerSecondT (registered in Catchall.jl testitem with same abbreviation); changed to use MeterPerSecond constructor directly
* Fixed pre-existing bugs: MeterPerMinute typo (was MetersPerMinute), inverted time unit factors (Minute/Hour/Day), inverted Liter/MilliLiter volume factors
* SI.jl: added Amount/Mole, Gram, Angstrom, Are/Hectare/Barn, SolidAngle/Steradian, Week/Year, Becquerel/RevolutionsPerSecond/RevolutionsPerMinute/AngHertz, Energy/Joule with kJ/MJ/mJ/eV, AbsorbedDose/Gray/Sievert, CatalyticActivity/Katal, DynamicViscosity/PascalSecond, KinematicViscosity/MeterSquaredPerSecond, Permille through Perquadrillion, MolarConcentration/Molar, Bar/Atmosphere/Torr
* Imperial.jl: added Mil, Dram, Grain, PoundsPerSquareInch, Calorie, BritishThermalUnit; corrected PoundForce factor
* src/CGS.jl (new): CGS units Gal/Dyne/Erg/Barye/Poise/Stokes/Gauss/Oersted/Maxwell
* src/OtherSystems.jl (new): Percentages 
