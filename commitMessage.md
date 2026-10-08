# Trigonometry on AbstractAngle

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Forward trig `sin`, `cos`, `tan`, `sec`, `csc`, `cot`, `sincos` are defined once on `AbstractAngle`, returning `Float64`.
* `Degree` dispatches to Base's `sind`, `cosd`, etc., so values at multiples of 90° are exact, e.g. `sin(Degree(180)) === 0.0`.
* Inverse trig takes the result angle type first: `asin(Radian, 0.5)`, `acos(Degree, x)`, plus `atan`, `asec`, `acsc`, `acot`; Base's `asin(::Real)` is unchanged.
* Two-argument `atan(y, x)` accepts measures of the same dimension in any units and returns `Radian`; `atan(Degree, y, x)` selects the angle type; mismatched dimensions throw an ArgumentError.
* `pi` and `tau` are no longer exported, which fixes `pi` being undefined in user code after `using UnitTypes`; they remain as `const UnitTypes.pi` and `UnitTypes.tau`.
* Fixed `Oersted` conversion, which threw a MethodError because CGS.jl's `pi` resolved to the Radian constant; it now uses `Base.pi`, with a regression test.
* New conversions use `convert()` rather than `toBaseFloat()` or conversion constructors, keeping every new method allocation-free.
* Added testitems for reciprocal functions, sincos, exact degree values, inverse trig, two-argument atan, and the unexported constants.
* Added trigonometry, inverse trigonometry, and two-argument atan sections to benchmark/benchmark.jl; all report 0 allocations.
* Postponed: angle as a dimension in BaseDimensions, additional angle units (ArcMinute, Turn, Gradian, RPM).

# Density cancellation and Catchall resolution

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Fixed `GramPerCentiMeter3`, which was defined as 1e-3 kg/m^3 instead of 1000 kg/m^3.
* Density, SpecificVolume, and SurfaceDensity are now connected to the SI base units via `@relateMeasures`, so they have SI dimensions; previously their dimensions were silently dropped in Catchall arithmetic, e.g. `1.2u"g/cm^3" * mm * mm * mm` returned Meter3 instead of KiloGram.
* Fixed the `/` form of `@relateMeasures` (M/N=NM), which defined M*NM = N and NM*M = N (e.g. Newton*Pascal returned Meter2, Pascal*Newton threw); it now defines M/NM = N, N*NM = M, and NM*N = M.
* Added `convert(T, x::Catchall)` and a per-type `T(x::Catchall)` constructor that check the SI dimensions match, so `Gram(catchallMass)` works and a mismatch throws an ArgumentError.
* Added Density and Catchall conversion testitems, including the acrylic sheet example; all tests pass and the benchmark reports 0 allocations.
* Added `angle` as an eighth field of `BaseDimensions` and anchored `AbstractAngle` to it in Angle.jl, so Radian and Degree carry dimensions through Catchall arithmetic instead of being dropped with a warning; e.g. `Radian(2)*Meter(3)` is now a Catchall `m*rad` rather than `Meter`, and dividing by Meter resolves back to Radian.
* Catchall tests now build BaseDimensions with keyword arguments rather than positional ones; added an "Angle base dimension" testitem.
* Mapped Steradian, Lumen, and Lux to SI dimensions via `@relateMeasures Radian*Radian=Steradian`, `Candela*Steradian=Lumen`, and `Lumen/Meter2=Lux` in Angle.jl, which must follow the angle anchor; sr = rad^2, lm = cd*rad^2, lx = cd*rad^2/m^2.
* Moved testitems to sit immediately after the code they test: Density, SpecificVolume, and SurfaceDensity (with the acrylic example) each follow their definitions; the Catchall conversion and angle base dimension testitems follow their definitions; added an "addRelations division form" testitem after addRelations and a per-type Catchall constructor check to the makeSelfConversion testitem.
* Moved the Radian and Degree definitions, the AbstractAngle anchor, and their testitems from Angle.jl into SI.jl after the base-unit seeds; Angle.jl now holds only the pi/tau constants and trigonometry.
* Moved the Steradian, Lumen, and Lux relations into SI.jl next to their definitions, each followed by its own testitem.
* Moved the generic `sameUnitValue` into Measure.jl and its Catchall overload into Catchall.jl (Catchall is not yet defined when Measure.jl loads), each with a testitem; the error message no longer names atan.
* Bumped version to 3.0.2.

# Julia compat lower bound

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Lowered the `julia` compat bound from "1" to "1.10" (the LTS) so the General registry AutoMerge no longer tries to install on Julia 1.1.1.
* Relaxed the Printf compat from "1.11.0" to "1"; the stdlib version tracks Julia, so the old bound silently required Julia 1.11.
* Added Julia 1.10 back to the CI matrix; all 494 tests pass on 1.10 and 1.12.
* Bumped version to 3.1.0 instead of 3.0.2, because the registry forbids a patch release from narrowing the supported Julia range; the new angle dimension and Catchall conversions also make this a minor release.

# United States customary units

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Added src/USCustomary.jl following https://en.wikipedia.org/wiki/United_States_customary_units, included after Imperial.jl; units shared with the imperial system (Inch, Foot, Yard, Mile, Ounce, PoundMass, Grain, Dram) stay in Imperial.jl.
* Volumes whose US and imperial definitions differ carry a US prefix (USGallon "usgal", USQuart, USPint, USCup, USFluidOunce, USPeck, USBushel, ...), so the existing imperial Gallon, Quart, Pint, and FluidOunce are unchanged in name.
* Length: Point, Pica, Fathom, Cable, Link, Rod, Chain, Furlong, League (on the international foot, per NIST since 2023), plus SurveyFoot "ftUS" and SurveyMile "miUS".
* Area: Yard2, Chain2, Section, Township; volume: Inch3, Foot3, Yard3, AcreFoot.
* Fluid volume: USMinim, USFluidDram, USTeaspoon, USTablespoon, USFluidOunce, USJigger, USGill, USCup, USPint, USQuart, USGallon, USBarrel, OilBarrel, USHogshead; dry volume: USDryPint, USDryQuart, USDryGallon, USPeck, USBushel, USDryBarrel.
* Mass: ShortHundredweight, LongHundredweight, ShortTon, LongTon, and troy Pennyweight, TroyOunce, TroyPound.
* Force and pressure: KiloPoundForce "kip", PoundsPerSquareFoot "psf", KiloPoundsPerSquareInch "ksi", InchOfMercury, InchOfWater; torque: PoundFoot "lbf*ft", PoundInch "lbf*in".
* Energy and power: FootPound "ft*lbf", Therm, Horsepower, BritishThermalUnitPerHour, TonOfRefrigeration; velocity and acceleration: FootPerMinute, InchPerSecond, Knot, FootPerSecond2; density: PoundMassPerFoot3, PoundMassPerInch3.
* Each group is followed by its own testitem; tests use isapprox with an explicit rtol because the package's isapprox defaults to exact comparison.
* Fixed the imperial FluidOunce, Pint, Quart, and Gallon, which were 1000x too large (e.g. Gallon was 4.546 m^3); also corrected the Pint value from 568.26126 to 568.26125 mL; added an "Imperial volume" regression testitem.
* Fixed MilliLiter, which referenced Liter rather than the base unit Meter3, so MilliLiter(1000) equaled 1 m^3; added a "Volume" regression testitem.
* Regenerated docs/unitTypesTree.md; all 583 tests pass and the benchmark reports 0 allocations.
* Switched Acre from the old US survey acre (4046.873 m^2) to the international acre, 43560 international square feet = 4046.8564224 m^2; Acre(640) now equals Mile2(1) to rtol=1e-12, and Chain2(10) equals Acre(1).
* Bumped version to 3.2.0.

# Fix and test displayInUnits

Date: 2026-10-06
Model: claude-opus-5-5
Effort: low

* Fixed `displayInUnits`, which always threw an UndefVarError because it called the nonexistent `unitString`; it now uses the registered abbreviation and `convert()`.
* The dimension check now compares registered abstract types rather than `supertype`, and the error message names the expected quantity.
* Added an optional leading `io::IO` argument so output can be captured; the method returns `nothing`.
* Exported `displayInUnits` and expanded its docstring with an example.
* Added a "displayInUnits" testitem covering multiple units, same unit, no units, angles, mass, mismatched units, and the stdout method; full suite passes (592 tests).

# Fix @relateMeasures for relations not expressed in base units
2026-10-08, claude-opus-5-5, effort: low

* `@relateMeasures Dollar/Foot = DollarPerFoot` gave wrong results because addRelations multiplied raw base values, treating $/ft as $/m (4307mm * 2.79$/ft gave 12.02$ instead of 39.42$).
* Added `relationScale` to compute the base-value scale factor implied by the relation's own units; addRelations folds it into every generated method.
* Base-consistent relations (all existing SI relations) have a scale of exactly 1, so their behavior is unchanged.
* Power registration is skipped for scaled relations since registerPower assumes base-consistent types.
* Added testitems for relationScale and for non-base relations; all tests pass and the benchmark stays at zero allocations.
