export @registerMeasures

"""
  `struct ModuleRegistry`

  The entries a module other than UnitTypes has added to `allUnitTypes`, `powerTypes` and `abstractToSI`.
  A module that defines measures gets one as the constant `unitTypesRegistry`, created on its first registration.
  Writes to UnitTypes' global tables made while another package precompiles are not saved in that package's cache, so the package replays this record from its `__init__` with `@registerMeasures`.
"""
struct ModuleRegistry
  allUnitTypes::Dict{DataType, UnitTypeAttributes}
  powerTypes::Dict{Tuple{DataType,Int}, DataType}
  abstractToSI::Dict{DataType, BaseDimensions}
end
ModuleRegistry() = ModuleRegistry(Dict{DataType, UnitTypeAttributes}(), Dict{Tuple{DataType,Int}, DataType}(), Dict{DataType, BaseDimensions}())

"""
  `moduleRegistry(mod::Module) -> ModuleRegistry`

  Returns `mod.unitTypesRegistry`, defining it as a constant in `mod` on first use.
"""
function moduleRegistry(mod::Module)::ModuleRegistry
  # invokelatest because the binding is created by Core.eval in a newer world than the caller's, which Julia 1.12 warns about
  if !Base.invokelatest(isdefined, mod, :unitTypesRegistry)
    Core.eval(mod, :(const unitTypesRegistry = $(ModuleRegistry())))
  end
  return Base.invokelatest(getfield, mod, :unitTypesRegistry)
end

"""
  `setUnitType!(mod, T, uta)`, `setPowerType!(mod, key, T)`, `setAbstractToSI!(mod, abstractType, dims)`

  Write one entry to UnitTypes' global tables and, when `mod` is not UnitTypes, also record it in `mod`'s `unitTypesRegistry` so `@registerMeasures` can replay it.
  The macros call these; a package that extends `abstractToSI` directly, as for a new base dimension, should use `UnitTypes.setAbstractToSI!(@__MODULE__, ...)`.
"""
function setUnitType!(mod::Module, T::DataType, uta::UnitTypeAttributes)
  allUnitTypes[T] = uta
  mod === UnitTypes || (moduleRegistry(mod).allUnitTypes[T] = uta)
  return nothing
end

function setPowerType!(mod::Module, key::Tuple{DataType,Int}, T::DataType)
  powerTypes[key] = T
  mod === UnitTypes || (moduleRegistry(mod).powerTypes[key] = T)
  return nothing
end

function setAbstractToSI!(mod::Module, abstractType::DataType, dims::BaseDimensions)
  abstractToSI[abstractType] = dims
  mod === UnitTypes || (moduleRegistry(mod).abstractToSI[abstractType] = dims)
  return nothing
end

"""
  `registerMeasures(mod::Module)`

  Replays the registrations recorded in `mod.unitTypesRegistry` into UnitTypes' global tables; does nothing if `mod` registered none.
  Replaying is idempotent, so it is harmless when the package was not loaded from a precompile cache.
"""
function registerMeasures(mod::Module)
  Base.invokelatest(isdefined, mod, :unitTypesRegistry) || return nothing
  registry = Base.invokelatest(getfield, mod, :unitTypesRegistry)
  merge!(allUnitTypes, registry.allUnitTypes)
  merge!(powerTypes, registry.powerTypes)
  merge!(abstractToSI, registry.abstractToSI)
  return nothing
end

"""
  `@registerMeasures`

  Call from the `__init__` of any package that defines measures with `@makeBaseMeasure`, `@makeMeasure` or `@relateMeasures`:
  ```
  module MyMeasures
    using UnitTypes
    @makeBaseMeasure HyperVolume Meter5 "m^5"
    @relateMeasures Meter4*Meter=Meter5
    function __init__()
      @registerMeasures
    end
  end
  ```
  Without it, a precompiled package's types and methods load but are missing from `UnitTypes.allUnitTypes`, so operations that look them up, like `Meter2(1)*Meter3(1)`, throw a KeyError or return Catchall.
"""
macro registerMeasures()
  return :($(@__MODULE__).registerMeasures($__module__))
end

@testitem "registerMeasures records and replays" begin
  @makeBaseMeasure RegistryLengthTest RegMeterT "regmT"
  @makeBaseMeasure RegistryAreaTest RegMeter2T "regm2T"
  @relateMeasures RegMeterT*RegMeterT=RegMeter2T
  @makeMeasure 1e-3 RegMeterT = 1 RegMilliMeterT "regmmT"

  registry = UnitTypes.moduleRegistry(@__MODULE__) # created in this testitem's module by the first registration
  @test registry isa UnitTypes.ModuleRegistry
  @test haskey(registry.allUnitTypes, RegMeterT)
  @test haskey(registry.allUnitTypes, RegMilliMeterT)
  @test registry.allUnitTypes[RegMeter2T].dimensions == Dict{DataType,Int}(AbstractRegistryLengthTest => 2)
  @test registry.powerTypes[(AbstractRegistryLengthTest, 2)] == RegMeter2T
  @test !isdefined(UnitTypes, :unitTypesRegistry) # UnitTypes' own registrations are not recorded

  # simulate loading from a precompile cache: remove the entries, then replay them
  for T in keys(registry.allUnitTypes)
    delete!(UnitTypes.allUnitTypes, T)
  end
  delete!(UnitTypes.powerTypes, (AbstractRegistryLengthTest, 2))
  @test !haskey(UnitTypes.allUnitTypes, RegMeterT)
  @registerMeasures
  @test UnitTypes.allUnitTypes[RegMilliMeterT].abbreviation == "regmmT"
  @test UnitTypes.powerTypes[(AbstractRegistryLengthTest, 2)] == RegMeter2T
  @test RegMilliMeterT(RegMeterT(1)) ≈ RegMilliMeterT(1000)
  @registerMeasures # idempotent
  @test UnitTypes.allUnitTypes[RegMeterT].base == RegMeterT
end

@testitem "registerMeasures survives a package precompile" begin
  # RegisterFixture defines Meter5F and MilliMeter5F and calls @registerMeasures in its __init__.
  # A fresh process develops it and UnitTypes into a temporary environment, precompiles both, and loads them from the cache.
  root = pkgdir(UnitTypes)
  fixture = joinpath(root, "test", "fixtures", "RegisterFixture")
  envDir = mktempdir()
  script = """
    using Pkg
    Pkg.develop([PackageSpec(path=$(repr(root))), PackageSpec(path=$(repr(fixture)))]; io=devnull)
    using UnitTypes, RegisterFixture
    println(haskey(UnitTypes.allUnitTypes, Meter5F))
    println(haskey(UnitTypes.allUnitTypes, MilliMeter5F))
    println(UnitTypes.powerTypes[(UnitTypes.AbstractLength, 5)] == Meter5F)
    println(Meter2(2) * Meter3(3) isa Meter5F)
    println(Meter(2)^5 ≈ Meter5F(32))
    println(MilliMeter5F(Meter5F(1)) ≈ MilliMeter5F(1e15))
    println(getBaseDims(Meter5F(1)) == BaseDimensions(length=5))
  """
  cmd = addenv(`$(Base.julia_cmd()) --startup-file=no --project=$envDir -e $script`, "JULIA_LOAD_PATH" => "@:@stdlib", "JULIA_PKG_OFFLINE" => "true")
  results = split(strip(read(cmd, String)), "\n")
  @test length(results) == 7
  @test all(==("true"), results)
end
