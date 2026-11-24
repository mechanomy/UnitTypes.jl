# Feature request: a public package-load callback

Draft for an issue on JuliaLang/julia, 2026-11-24.

## Title

Public API to run code when any package is loaded (stabilize `Base.package_callbacks`)

## Summary

A package that keeps a registry, such as a table of unit types, cannot learn when a dependent package that adds to it is loaded.
`Base.package_callbacks` does this today, but it is internal and undocumented.
Please make a load callback public, so a registry package can replay its dependents' registrations without each dependent having to opt in.

## Motivation

UnitTypes.jl keeps a global `allUnitTypes` table (plus two smaller ones) that its macros fill: `@makeBaseMeasure Length Meter "m"` defines the type and methods and also writes an entry to the table.
Arithmetic and conversion look up that table at run time.

When a dependent package such as Mechanics.jl uses these macros, its methods are saved in its precompile cache, but its writes to UnitTypes' table are not, because a package cache only holds the package's own state.
After loading the cached package the types exist but are not registered, and `Meter(2)^2 * Meter(3)^2` throws `KeyError: key Meter4 not found`.

The general pattern is any registry whose entries are added by dependents at load time: unit systems, plugin or format registries, serializer tables, trait tables keyed by type.

## Current workarounds

1. Each dependent package calls a replay function from its own `__init__`.
   UnitTypes now does this: the macros also record each entry in a constant in the defining module, and `UnitTypes.@registerMeasures` in that module's `__init__` merges it into the global tables.
   It works, but every dependent must remember the call, and forgetting it fails silently until a lookup misses, and only in precompiled use, not in the REPL or tests run from source.
2. `__precompile__(false)` in the dependent package.
   This makes the package unloadable from any precompiled package, so it does not compose.
3. `push!(Base.package_callbacks, f)`, as Requires.jl does.
   This would let UnitTypes find every loaded module that has a registry constant and replay it, with no change to dependents.
   It is internal, so its signature and timing may change without notice.
4. Package extensions do not help, because they trigger on a fixed pair of packages, not on "any package that depends on me".

## Proposal

A documented, public way to register a function that is called with the `Base.PkgId` of each package after it has finished loading, including its `__init__`, for example:

```julia
Base.add_package_callback(f::Function) # f(pkg::Base.PkgId) is called after each package loads
```

or a public, documented `Base.package_callbacks` with the same semantics.

Desirable properties:
- It is called for packages loaded from a precompile cache and for packages loaded from source.
- It is called after the package's `__init__`, so the package's own state is ready.
- It is called in the latest world, so the callback can access bindings and methods the package defined.
- There is a public way to list modules already loaded when the callback is registered, such as a documented `Base.loaded_modules`, so a registry package can catch up on dependents loaded earlier.

## How UnitTypes would use it

```julia
function __init__()
  Base.add_package_callback() do pkg
    mod = Base.loaded_modules[pkg]
    isdefined(mod, :unitTypesRegistry) && registerMeasures(mod)
  end
end
```

Dependents would then need neither `__precompile__(false)` nor an `__init__` call, and the existing `@registerMeasures` would remain harmless, since replaying is idempotent.

## Alternatives considered

- Saving a dependent's writes to another module's globals in the dependent's cache.
  That would be convenient here, but is a much larger change with unclear semantics when several packages write the same key.
- A hook scoped to dependents, such as `Base.on_dependent_load(UnitTypes, f)`, which runs `f` only for packages that depend on UnitTypes.
  This is narrower and cheaper than a global callback, and would serve this use equally well.
