# Zero-Allocation Unit Arithmetic in Julia

UnitTypes.jl provides unit-typed quantities — `Meter(1.5)`, `MilliMeter(25.4)`, `Inch(1.0)` — with the goal of zero heap allocations for arithmetic, conversion, comparison, and exponentiation. This post describes the techniques that make that possible.

## The Problem

A naive unit library stores conversion factors in a runtime dictionary and calls them on every operation:

```julia
struct UnitTypeAttributes
  toBase::Function   # e.g. x -> x * 0.001 for MilliMeter
  fromBase::Function # e.g. x -> x * 1000
  ...
end
const allUnitTypes = Dict{DataType, UnitTypeAttributes}()

# Naive conversion constructor
MilliMeter(x::AbstractLength) = MilliMeter( allUnitTypes[typeof(x)].toBase(x.value) )
```

Every call to `MilliMeter(Inch(1.0))` looks up `allUnitTypes[Inch]` at runtime. Julia types the `toBase` field as `Function` (abstract), so it cannot infer the return type of `toBase(x.value)`. The inference failure propagates: the result is typed `Any`, which forces heap boxing and triggers GC.

Benchmarking with BenchmarkTools reveals the cost: on the order of 80 bytes and 5 allocations per cross-unit construction, compared to zero for equivalent float arithmetic.

## The Core Insight: Capture Functions at Registration Time

The solution is to never call a function stored in a dictionary at operation time. Instead, when a type is registered, use `mod.eval(quote ... end)` to emit concrete methods that capture the conversion functions as closed-over constants. Julia's optimizer sees a call to a specific, known function type rather than an abstract `Function`, and inlines it to a register operation.

This is the same technique Julia uses internally for `@generated` functions and for `ccall` — bind the expensive lookup once, then dispatch is free.

## makeSelfConversion: scalar arithmetic

When `@makeBaseMeasure Length Meter "m"` or `@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"` registers a new type, `makeSelfConversion` immediately emits all the methods for operating on that type with scalars:

```julia
function makeSelfConversion(newType, mod=@__MODULE__)
  mod.eval(quote
    Base.:*(x::$newType, y::U) where U<:Number = $newType(x.value * y)
    Base.:/(x::$newType, y::U) where U<:Number = $newType(x.value / y)
    Base.:+(x::$newType, y::$newType)           = $newType(x.value + y.value)
    ...
  end)
end
```

These methods operate on `x.value` (a `Float64`) directly — no dictionary, no conversion, no boxing. `Meter(3.0) * 2 = Meter(6.0)` compiles to a float multiply and a struct store.

## makeJointConversions: cross-unit operations

When a new type `MilliMeter` is created, `makeJointConversions(MilliMeter, mod)` iterates every already-registered type with the same abstract supertype and emits one method per pair:

```julia
function makeJointConversions(newType, mod=@__MODULE__)
  for a in Dict(newType => allUnitTypes[newType])
    for b in allUnitTypes
      if supertype(a.first) == supertype(b.first)
        mod.eval(quote
          Base.convert(::Type{$(a.first)}, y::$(b.first)) =
            $(a.first)( $(a.second.fromBase)( $(b.second.toBase)(y.value)) )

          # specific constructor — more specific than the generic conversion constructor,
          # so Julia dispatches here first
          (::Type{$(a.first)})(y::$(b.first)) =
            $(a.first)( $(a.second.fromBase)( $(b.second.toBase)(y.value)) )

          Base.isapprox(x::$(a.first), y::$(b.first); ...) =
            isapprox(x.value, convert($(a.first), y).value, ...)
          ...
        end)
      end
    end
  end
```

The key is `$(a.second.fromBase)` and `$(b.second.toBase)`: these interpolate the actual function objects as literal values into the quote. The emitted method body captures them as constants of their concrete closure types (e.g. `var"#206#207"`, not `Function`). Julia can infer return types through them and inline the arithmetic.

The specific outer constructor `(::Type{MilliMeter})(y::Inch) = ...` is more specific than the generic conversion constructor `MilliMeter(x::T where T<:AbstractLength)`, so Julia dispatches to it first for any registered type pair.

## addRelations and hasExactMethod: relational dispatch

`@relateMeasures Meter*Meter = Meter2` registers that multiplying two lengths produces an area. Inside `addRelations`, this emits:

```julia
Base.:*(x::AbstractLength, y::AbstractLength) = Meter2(convert(Meter, x).value * convert(Meter, y).value)
```

A critical guard: Julia's `hasmethod(Base.:*, (AbstractLength, AbstractLength))` returns `true` even before this method is defined, because a parametric catch-all `*(x::T, y::U) where {T<:AbstractMeasure, U<:AbstractMeasure}` in `UnitExpr.jl` already matches. If the guard fires, the specific abstract-type method is never registered, forcing all `Meter*Meter` calls through the slow catch-all path.

The fix is `hasExactMethod`, which compares method signatures exactly:

```julia
function hasExactMethod(f, types)
  target = Tuple{typeof(f), types...}
  return any(m -> m.sig == target, methods(f))
end
```

With this guard, `addRelations` correctly registers `*(AbstractLength, AbstractLength)`, enabling Julia to dispatch `Meter(1.0) * Meter(2.0)` to a type-stable method with a concrete `Meter2` return type — zero allocations.

## registerPower and makeJointConversions: integer exponentiation

`@relateMeasures Meter*Meter = Meter2` also calls `registerPower(AbstractLength, 2, Meter2, mod)`, which emits `Base.literal_pow` methods for all current concrete subtypes:

```julia
function registerPower(abstractType, n, baseResultType, mod)
  powerTypes[(abstractType, n)] = baseResultType
  fromBase = allUnitTypes[baseResultType].fromBase
  valN = Val{n}
  for (T, uta) in allUnitTypes
    supertype(T) == abstractType || continue
    toBase = uta.toBase
    mod.eval(quote
      Base.literal_pow(::typeof(^), x::$T, ::$valN) =
        $baseResultType($fromBase($toBase(x.value)^$n))
    end)
  end
end
```

Julia emits `Base.literal_pow(^, x, Val{3}())` for compile-time literal exponents (`x^3`), so this intercepts `Meter(2.0)^3` and routes it to a concrete, allocation-free method. Again, `$fromBase` and `$toBase` are captured at registration time — no runtime lookup.

The `makeJointConversions` function contains a matching block that runs `registerPower`-style logic for any new type defined after a power relation has been registered, so `@makeMeasure 1e-3 Meter = 1 MilliMeter "mm"` defined after `@relateMeasures Meter*Meter = Meter2` automatically gets `literal_pow(^, MilliMeter, Val{2})`.

For non-literal exponents (`x^n` where `n` is a variable), a general fallback method on `AbstractMeasure` uses `resolveOrExpr` and does allocate — this is acceptable since the exponent is not known at compile time.

## Const globals: closures must not capture mutable state

The final allocation source was an unexpected one. `Imperial.jl` defined:

```julia
mPerIn = 0.0254   # meters per inch
@makeMeasure mPerIn Meter = 1 Inch "in"
```

The conversion closure `x -> x * mPerIn / 1` captures the module-level variable `mPerIn`. Because `mPerIn` is not declared `const`, Julia types it as `Any`. The closure's return type is then `Any`, infecting `convert(MilliMeter, Inch)` with the same instability even though `fromBase_MilliMeter` is perfectly well-typed.

The fix is a single keyword:

```julia
const mPerIn  = 0.0254
const inPerFt = 12
const ftPerMi = 5280
```

With `const`, Julia knows the variable's type will never change and can propagate the concrete type `Float64` through the closure body.

## Result

After all four techniques — captured closures via `mod.eval`, specific per-pair constructors in `makeJointConversions`, `hasExactMethod` guards in `addRelations`, and `const` globals for conversion factors — every measured operation reaches zero:

| Operation | Bytes | Allocs |
|---|---|---|
| `(m + 3m - m/4) * m` | 0 | 0 |
| `Meter(2.0)^3` | 0 | 0 |
| `MilliMeter(Inch(1.0))` | 0 | 0 |
| `isapprox(Meter(1.0), MilliMeter(1000.0))` | 0 | 0 |

The pattern is consistent: do the expensive lookup once, at type-registration time, and bake the result into a concrete method. Julia's specialization and inlining then eliminate the overhead entirely at call time.
