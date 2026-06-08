# Changelog

All notable changes to alembic are documented here.
Format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

---

## [Unreleased]

---

## [0.1.0] — 2026-06-08

### Added

#### Core DSL

- **`defpatch!`** — define a named signal graph as a Clojure value.  Graphs
  are pure data: inspectable, composable, serialisable to EDN.
- **`alembic.emit/emit-faust`** — compile a graph value to a Faust `.dsp`
  source string without invoking the compiler.
- **`alembic.patch`** — graph construction primitives: `param`, `output`,
  `add`, `mul`, `sine`, `sine-bi`, `phasor`, and arithmetic combinators.
- **Compile-time options extension** — `:params` map on `defpatch!` declares
  named parameters with `:range` and `:default` metadata; emitted as
  `hslider` UI declarations in Faust.
- **Variable-arity inlets** — ops can declare `N` named inlets; `defpatch!`
  validates connectivity at definition time.
- **`:select` multiplexer** — N-way signal selector driven by an integer
  index input.
- **Multi-output nodes** — `port-map` return, `:counter-carry`,
  `:comparator-inv` for graphs that produce more than one output signal.

#### Level 1 primitives

- **Filters** — `:ladder` (Moog ladder, `ve.moogLadder`), `:svf` (ZDF
  state-variable, `fi.svf_morph`), `naive-svf`, `crossover`.
- **Delay** — `:buffer` (read-write circular buffer via `rwtable`),
  `:delay` (opts-aware delay line).
- **Waveshapers** — `:segment` (morphable slope), `hysteresis`, `damping`.
- **Signal ops** — `:abs`, `:min`, `:max`, `:track-hold`, `:sqrt`.
- **Rate-polymorphic ops** — ops that work at both audio and control rate.
- **`:audio-in`** — audio input op; bridges host signal into the graph.
- **Beat-domain ops** — `beat-phase`, `beat-bpm`, `beat-trigger`.
- **`:shelf-lo`, `:shelf-hi`, `:peak-eq`** — shelving/peaking EQ ops.

#### Inline Faust authoring

- **`(faust ...)` form** — embed raw Faust DSP source directly inside a
  `defpatch!` body.  `%inlet-name` substitution splices named inlets.
  Provides an escape hatch for anything not yet in the op vocabulary.

#### Compilation backends

- **`alembic.compile/compile-to-wasm`** — emit Faust source and invoke
  `faust -lang wasm`; returns absolute path to `.wasm` + companion `.json`
  (parameter metadata: labels and WASM memory addresses).
- **`alembic.compile/compile-to-cpp`** — emit Faust source and invoke
  `faust -lang cpp`; returns the C++ source string.
- **`alembic.compile/compile-to-clap`** — full `defpatch!` → `.clap` bundle
  pipeline: `faust -a clap-arch.cpp`, cmake reconfigure, cmake build.
  Supports `:polyphonic true` / `:nvoices N` for voice-architecture builds.
- **`alembic.compile/validate`** — compile to C++ and discard output; fast
  DSP correctness check without producing artefacts.
- **`alembic.compile/check-faust!`** — verify `faust` is on PATH and meets
  the minimum version requirement (2.50).
- **`cpp/` build scaffold** — `AlembicPatch.cmake` macro discovers and
  compiles staged patches; configures macOS bundles and Linux `.clap` output.

#### Visualisation

- **`alembic.viz`** — DOT renderer, HTML viewer, SVG export via Graphviz.
  `start!` launches a browser-based live graph view during REPL sessions.

#### Examples

- `ks_string` — Karplus-Strong string resonator (Mutable Rings KS-mode)
- `shelves` — 4-band shelving EQ
- `ripples` — FM-capable dual SVF filter (Mutable Ripples)
- `ears` — envelope follower / transient detector
- `kinks` — waveshaper bank (`:abs`, `:min`, `:max`, `:track-hold`)
- `blinds` — dual-rate VCA / crossfader
- `blades` — Blade Runner-style evolving pad

#### Dependencies

- `nomos-maths 0.1.0` — shared maths primitives.
- `nomos-topology 0.1.0` — synthesis topology schema constants.

### Fixed

- `emit-faust`: dead secondary port nodes pruned from the Faust process
  expression to avoid unused-signal warnings in the Faust compiler.
- `emit-faust`: scientific notation in float literals (`1e-3`) rewritten
  to decimal form (`0.001`) for Faust 2.x compatibility.
- `lein clean` failure caused by `:target-path "target/%s"` format string;
  corrected to `"target"` with `:clean-targets ^{:protect false} ["target"]`.

### Changed

- Migrated from `cljseq` to `nomos-studio` organisation; dependency updated
  from `cljseq-maths` to `nomos-maths`.
