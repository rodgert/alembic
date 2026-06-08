# alembic

Homoiconic Clojure DSL for authoring DSP signal graphs that compile to
Faust WASM modules, native CLAP plugins, or raw C++.  Part of the
[nomos-studio](https://github.com/nomos-studio) open-source music platform.

```clojure
(require '[alembic.patch :refer [defpatch!]]
         '[alembic.compile :as ac])

;; Define a stereo FM oscillator with two named parameters
(defpatch! fm-synth
  {:params {:carrier {:range [20.0 20000.0] :default 440.0}
            :depth   {:range [0.0 1.0]      :default 0.5}}}
  (let [mod (phasor 880.0)
        sig (sine-bi (add (phasor (param :carrier))
                          (mul (sine-bi mod) (param :depth))))]
    (output sig)))

;; Compile to Faust WASM — returns absolute path to .wasm + companion .json
(ac/compile-to-wasm fm-synth :name "fm-synth")

;; Compile to a native CLAP plugin bundle
(ac/compile-to-clap fm-synth :name "fm-synth" :vendor "myname")
```

## Features

- **Homoiconic graph DSL** — patches are plain Clojure data; inspectable,
  transformable, and serialisable to EDN.
- **Multiple backends** — `compile-to-wasm` for kairos-grid live loading;
  `compile-to-clap` for native DAW integration; `compile-to-cpp` for embedding.
- **Faust as library** — alembic emits `.dsp` source and delegates JIT compilation
  to `faust`; the DSL is a thin but expressive layer over Faust's signal graph.
- **Live reload** — WASM output integrates directly with kairos-grid's hot-swap
  extension for gapless patch iteration from a Clojure REPL.
- **Visual debug** — `alembic.viz/start!` renders the signal graph in a browser.

## Prerequisites

- **Java** 17+ and **Clojure** 1.12
- **Faust** ≥ 2.50 — `brew install faust` (macOS) or from [faust.grame.fr](https://faust.grame.fr)
- **cmake** ≥ 3.20 and a C++17 compiler (for `compile-to-clap` only)

## Quick start

```bash
# Clone and start a REPL
git clone https://github.com/nomos-studio/alembic
cd alembic
lein repl
```

```clojure
;; Verify Faust is installed
(require '[alembic.compile :refer [check-faust!]])
(check-faust!)  ;=> nil on success

;; One-shot WASM compilation
(require '[alembic.compile :as ac]
         '[alembic.patch :refer [defpatch!]])

(defpatch! osc {}
  (output (phasor 440.0)))

(ac/compile-to-wasm osc :name "osc")
;=> "/tmp/osc.wasm"
```

## Compile-to-CLAP setup

The native CLAP pipeline requires a one-time cmake configuration of the
`cpp/` build directory:

```bash
cmake -S cpp -B cpp/build
```

Subsequent calls to `compile-to-clap` reconfigure and rebuild incrementally.

## Running tests

```bash
lein test
```

The CLAP integration tests (`compile-to-clap-*`) are skipped automatically
when `cpp/build/CMakeCache.txt` is absent.  Run the cmake setup above to
enable them.

## Examples

The `examples/` directory contains ready-to-run patches:

| File | Description |
|---|---|
| `ks_string.clj` | Karplus-Strong string synthesis |
| `shelves.clj` | 4-band shelving EQ |
| `ripples.clj` | Mutable Instruments Ripples-inspired filter |
| `blades.clj` | Blade Runner-style pad |
| `ears.clj` | Envelope follower / transient detector |
| `kinks.clj` | Waveshaper bank |
| `blinds.clj` | Quad VCA / crossfader |

## Architecture

```
defpatch!  →  alembic.patch  (graph value — pure Clojure data)
                    ↓
           alembic.emit      (emit-faust → .dsp source string)
                    ↓
           faust compiler    (-lang wasm / -lang cpp / -a clap-arch.cpp)
                    ↓
           .wasm + .json     → kairos-grid WasmGridModule (live reload)
           native .clap      → any CLAP host (Bitwig, REAPER, kairos)
```

## License

EPL-2.0 — see [LICENSE](LICENSE).
