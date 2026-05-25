; SPDX-License-Identifier: EPL-2.0
(ns alembic.compile
  "Faust toolchain integration for alembic patch graphs.

  Requires `faust` >= 2.50 on PATH (brew install faust).
  Native CLAP compilation also requires cmake >= 3.20 and a C++17 compiler.

  (check-faust!)                     — verify faust installation
  (validate graph)                   — confirm DSP source compiles
  (compile-to-cpp graph)             — Faust C++ source string (generic)
  (compile-to-wasm graph & opts)     — compile to .wasm + companion .json
  (compile-to-clap graph & opts)     — compile to native .clap bundle
  (faust-source graph)               — emitted .dsp source (no compiler)

  compile-to-wasm options:
    :name        base filename for .wasm/.json output (default: \"alembic-patch\")
    :out-dir     output directory (default: system temp dir)

  compile-to-clap options:
    :name        plugin name used for the cmake target and bundle (default: \"alembic-patch\")
    :vendor      plugin vendor string (default: \"alembic\")
    :version     plugin version string (default: \"1.0.0\")
    :polyphonic  true for polyphonic voice architecture (default: false)
    :nvoices     voice count when polyphonic (default: 16)
    :cpp-dir     path to alembic cpp/ directory; auto-detected if not supplied
                 (also checked: ALEMBIC_CPP_DIR env var, alembic.cpp-dir system property)"
  (:require [alembic.emit :refer [emit-faust]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; Version check
;; ---------------------------------------------------------------------------

(def ^:private minimum-version [2 50 0])

(defn- parse-version [s]
  (when-let [[_ v] (re-find #"FAUST Version (\S+)" s)]
    (mapv #(try (Integer/parseInt %) (catch Exception _ 0))
          (str/split v #"\."))))

(defn- version>= [[a b c & _] [x y z & _]]
  (or (> (or a 0) x)
      (and (= (or a 0) x)
           (or (> (or b 0) y)
               (and (= (or b 0) y)
                    (>= (or c 0) z))))))

(defn check-faust!
  "Verify faust is on PATH and meets the minimum version requirement.
  Throws ex-info on failure; returns nil on success."
  []
  (let [{:keys [exit out err]} (sh "faust" "--version")]
    (when (not= 0 exit)
      (throw (ex-info "faust not found — install via: brew install faust"
                      {:stderr err})))
    (if-let [v (parse-version out)]
      (when-not (version>= v minimum-version)
        (throw (ex-info (format "faust %s is below minimum %s — upgrade via: brew upgrade faust"
                                (str/join "." v)
                                (str/join "." minimum-version))
                        {:version v :minimum minimum-version})))
      (throw (ex-info (str "Could not parse faust version from output: " out) {}))))
  nil)

;; ---------------------------------------------------------------------------
;; Temp-file helpers
;; ---------------------------------------------------------------------------

(defn- make-temp [suffix]
  (doto (File/createTempFile "alembic-" suffix)
    (.deleteOnExit)))

(defmacro ^:private with-dsp-file
  "Bind `sym` to a temp .dsp File written with `src`, then execute body.
  File is deleted on exit regardless of outcome."
  [[sym src] & body]
  `(let [f# (make-temp ".dsp")]
     (try
       (spit f# ~src)
       (let [~sym f#] ~@body)
       (finally (.delete f#)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn faust-source
  "Return the Faust .dsp source string for `graph` without invoking the compiler."
  [graph]
  (emit-faust graph))

(defn validate
  "Confirm that `graph` produces valid Faust DSP.
  Runs `faust -lang cpp` on the emitted source and discards the output.
  Returns nil on success; throws ex-info with :errors and :source on failure."
  [graph]
  (let [src    (emit-faust graph)
        out-f  (make-temp ".cpp")]
    (try
      (with-dsp-file [in-f src]
        (let [{:keys [exit err]} (sh "faust" "-lang" "cpp"
                                     (.getAbsolutePath in-f)
                                     "-o" (.getAbsolutePath out-f))]
          (when (not= 0 exit)
            (throw (ex-info "Faust compilation failed"
                            {:errors err :source src})))))
      (finally (.delete out-f))))
  nil)

(defn compile-to-cpp
  "Compile `graph` to a C++ source string via `faust -lang cpp`.
  Returns the C++ source on success; throws ex-info with :errors and :source on failure."
  [graph]
  (let [src   (emit-faust graph)
        out-f (make-temp ".cpp")]
    (try
      (with-dsp-file [in-f src]
        (let [{:keys [exit err]} (sh "faust" "-lang" "cpp"
                                     (.getAbsolutePath in-f)
                                     "-o" (.getAbsolutePath out-f))]
          (if (= 0 exit)
            (slurp out-f)
            (throw (ex-info "Faust compilation failed"
                            {:errors err :source src})))))
      (finally (.delete out-f)))))

;; ---------------------------------------------------------------------------
;; WASM compilation
;; ---------------------------------------------------------------------------

(defn compile-to-wasm
  "Compile `graph` to a Faust WASM module (.wasm + companion .json metadata).

  Faust automatically generates a <name>.json alongside the .wasm; both share
  the same directory and stem. The JSON encodes parameter metadata consumed by
  the kairos WASM bridge (CLAP_EXT_PARAMS, setParamValue addresses).

  Returns the absolute path to the .wasm file on success.
  Throws ex-info with :errors and :source on compilation failure."
  [graph & {:keys [name out-dir]
            :or   {name "alembic-patch"}}]
  (let [src    (emit-faust graph)
        stem   (str/replace name #"[^a-zA-Z0-9\-_]" "-")
        dir    (or out-dir (System/getProperty "java.io.tmpdir"))
        wasm-f (io/file dir (str stem ".wasm"))]
    (with-dsp-file [in-f src]
      (let [{:keys [exit err]} (sh "faust" "-lang" "wasm"
                                   (.getAbsolutePath in-f)
                                   "-o" (.getAbsolutePath wasm-f))]
        (when (not= 0 exit)
          (throw (ex-info "Faust WASM compilation failed"
                          {:errors err :source src})))
        (.getAbsolutePath wasm-f)))))

;; ---------------------------------------------------------------------------
;; Native CLAP compilation
;; ---------------------------------------------------------------------------

(defn- find-cpp-dir
  "Locate the alembic cpp/ directory. Checks in order:
  1. ALEMBIC_CPP_DIR environment variable
  2. alembic.cpp-dir system property
  3. cpp/ relative to the JVM working directory (works in the source checkout)"
  []
  (or (System/getenv "ALEMBIC_CPP_DIR")
      (System/getProperty "alembic.cpp-dir")
      (let [candidate (io/file (System/getProperty "user.dir") "cpp")]
        (when (.exists (io/file candidate "CMakeLists.txt"))
          (.getAbsolutePath candidate)))))

(defn- safe-cmake-name
  "Sanitise a plugin name to a valid cmake target name (alphanumeric, hyphens, underscores)."
  [s]
  (str/replace s #"[^a-zA-Z0-9\-_]" "-"))

(defn- write-metadata-header!
  "Write plugin_metadata.h into `dir` for the CLAP arch file."
  [dir name vendor version polyphonic nvoices]
  (let [id (str "org.cljseq.alembic." (str/lower-case name))]
    (spit (io/file dir "plugin_metadata.h")
          (str "#define FAUST_PLUGIN_ID          \"" id "\"\n"
               "#define FAUST_PLUGIN_NAME        \"" name "\"\n"
               "#define FAUST_PLUGIN_VENDOR      \"" vendor "\"\n"
               "#define FAUST_PLUGIN_VERSION     \"" version "\"\n"
               "#define FAUST_PLUGIN_DESCRIPTION \"Compiled by alembic\"\n"
               "#define FAUST_NVOICES            " nvoices "\n"
               "#define FAUST_IS_POLYPHONIC      " (if polyphonic "1" "0") "\n"))))

(defn- faust-arch-dir []
  (str/trimr (:out (sh "faust" "--archdir"))))

(defn- sh-or-throw [err-msg & cmd]
  (let [{:keys [exit out err]} (apply sh cmd)]
    (when (not= 0 exit)
      (throw (ex-info err-msg {:stdout out :stderr err :cmd cmd})))
    {:out out :err err}))

(defn compile-to-clap
  "Compile `graph` to a native CLAP plugin bundle (.clap).

  Stages the generated C++ under cpp/patches/<name>/, reconfigures the
  alembic cmake project to pick up the new patch, then builds it.
  Returns the absolute path to the .clap bundle on success.

  Requires cmake >= 3.20 and a C++17 compiler in addition to faust.
  The cpp/ build directory must have been configured at least once
  (cmake -S cpp -B cpp/build) before calling this function."
  [graph & {:keys [name vendor version polyphonic nvoices cpp-dir]
            :or   {name "alembic-patch" vendor "alembic" version "1.0.0"
                   polyphonic false nvoices 16}}]
  (let [cpp-dir   (or cpp-dir (find-cpp-dir))
        _         (when-not cpp-dir
                    (throw (ex-info
                             (str "Cannot find alembic cpp/ directory.\n"
                                  "Set ALEMBIC_CPP_DIR or pass :cpp-dir option.")
                             {})))
        tgt       (safe-cmake-name name)
        patch-dir (io/file cpp-dir "patches" tgt)
        build-dir (io/file cpp-dir "build")
        arch-dir  (faust-arch-dir)
        dsp-src   (emit-faust graph)]

    (.mkdirs patch-dir)

    ;; Write plugin_metadata.h
    (write-metadata-header! patch-dir tgt vendor version polyphonic nvoices)

    ;; faust -a clap-arch.cpp → CLAP C++ source
    (with-dsp-file [dsp-f dsp-src]
      (let [cpp-out (io/file patch-dir (str tgt "_clap.cpp"))
            {:keys [exit err]} (sh "faust"
                                   "-a" (str arch-dir "/clap/clap-arch.cpp")
                                   (.getAbsolutePath dsp-f)
                                   "-o" (.getAbsolutePath cpp-out))]
        (when (not= 0 exit)
          (throw (ex-info "faust CLAP arch step failed"
                          {:errors err :source dsp-src})))))

    ;; cmake reconfigure — picks up the newly staged patch directory
    (sh-or-throw "cmake configure failed"
                 "cmake" "-S" cpp-dir "-B" (.getAbsolutePath build-dir))

    ;; cmake build — only the target for this patch
    (sh-or-throw (str "cmake build failed for target: " tgt)
                 "cmake" "--build" (.getAbsolutePath build-dir) "--target" tgt)

    ;; Return path to the .clap bundle
    (let [clap (io/file build-dir "plugins" (str tgt ".clap"))]
      (if (.exists clap)
        (.getAbsolutePath clap)
        (throw (ex-info "Build succeeded but .clap not found"
                        {:expected (.getAbsolutePath clap)}))))))
