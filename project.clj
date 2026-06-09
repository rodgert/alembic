; SPDX-License-Identifier: EPL-2.0
(defproject alembic "0.1.0"
  :description "alembic — homoiconic Clojure DSP DSL for the nomos-studio ecosystem"
  :url "https://github.com/nomos-studio/alembic"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [nomos-maths "0.2.1"]
                 [nomos-topology "0.1.0"]]
  :source-paths  ["src"]
  :test-paths    ["test"]
  :target-path   "target"
  :clean-targets ^{:protect false} ["target"]
  :profiles {:dev  {:source-paths ["."]
                    :dependencies []}})
