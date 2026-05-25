; SPDX-License-Identifier: EPL-2.0
(ns alembic.viz
  "Graphviz DOT renderer and browser viewer for alembic patch graphs.

  Pure API:
    (dot graph)            → Graphviz DOT source string
    (html graph)           → self-contained HTML string (viz.js CDN, no install needed)
    (save-dot! graph path) → write DOT to path

  Side-effecting API:
    (show! graph)          → write HTML to temp file and open in system browser
                             Returns the path of the generated HTML file."
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh])
  (:import [java.awt Desktop]
           [java.io File]))

;; ---------------------------------------------------------------------------
;; DOT helpers
;; ---------------------------------------------------------------------------

(defn- esc-dot
  "Escape a string for use inside a Graphviz double-quoted label."
  [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn- rate-fill
  "Background colour for a node, keyed by rate."
  [rate]
  (case rate
    :beat   "#b3d9ff"
    :block  "#fff5b1"
    :sample "#ffffff"
    "#f0f0f0"))

(defn- node-label [node]
  (let [op-str (name (:op node))
        extra  (cond
                 (= (:op node) :const) (str "\n" (:value node))
                 (= (:op node) :param) (str "\n" (name (:name node)))
                 :else "")]
    (str op-str extra "\n[" (name (:rate node)) "]")))

(defn- node-shape [node]
  (case (:op node)
    (:param :beat-phase :beat-bpm) "parallelogram"
    "box"))

(defn- node->dot [[_key node]]
  (let [id     (name (:id node))
        label  (esc-dot (node-label node))
        shape  (node-shape node)
        fill   (rate-fill (:rate node))
        faust? (= (:op node) :faust)]
    (str "  " id
         " [label=\"" label "\""
         " shape=" shape
         (when faust? " peripheries=2")
         " style=filled"
         " fillcolor=\"" fill "\""
         "];\n")))

(defn- edge->dot [{:keys [from to inlet feedback? rate-crossing?]}]
  (let [label (esc-dot (str inlet))
        attrs (cond-> [(str "label=\"" label "\"")]
                rate-crossing? (conj "color=red" "style=dashed")
                feedback?      (conj "color=blue" "style=dotted" "constraint=false"))]
    (str "  " (name from) " -> " (name to)
         " [" (str/join " " attrs) "];\n")))

(defn- output->dot [{:keys [node channel name]}]
  (let [sink-id (str "_out_" channel)
        label   (esc-dot (str "out:" channel "\n" name))]
    [(str "  " sink-id
          " [label=\"" label "\""
          " shape=octagon style=filled fillcolor=\"#c8f0c8\"];\n")
     (str "  " (clojure.core/name node) " -> " sink-id " [style=bold];\n")]))

(defn- sorted-nodes
  "Return nodes map entries in ascending node-index order for deterministic output."
  [nodes]
  (sort-by #(try (Integer/parseInt (subs (name (key %)) 1))
                 (catch Exception _ Integer/MAX_VALUE))
           nodes))

;; ---------------------------------------------------------------------------
;; Public — pure functions
;; ---------------------------------------------------------------------------

(defn dot
  "Render a normalised alembic patch graph as a Graphviz DOT string.

  Node colours:  beat=#b3d9ff  block=#fff5b1  sample=white
  Edge styles:   normal=black  rate-crossing=dashed/red  feedback=dotted/blue
  Output sinks:  octagon in pale green

  The returned string can be passed to `dot -Tsvg` or any Graphviz viewer."
  [{:keys [nodes edges outputs]}]
  (str "digraph patch {\n"
       "  graph [rankdir=LR];\n"
       "  node [fontname=\"Helvetica\" fontsize=10 margin=\"0.1,0.05\"];\n"
       "  edge [fontname=\"Helvetica\" fontsize=8];\n"
       "\n"
       (str/join (map node->dot (sorted-nodes nodes)))
       "\n"
       (str/join (map edge->dot edges))
       "\n"
       (str/join (mapcat output->dot outputs))
       "}\n"))

(defn html
  "Return a self-contained HTML string that renders the graph in a browser.

  Uses the viz.js library (Graphviz compiled to WebAssembly) served from
  unpkg.com — requires internet access on first load; cached thereafter.
  No local Graphviz installation needed."
  [graph]
  (let [dot-src  (dot graph)
        js-dot   (-> dot-src
                     (str/replace "\\" "\\\\")
                     (str/replace "`" "\\`"))]
    (str "<!DOCTYPE html>\n"
         "<html>\n"
         "<head>\n"
         "<meta charset=\"utf-8\">\n"
         "<title>Alembic Patch</title>\n"
         "<style>\n"
         "body { margin: 0; font-family: Helvetica, sans-serif; background: #f5f5f5; }\n"
         "#graph { padding: 20px; }\n"
         "#graph svg { max-width: 100%; height: auto; }\n"
         "#error { color: red; padding: 20px; }\n"
         "</style>\n"
         "</head>\n"
         "<body>\n"
         "<div id=\"graph\"></div>\n"
         "<div id=\"error\"></div>\n"
         "<script src=\"https://unpkg.com/@viz-js/viz@3.4.0/lib/viz-standalone.js\"></script>\n"
         "<script>\n"
         "Viz.instance().then(function(viz) {\n"
         "  try {\n"
         "    var dot = `" js-dot "`;\n"
         "    var el = viz.renderSVGElement(dot);\n"
         "    el.style.maxWidth = '100%';\n"
         "    document.getElementById('graph').appendChild(el);\n"
         "  } catch(e) {\n"
         "    document.getElementById('error').textContent = 'Render error: ' + e.message;\n"
         "  }\n"
         "}).catch(function(e) {\n"
         "  document.getElementById('error').textContent = 'Failed to load viz.js: ' + e.message;\n"
         "});\n"
         "</script>\n"
         "</body>\n"
         "</html>\n")))

(defn save-dot!
  "Write the DOT representation of graph to path."
  [graph path]
  (spit path (dot graph))
  path)

(defn render-svg
  "Render graph to an SVG string via the Graphviz `dot` command.
  Requires `dot` on PATH (brew install graphviz).
  Useful for docs, CI artefacts, and MCP image payloads.
  Throws ex-info if `dot` is not found or returns a non-zero exit code."
  [graph]
  (let [{:keys [out err exit]} (sh/sh "dot" "-Tsvg" :in (dot graph))]
    (when-not (zero? exit)
      (throw (ex-info "Graphviz `dot` failed — is it on PATH? (brew install graphviz)"
                      {:stderr err :exit exit})))
    out))

(defn save-svg!
  "Render graph to SVG via Graphviz and write to path. Returns path."
  [graph path]
  (spit path (render-svg graph))
  path)

;; ---------------------------------------------------------------------------
;; Public — side effects
;; ---------------------------------------------------------------------------

(defn show!
  "Render graph to a self-contained HTML file and open it in the system browser.
  Returns the path of the generated HTML file.

  The HTML file uses viz.js (WebAssembly) to render the graph client-side —
  no Graphviz installation required, but internet access is needed on first
  load to fetch viz.js from the unpkg CDN (cached by the browser thereafter)."
  [graph]
  (let [html-str (html graph)
        tmp      (File/createTempFile "alembic-" ".html")]
    (.deleteOnExit tmp)
    (spit tmp html-str)
    (when (Desktop/isDesktopSupported)
      (.browse (Desktop/getDesktop) (.toURI tmp)))
    (.getAbsolutePath tmp)))
