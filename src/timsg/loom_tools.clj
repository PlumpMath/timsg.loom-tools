(ns timsg.loom-tools
  (:require [timsg.ports.loom.graph :as lg]
            [timsg.ports.loom.attr :as la]
            [timsg.ports.loom.io :as lio]
            [clojure.clr.shell :as sh]
            [clojure.pprint :as pprint])
  (:import [System.IO FileInfo FileSystemInfo]))

(defn- file-info [x]
  (cond
    (instance? FileInfo x) x
    (string? x) (try
                  (FileInfo. x)
                  (catch System.ArgumentException e))
    :else (throw (System.ArgumentException. "Expects FileInfo or String."))))

(defn- without-extension [path]
  (let [fi (file-info path)
        path' (.FullName fi)]
    (subs path' 0
      (- (count path')
         (count (.Extension fi))))))

(defn- make-temp-dot [path data]
  (let [fi (file-info
             (str (without-extension path) ".dot"))]
    (when (.Exists fi) (.Delete fi))
    (spit (.FullName fi) data)
    (.FullName fi)))

(defn- assoc-if-missing
  ([m k v]
   (if-not (contains? m k)
     (assoc m k v)
     m))
  ([m k v & vs]
   (reduce assoc-if-missing (assoc-if-missing m k v) vs)))

;; pass to show as :edge-label (attr-labeler g)
;; (defn attr-labeler [g opts]
;;   (fn [n1 n2]
;;     (fn [n1 n2]
;;       (or (la/attr g n1 n2 :label)
;;           (if-let [a (la/attrs g n1 n2)]
;;             (with-out-str (pprint/pprint a)))))))

(defn show
  ([g] (show g nil))
  ([g {:keys [alg path]
       :or {alg "/usr/local/bin/dot"
            path "Temp/dotxamp.png"} ;; this is bullshit
       :as opts}]
   (let [path' (.FullName (file-info path))
         _ (println "opts: " opts)
         dotstr (apply lio/dot-str g (apply concat opts))
         temp-dot (make-temp-dot path' dotstr) 
         attempt (sh/sh alg  "-Tpng" temp-dot "-o" path')]
     (when (not= "" (:err attempt))
       (throw (Exception.
                (str "Some problem:\n"
                     (with-out-str
                       (clojure.pprint/pprint attempt))))))
     (sh/sh "open" path')
     g)))
