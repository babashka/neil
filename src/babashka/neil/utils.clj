(ns babashka.neil.utils)

;; Workaround for pmap + require which doesn't work well in bb - 2023-02-04

(def ^:private lock (Object.))

(defn- serialized-require
  [& args]
  (locking lock
    (apply require args)))

(defn req-resolve
  [sym]
  (if (qualified-symbol? sym)
    (or (resolve sym)
        (do (-> sym namespace symbol serialized-require)
            (resolve sym)))
    (throw (IllegalArgumentException. (str "Not a qualified symbol: " sym)))))

;; End workaround
