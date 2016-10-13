(ns hearts.utils)

(defn error? [x]
  (instance? js/Error x))

(defn throw-err [x]
  (if (error? x)
    (throw x)
    x))

(defn spy [& args]
  (println (apply pr-str args))
  (last args))

