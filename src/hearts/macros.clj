(ns hearts.macros)

(defmacro spy2
  [& body]
  `(let [x# ~@body]
     (goog.string/format "=> %s = %s\n" (first '~body) x#)
     x#))

