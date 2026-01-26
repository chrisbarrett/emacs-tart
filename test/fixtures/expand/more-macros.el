;; More macros for testing multiple --load files

(defmacro double (x)
  `(+ ,x ,x))
