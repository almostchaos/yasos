(ns yasos.object)

(defmacro operator [name]
  `(defn ~name [~'object & ~'args]
     (~'object ~name ~'args)))

(defmacro object [& body]
  `(let [~'methods (into {} [~@body])]
     (fn [~'op ~'args]
       (let [~'meth (get ~'methods ~'op)]
         (if (nil? ~'meth)
           (throw (NoSuchMethodException. (str ~'op)))
           (apply ~'meth ~'args))))))

(defmacro method [op & body]
  (if (vector? (first body))
    `[~op (fn ~(first body) ~@(rest body))]
    `[~op (fn ~@body)]))