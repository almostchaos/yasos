(ns yasos.object)

(defn define-object [define-method]
  (let [methods (atom {})]
    (define-method (fn [op meth]
                     (swap! methods assoc op meth)))
    (fn self [op args]
      (let [meth (get @methods op)]
        (if (nil? meth)
          (throw (NoSuchMethodException. (str op)))
          (apply meth args))))))

(defmacro operator [name]
  `(defn ~name [~'object & ~'args]
     (~'object ~name ~'args)))

(defmacro object [& body]
  `(define-object (fn [~'add-method] ~@body)))

(defmacro method [op & body]
  (if (vector? (first body))
    `(~'add-method ~op (fn ~(first body) ~@(rest body)))
    `(~'add-method ~op (fn ~@body))))