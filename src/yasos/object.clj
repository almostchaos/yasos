(ns yasos.object)

(defn operator []
  (fn self [object & args]
    (object self args)))

(defn object [method]
  (let [methods (atom {})]
    (method (fn [op meth]
              (swap! methods assoc op meth)))
    (fn [op args]
      (let [meth (get @methods op)]
        (if (nil? meth)
          (throw (NoSuchMethodException.))
          (apply meth args))))))

(def ttt (operator))
(def obj (object
           (fn [method]
             (method ttt (fn [a b c]
                           (println a b c))))))
(defn -main [& args]
  (ttt obj 1 2 3))

