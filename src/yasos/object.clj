(ns yasos.object
  (:require [clojure.pprint :refer :all]))

(defn- define-object [define-method]
  (let [methods (atom {})]
    (define-method (fn [op meth] (swap! methods assoc op meth)))
    (fn self [op args]
      (let [meth (get @methods op)]
        (if (nil? meth)
          (throw (NoSuchMethodException. (str op)))
          (apply meth args))))))

(defmacro operator [name]
  `(defn ~name [~'object & ~'args]
     (~'object ~name ~'args)))

(defmacro object [& body]
  (let [forms (map
                (fn [form] (list 'method (first form) (second form)))
                body)]
    `(define-object (fn [~'method] ~@forms))))





(operator ttt)
(operator zzz)
(def obj (object
           (ttt (fn
                  ([a b]
                   (println a b "."))
                  ([a b c]
                   (println a b c))))
           (zzz (fn [a b]
                  (println "->" a b)))))
(defn cl [prefix]
  (object
    (ttt (fn [x] (println prefix x)))))

(defn -main [& args]
  (println zzz)
  (ttt (cl "--->") "ZZZ")
  (ttt obj 1 2)
  (ttt obj 1 2 3)
  (zzz obj "A" "B")
  )

