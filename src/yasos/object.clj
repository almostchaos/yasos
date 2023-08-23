(ns yasos.object
  (:require [clojure.pprint :refer :all]))

(defn operator []
  (fn self [object & args]
    (object self args)))

(defn- define-object [define-method]
  (let [methods (atom {})]
    (define-method (fn [op meth] (swap! methods assoc op meth)))
    (fn self [op args]
      (let [meth (get @methods op)]
        (if (nil? meth)
          (throw (NoSuchMethodException. (str op)))
          (apply meth args))))))

(defmacro object [& body]
  (define-object (fn [method]
                   (run!
                     (fn [form] (method (eval (first form)) (eval (second form))))
                     body))))





(def ttt (operator))
(def zzz (operator))
(def obj (object
           (ttt (fn
                  ([a b]
                   (println a b "."))
                  ([a b c]
                   (println a b c))))
           (zzz (fn [a b]
                  (println "->" a b)))))
(defn -main [& args]
  (ttt obj 1 2)
  (ttt obj 1 2 3)
  (zzz obj "A" "B"))

