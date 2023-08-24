(ns yasos.object
  (:require [clojure.pprint :refer :all]
            [clojure.string :refer [upper-case]]))

(defn- define-object [define-method]
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

(defmacro method [op args & body]
  `(~'add-method ~op (fn ~args ~@body)))

(operator ttt)
(operator zzz)
(def obj (object
           (method ttt [a b c]
             (println a b c)
             (apply println (map upper-case [a b c])))
           (method zzz [a b]
             (println "->" a b))))

(operator running?)
(operator start)
(operator stop)
(defn test-server []
  (let [running (atom false)]
   (object
     (method running? [] @running)
     (method start [] (reset! running true))
     (method stop [] (reset! running false)))))

(defn -main [& args]
  (let [server (test-server)]
    (println "running? " (running? server))
    (println "starting...")
    (start server)
    (println "running? " (running? server))
    (println "stopping...")
    (stop server)
    (println "running? " (running? server)))

  (ttt obj "a" 2 3)
  (zzz obj "A" "B"))

