(ns yasos.internal-state
  (:require [clojure.test :refer :all]
            [yasos.object :refer :all]))

(operator running?)
(operator start)
(operator stop)

(deftest internal-state
  (let [server (fn test-server []
                 (let [running (atom false)]
                   (object
                     (method running? [] @running)
                     (method start [] (reset! running true))
                     (method stop [] (reset! running false)))))
        server-instance (server)]

    (testing "initial state"
      (is (not (running? server-instance))))
    (testing "running state"
      (start server-instance)
      (is (running? server-instance)))
    (testing "stopped state"
      (stop server-instance)
      (is (not (running? server-instance))))))