(ns lab3.core-test
  (:require [clojure.test :refer :all]
            [lab3.core :refer :all]))

(deftest my-partition-test
  (testing "my-partition"
    (is (lab3.task1/my-partition 1 (range 10)) (partition 1 (range 10)))
    (is (lab3.task1/my-partition 2 (range 10)) (partition 2 (range 10)))
    (is (lab3.task1/my-partition 3 (range 10)) (partition 3 (range 10)))
    (is (lab3.task1/my-partition 4 (range 10)) (partition 4 (range 10)))
    (is (lab3.task1/my-partition 5 (range 10)) (partition 5 (range 10)))
    (is (lab3.task1/my-partition 6 (range 10)) (partition 6 (range 10)))
    (is (lab3.task1/my-partition 7 (range 10)) (partition 7 (range 10)))
    (is (lab3.task1/my-partition 8 (range 10)) (partition 8 (range 10)))
    (is (lab3.task1/my-partition 9 (range 10)) (partition 9 (range 10)))
    (is (lab3.task1/my-partition 10 (range 10)) (partition 10 (range 10)))))

    
(deftest task1-test
  (testing "task1"
    (is (lab3.task1/task1 even? 1 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 2 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 4 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 5 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 6 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 7 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 8 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 9 (range 100)) (filter even? (range 100)))
    (is (lab3.task1/task1 even? 10 (range 100)) (filter even? (range 100)))))

(deftest task1-time-test
  (testing "task1-time"
    (println)
    (println "Test filter time wint (range 10000)")
    (time (doall (filter even? (range 10000))))
    (println)
    (println "Test task1 time wint (range 10000) from 1 to 10 threads")
    (time (doall (lab3.task1/task1 even? 1 (range 10000))))
    (time (doall (lab3.task1/task1 even? 2 (range 10000))))
    (time (doall (lab3.task1/task1 even? 3 (range 10000))))
    (time (doall (lab3.task1/task1 even? 4 (range 10000))))
    (time (doall (lab3.task1/task1 even? 5 (range 10000))))
    (time (doall (lab3.task1/task1 even? 6 (range 10000))))
    (time (doall (lab3.task1/task1 even? 7 (range 10000))))
    (time (doall (lab3.task1/task1 even? 8 (range 10000))))
    (time (doall (lab3.task1/task1 even? 9 (range 10000))))
    (time (doall (lab3.task1/task1 even? 10 (range 10000))))
    (println)))
    
(deftest task2-test
  (testing "task2"
    (is (lab3.task2/task2 even? 1 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 2 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 3 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 4 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 5 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 6 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 7 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 8 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 9 3 (range 100)) (filter even? (range 100)))
    (is (lab3.task2/task2 even? 10 3 (range 100)) (filter even? (range 100)))))

(deftest task2-infinite-test
  (testing "task2-infinite"
    (is (take 1000 (lab3.task2/task2 even? 1 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 2 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 3 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 4 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 5 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 6 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 7 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 8 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 9 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))
    (is (take 1000 (lab3.task2/task2 even? 10 3 (iterate inc 0))) (take 1000 (filter even? (iterate inc 0))))))

(deftest task2-time-test
  (testing "task2-time"
    (println)
    (println "Test filter time wint (take 1000) (iterate inc 0)")
    (time (doall (take 1000 (filter even? (iterate inc 0)))))
    (println)
    (println "Test task2 time wint (take 1000) (iterate inc 0) from 1 to 10 threads")
    (time (doall (take 1000 (lab3.task2/task2 even? 1 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 2 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 3 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 4 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 5 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 6 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 7 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 8 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 9 3 (iterate inc 0)))))
    (time (doall (take 1000 (lab3.task2/task2 even? 10 3 (iterate inc 0)))))
    (println)))

