(ns lab3.task2
    (:use lab3.task1))

(defn task2 [f n s xs]
    (->> xs
        (my-partition (* n s))
        (map (fn [ys] 
            (->> ys
                (my-partition s)
                (map (fn [y] (future (doall (filter f y)))))
                (doall)
                (map deref)
                (apply concat))))
        (flatten)))
