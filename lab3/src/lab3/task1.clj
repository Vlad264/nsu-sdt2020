(ns lab3.task1)

(defn my-partition [n xs]
    (take-while 
        #(not (empty? %))
        (lazy-seq
            (cons
                (take n xs)
                (my-partition n (drop n xs))))))

(defn task1 [f n xs]
    (->> xs
        (my-partition (/ (count xs) n))
        (map #(future (doall (filter f %))))
        (doall)
        (map deref)
        (apply concat)))
