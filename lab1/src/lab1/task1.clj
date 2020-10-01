(ns lab1.task1)

(defn add-char [c xs]
    (if (> (count xs) 0)
        (if (= c (.substring (first xs) 0 1))
            (add-char c (rest xs))
            (concat
                (list (str c (first xs)))
                (add-char c (rest xs))))
        (list)))

(defn concat-string-list [xs ys]
    (if (> (count xs) 0) 
        (concat
            (add-char (first xs) ys)
            (concat-string-list (rest xs) ys))
        (list)))

(defn task1
    ([xs n] 
        (if (> n 0)
            (task1 xs (dec n) xs)
            (list)))
    ([xs n acc]
        (if (> n 0)
            (task1 xs (dec n) (concat-string-list xs acc))
            acc)))
