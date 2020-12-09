(ns lab4.dnf
    (:use lab4.api))

(defn apply-recur
    "Apply function f to expr recurrently"
    [f expr]
    (if (or (variable? (f expr)) (constant? (f expr)))
        (f expr)
        (f (update-args expr (map #(apply-recur f %) (args expr))))))

(defn remove-implication
    "Transform all implication A > B into not A or B"
    [expr]
    (if (dnf-impl? expr)
        (dnf-or
            (dnf-not (dnf-impl-first expr))
            (dnf-impl-second expr))
        expr))

(defn apply-not-to-brackets
    "Apply not expression to brackets"
    [expr]
    (if (dnf-not? expr)
        (let [next-expr (first (args expr))]
            (cond
                (dnf-not? next-expr)
                    (first (args next-expr))
                (constant? next-expr)
                    (if (= next-expr (constant true))
                        (constant false)
                        (constant true))
                (dnf-and? next-expr)
                    (apply dnf-or (map #(dnf-not %) (args next-expr)))
                (dnf-or? next-expr)
                    (apply dnf-and (map #(dnf-not %) (args next-expr)))
                :else
                    expr))
        expr))

(defn distributive-property
    "Transforms a&(b|c) to (a&b)|(a&c), where x = a and y = (b|c)"
    [x y]
    (dnf-or (dnf-and x (first (args y))) (dnf-and x (last (args y)))))

(defn apply-distributive-property
    "Apply distributive property"
    [expr]
    (if (dnf-and? expr)
        (let [x (first (args expr))
              y (last (args expr))]
            (cond
                (dnf-or? x)
                    (distributive-property y x)
                (dnf-or? y)
                    (distributive-property x y)
                :else
                    expr))
        expr))

(defn apply-decompose
    "Apply decomtose: transforms (a*(b*(c*...))) to (a*b*c*...)"
    [expr]
    (if (or (dnf-and? expr) (dnf-or? expr))
        (update-args expr (collect-args expr))
        expr))

(defn replace-variable
    "Replace variables using var-map"
    [expr var-map]
    (if (and (not (empty? var-map)) (variable? expr))
        (let [var-name (variable-name expr)]
            (if (contains? var-map var-name)
                (constant (get var-map var-name))
                expr))
        expr))

(defn simplify
    "Siplify expressions with constant"
    [expr]
    (cond
        (and (dnf-and? expr) (some #(= (constant false) %) (args expr)))
            (constant false)
        (and (dnf-and? expr) (some #(= (constant true) %) (args expr)))
            (if (apply = (args expr))
                (constant true)
                (apply dnf-and (filter #(not (= (constant true) %)) (args expr))))
        (and (dnf-or? expr) (some #(= (constant true) %) (args expr)))
            (constant true)
        (and (dnf-or? expr) (some #(= (constant false) %) (args expr)))
            (if (apply = (args expr))
                (constant false)
                (apply dnf-and (filter #(not (= (constant false) %)) (args expr))))
        :else
            expr))

(defn to-dnf
    "Transforms expression to dnf"
    ([expr]
        (to-dnf expr `()))
    ([expr var-map]
    (let [replace-by-var-map #(replace-variable % var-map)]
        (->> expr
            (apply-recur remove-implication)
            (apply-recur apply-not-to-brackets)
            (apply-recur apply-distributive-property)
            (apply-recur apply-decompose)
            (apply-recur replace-by-var-map)
            (apply-recur simplify)))))
