(ns lab4.api)

;; =================================== Constant ===================================

(defn constant
    "Constructor for constant"
    [value] (list ::const (if value true false)))

(defn constant?
    "Check if expression is constant"
    [expr] (= (first expr) ::const))

(defn constant-value
    "Get constant value"
    [expr] (second expr))

;; =================================== Variables ===================================

(defn variable
    "Constructor for variable"
    [name]
    {:pre [(keyword? name)]}
    (list ::var name))

(defn variable?
    "Check if expression is variable"
    [expr] (= (first expr) ::var))

(defn variable-name
    "Get variable name"
    [var] (second var))

(defn same-variables?
    "Check if two variable are same"
    [v1 v2]
    (and
        (variable? v1)
        (variable? v2)
        (= (variable-name v1)
           (variable-name v2))))

(defn leaf?
    "Check if it leaf node (variable or constant)"
    [expr]
    (or
        (variable? expr)
        (constant? expr)))

;; =================================== And ===================================

(defn args
    "Get arguments of expression"
    [expr] (rest expr))

(defn dnf-and
    "Constructor for conjunctions operation"
    [expr & rest]
    (if (empty? rest)
        expr
        (cons ::and (cons expr rest))))

(defn dnf-and?
    "Check if expression is conjunction"
    [expr] (= (first expr) ::and))

;; =================================== Or ===================================

(defn dnf-or
    "Constructor for disjunction expression"
    [expr & rest]
    (if (empty? rest)
        expr
        (cons ::or (cons expr rest))))

(defn dnf-or?
    "Check if expression is disjunction"
    [expr] (= (first expr) ::or))

;; =================================== Not ===================================

(defn dnf-not
    "Constructor for not expression"
    [expr] (list ::not expr))

(defn dnf-not?
    "Check if expression is not"
    [expr] (= (first expr) ::not))

;; =================================== Implication ===================================

(defn dnf-impl
    "Constructor for implication expression"
    [expr1 expr2] (list ::impl expr1 expr2))

(defn dnf-impl?
    "Check if expression is implication"
    [expr] (= (first expr) ::impl))

(defn impl-first-arg
    "Get first argument of implication"
    [expr]
    {:pre (dnf-impl? expr)}
    (first (rest expr)))

(defn impl-second-arg
    "Get second argument of implication"
    [expr]
    {:pre (dnf-impl? expr)}
    (last (rest expr)))

;; ================== Utils ==================

(defn dnf-of-type [expr args] (cons (first expr) args))

(defn same-type?
    "Check that expr1 and expr2 have same type"
    [expr1 expr2] (= (first expr1) (first expr2)))

(defn same-expr-strict?
    "Check if two expressions is same"
    [expr1 expr2]
    (if (not (same-type? expr1 expr2))
        false
        (cond
            (variable? expr1) (same-variables? expr1 expr2)
            (constant? expr1) (= (constant-value expr1) (constant-value expr2))
            :else (->> (map #(same-expr-strict? %1 %2) (args expr1) (args expr2))
                       (every? true?)))))

(defn same-expr?
    [expr1 expr2]
    (cond
        (= expr1 expr2) true
        (same-type? expr1 expr2) (cond
                                     (variable? expr1) (same-variables? expr1 expr2)
                                     (constant? expr1) (= (constant-value expr1) (constant-value expr2))
                                     :else (let [aa (first (args expr1)) ab (last (args expr1))
                                                 ba (first (args expr2)) bb (last (args expr2))]
                                               (or
                                                   (and
                                                       (same-expr? aa ba)
                                                       (same-expr? ab bb))
                                                   (and
                                                       (same-expr? aa bb)
                                                       (same-expr? ab ba)))))
        :else false))

(defn update-args [expr new-args]
    (if (> (count new-args) 1)
        (cons (first expr) new-args)
        (list (first expr) (first new-args))))

(defn compose
    "(a*b*c) -> (a*(b*c))"
    [expr]
    (if (> (count (args expr)) 2)
        (reduce (fn [acc x] (dnf-of-type acc (list acc x))) (dnf-of-type expr (take 2 (args expr))) (drop 2 (args expr)))
        expr))

(defn- collect-args
    [expr]
    (if (leaf? expr)
        (list expr)
        (->> (args expr)
             (map (fn [arg] (if (same-type? expr arg)
                                (collect-args arg)
                                (list arg))))
             (apply concat))))
(defn decompose
    "(a*(b*c)) -> (a*b*c)"
    [expr]
    (if (and (not (leaf? expr))
             (or
                 (dnf-or? expr)
                 (dnf-and? expr))
             )
        (update-args expr (map #(decompose %) (collect-args expr)))
        expr))


