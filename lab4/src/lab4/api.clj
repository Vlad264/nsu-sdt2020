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

;; =================================== And ===================================

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

(defn dnf-impl-first
    "Get first argument of implication"
    [expr]
    {:pre (dnf-impl? expr)}
    (first (rest expr)))

(defn dnf-impl-second
    "Get second argument of implication"
    [expr]
    {:pre (dnf-impl? expr)}
    (last (rest expr)))

;; =================================== Args ===================================

(defn args
    "Return arguments of expression"
    [expr] (rest expr))

(defn update-args 
    "Update arguments in expression"
    [expr new-args]
    (if (> (count new-args) 1)
        (cons (first expr) new-args)
        (list (first expr) (first new-args))))

(defn collect-args
    "Recursively collect arguments with same type"
    [expr]
    (apply concat 
        (map
            #(if (= (first expr) (first %))
                (collect-args %)
                (list %))
            (args expr))))
