(ns lab4.parser
    (:use lab4.api))

(defn lexer
    "Lexer to parse input string"
    [string]
        (map 
            #(cond
                (= % \1) ::true
                (= % \0) ::false
                (= % \&) ::and
                (= % \|) ::or
                (= % \-) ::not
                (= % \>) ::impl
                (= % \() ::open
                (= % \)) ::close
                :else (list ::sym (keyword (str %)))) string)) 

(declare rpn)

(defn cons-to-stack 
    "Cons first token to stack and pop others tokens from stack using pred?"
    [pred? tokens result stack]
        (let [pop (take-while pred? stack)
              rest-stack (drop-while pred? stack)]
            (rpn
                (rest tokens)
                (if (empty? pop)
                    result
                    (vec (concat result pop)))
                (cons (first tokens) rest-stack))))

(defn rpn
    "Build Reverse Polish Notation from input tokens"
    [tokens result stack]
        (let [token (first tokens)
              rest-tokens (rest tokens)]
            (cond 
                (= token ::true)
                    (rpn rest-tokens (conj result token) stack)
                (= token ::false)
                    (rpn rest-tokens (conj result token) stack)
                (= token ::and)
                    (cons-to-stack #(or (= % ::not) (= % ::and)) tokens result stack)
                (= token ::or)
                    (cons-to-stack #(or (= % ::not) (= % ::and) (= % ::or)) tokens result stack)
                (= token ::not)
                    (rpn rest-tokens result (cons token stack))
                (= token ::impl)
                    (cons-to-stack #(or (= % ::not) (= % ::and)) tokens result stack)
                (= token ::open)
                    (rpn rest-tokens result (cons token stack))
                (= token ::close)
                    (let [pop (take-while #(not (= % ::open)) stack)
                          rest-stack (drop-while #(not (= % ::open)) stack)]
                        (rpn
                            (rest tokens)
                            (if (empty? pop)
                                result
                                (vec (concat result pop)))
                            (rest rest-stack)))
                (= (first token) ::sym)
                    (rpn rest-tokens (conj result token) stack)
                :else
                    (if (empty? stack)
                        result
                        (concat result stack)))))

(defn parse
    "Parse input string to api"
    ([string] (parse (rpn (lexer string) [] `()) `()))
    ([rpn stack]
        (let [token (first rpn)
              rest-rpn (rest rpn)]
            (cond
                (= token ::true)
                    (parse rest-rpn (cons (constant true) stack))
                (= token ::false)
                    (parse rest-rpn (cons (constant false) stack))
                (= token ::and)
                    (parse rest-rpn (cons (dnf-and (second stack) (first stack)) (drop 2 stack)))
                (= token ::or)
                    (parse rest-rpn (cons (dnf-or (second stack) (first stack)) (drop 2 stack)))
                (= token ::not)
                    (parse rest-rpn (cons (dnf-not (first stack)) (rest stack)))
                (= token ::impl)
                    (parse rest-rpn (cons (dnf-impl (second stack) (first stack)) (drop 2 stack)))
                (= (first token) ::sym)
                    (parse rest-rpn (cons (variable (second token)) stack))
                :else
                    (first stack)))))
