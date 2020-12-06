(ns lab4.parser
    (:use lab4.api))

(defn lexer
    "Lexer to parse input string"
    [str]
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
                :else (list ::sym (keyword (str %)))) str)) 

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
              rest (rest tokens)]
            (cond 
                (= token ::true)
                    (rpn rest (conj result token) stack)
                (= token ::false)
                    (rpn rest (conj result token) stack)
                (= token ::and)
                    (cons-to-stack #(or (= % ::not) (= % ::and)) tokens result stack)
                (= token ::or)
                    (cons-to-stack #(or (= % ::not) (= % ::and) (= % ::or)) tokens result stack)
                (= token ::not)
                    (rpn rest result (cons token stack))
                (= token ::impl)
                    (cons-to-stack #(or (= % ::not) (= % ::and)) tokens result stack)
                (= token ::open)
                    (rpn rest result (cons token stack))
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
                    (rpn rest (conj result token) stack)
                :else
                    (if (empty? stack)
                        result
                        (concat result stack)))))

(defn parse
    "Parse input string to api"
    ([str] (parse (rpn (lexer str) [] `()) `()))
    ([rpn stack]
        (let [token (first rpn)
              rest (rest rpn)]
            (cond
                (= token ::true)
                    (parse rest (cons (constant true) stack))
                (= token ::false)
                    (parse rest (cons (constant false) stack))
                (= token ::and)
                    (parse rest (cons (dnf-and (second stack) (first stack)) (drop 2 stack)))
                (= token ::or)
                    (parse rest (cons (dnf-or (second stack) (first stack)) (drop 2 stack)))
                (= token ::not)
                    (parse rest (cons (dnf-not (first stack)) (rest stack)))
                (= token ::impl)
                    (parse rest (cons (dnf-impl (second stack) (first stack)) (drop 2 stack)))
                (= (first token) ::sym)
                    (parse rest (cons (variable (second token)) stack))
                :else
                    (first stack)))))
