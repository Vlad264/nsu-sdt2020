(ns lab4.parser-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest lexer-test
  (testing "lexer"
    (is (= (lab4.parser/lexer "0") `(:lab4.parser/false)))
    (is (= (lab4.parser/lexer "1") `(:lab4.parser/true)))
    (is (= (lab4.parser/lexer "&") `(:lab4.parser/and)))
    (is (= (lab4.parser/lexer "|") `(:lab4.parser/or)))
    (is (= (lab4.parser/lexer "-") `(:lab4.parser/not)))
    (is (= (lab4.parser/lexer ">") `(:lab4.parser/impl)))
    (is (= (lab4.parser/lexer "(") `(:lab4.parser/open)))
    (is (= (lab4.parser/lexer ")") `(:lab4.parser/close)))
    (is (= (lab4.parser/lexer "a") (list (list :lab4.parser/sym :a))))
    (is (= (lab4.parser/lexer "1&1") `(:lab4.parser/true :lab4.parser/and :lab4.parser/true)))
    (is (= (lab4.parser/lexer "1&1|0") `(:lab4.parser/true :lab4.parser/and :lab4.parser/true :lab4.parser/or :lab4.parser/false)))
    (is (= (lab4.parser/lexer "(a&1)|0") (list :lab4.parser/open (list :lab4.parser/sym :a) :lab4.parser/and :lab4.parser/true :lab4.parser/close :lab4.parser/or :lab4.parser/false)))))

(deftest rpn-test
  (testing "rpn"
    (is (= (lab4.parser/rpn (list :lab4.parser/false) [] `()) `(:lab4.parser/false)))
    (is (= (lab4.parser/rpn (list :lab4.parser/false :lab4.parser/and :lab4.parser/false) [] `()) 
           `(:lab4.parser/false :lab4.parser/false :lab4.parser/and)))
    (is (= (lab4.parser/rpn (list :lab4.parser/open (list :lab4.parser/sym (keyword "a")) :lab4.parser/and :lab4.parser/true :lab4.parser/close :lab4.parser/or :lab4.parser/false) [] `()) 
           (list (list :lab4.parser/sym :a) :lab4.parser/true :lab4.parser/and :lab4.parser/false :lab4.parser/or)))))

(deftest parse-test
  (testing "parse"
    (is (= (lab4.parser/parse "0") `(:lab4.api/const false)))
    (is (= (lab4.parser/parse "1") `(:lab4.api/const true)))
    (is (= (lab4.parser/parse "a") `(:lab4.api/var :a)))
    (is (= (lab4.parser/parse "(a)") `(:lab4.api/var :a)))
    (is (= (lab4.parser/parse "-a") (list :lab4.api/not (list :lab4.api/var :a))))
    (is (= (lab4.parser/parse "a&a") (list :lab4.api/and (list :lab4.api/var :a) (list :lab4.api/var :a))))
    (is (= (lab4.parser/parse "a|a") (list :lab4.api/or (list :lab4.api/var :a) (list :lab4.api/var :a))))
    (is (= (lab4.parser/parse "a>a") (list :lab4.api/impl (list :lab4.api/var :a) (list :lab4.api/var :a))))
    (is (= (lab4.parser/parse "a&-a") (list :lab4.api/and (list :lab4.api/var :a) (list :lab4.api/not (list :lab4.api/var :a)))))
    (is (= (lab4.parser/parse "(a&a)>-b") (list :lab4.api/impl (list :lab4.api/and (list :lab4.api/var :a) (list :lab4.api/var :a)) (list :lab4.api/not (list :lab4.api/var :b)))))))
