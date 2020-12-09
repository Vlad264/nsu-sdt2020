(ns lab4.dnf-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest apply-recur-test
  (testing "apply-recur"
    (let [foo #(cond 
                  (lab4.api/constant? %) (lab4.api/constant false)
                  (lab4.api/variable? %) (lab4.api/variable :z)
                  :else (lab4.api/update-args `(::test) (lab4.api/args %)))]
    (is (= (lab4.dnf/apply-recur foo (lab4.parser/parse "a&b")) `(:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.api/var :z))))
    (is (= (lab4.dnf/apply-recur foo (lab4.parser/parse "a|b")) `(:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.api/var :z))))
    (is (= (lab4.dnf/apply-recur foo (lab4.parser/parse "a>b")) `(:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.api/var :z))))
    (is (= (lab4.dnf/apply-recur foo (lab4.parser/parse "a&(a|1)")) `(:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.api/const false)))))
    (is (= (lab4.dnf/apply-recur foo (lab4.parser/parse "a&(a|(1&1))")) 
          `(:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.dnf-test/test (:lab4.api/var :z) (:lab4.dnf-test/test (:lab4.api/const false) (:lab4.api/const false)))))))))

(deftest remove-implication-test
  (testing "remove-implication"
    (is (= (lab4.dnf/remove-implication (lab4.parser/parse "a&b")) (lab4.parser/parse "a&b")))
    (is (= (lab4.dnf/remove-implication (lab4.parser/parse "a>b")) (lab4.parser/parse "-a|b")))))

(deftest apply-not-to-brackets-test
  (testing "apply-not-to-brackets"
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "a&b")) (lab4.parser/parse "a&b")))
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "--a")) (lab4.parser/parse "a")))
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "-0")) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "-1")) (lab4.parser/parse "0")))
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "-(a&b)")) (lab4.parser/parse "-a|-b")))
    (is (= (lab4.dnf/apply-not-to-brackets (lab4.parser/parse "-(a|b)")) (lab4.parser/parse "-a&-b")))))

(deftest apply-distributive-property-test
  (testing "apply-distributive-property"
    (is (= (lab4.dnf/distributive-property (lab4.parser/parse "a") (lab4.parser/parse "b|c")) (lab4.parser/parse "(a&b)|(a&c)")))
    (is (= (lab4.dnf/apply-distributive-property (lab4.parser/parse "a&(b|c)")) (lab4.parser/parse "(a&b)|(a&c)")))
    (is (= (lab4.dnf/apply-distributive-property (lab4.parser/parse "(a|b)&c")) (lab4.parser/parse "(c&a)|(c&b)")))))

(deftest apply-decompose-test
  (testing "apply-decompose-property"
    (is (= (lab4.dnf/apply-decompose (lab4.parser/parse "a&b&c&d")) `(:lab4.api/and (:lab4.api/var :a) (:lab4.api/var :b) (:lab4.api/var :c) (:lab4.api/var :d))))
    (is (= (lab4.dnf/apply-decompose (lab4.parser/parse "a|b|c|d")) `(:lab4.api/or (:lab4.api/var :a) (:lab4.api/var :b) (:lab4.api/var :c) (:lab4.api/var :d))))))

(deftest simplify-test
  (testing "remove-implication"
    (is (= (lab4.dnf/simplify (lab4.parser/parse "a&0")) (lab4.parser/parse "0")))
    (is (= (lab4.dnf/simplify (lab4.parser/parse "1&1")) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/simplify (lab4.parser/parse "a&1")) (lab4.parser/parse "a")))
    (is (= (lab4.dnf/simplify (lab4.parser/parse "a|1")) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/simplify (lab4.parser/parse "0|0")) (lab4.parser/parse "0")))
    (is (= (lab4.dnf/simplify (lab4.parser/parse "a|0")) (lab4.parser/parse "a")))))

(deftest to-dnf-test
  (testing "to-dnf"
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&b")) (lab4.parser/parse "a&b")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a>b")) (lab4.parser/parse "-a|b")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a>b>c")) (lab4.dnf/apply-decompose (lab4.parser/parse "-a|-b|c"))))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a|b")) (lab4.parser/parse "a|b")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&(b|c)")) (lab4.parser/parse "a&b|a&c")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "(a|b)>(c|d>e)")) (lab4.dnf/apply-decompose (lab4.parser/parse "-a&-b|c|-d|e"))))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "(1|0)&1")) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&b") {:a true :b true}) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&b") {:a true :b false}) (lab4.parser/parse "0")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&(b|c)") {:a true :b true :c true}) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&(b|c)") {:a true :b false :c true}) (lab4.parser/parse "1")))
    (is (= (lab4.dnf/to-dnf (lab4.parser/parse "a&(b|c)") {:a true :b false :c false}) (lab4.parser/parse "0")))))
