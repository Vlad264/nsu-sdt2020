(ns lab4.api-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest constant-test
  (testing "constant"
    (is (= (lab4.api/constant true) `(:lab4.api/const true)))
    (is (= (lab4.api/constant false) `(:lab4.api/const false)))
    (is (lab4.api/constant? `(:lab4.api/const true)))
    (is (lab4.api/constant? `(:lab4.api/const false)))
    (is (not (lab4.api/constant? `(:lab4.api/var :a))))
    (is (= (lab4.api/constant-value `(:lab4.api/const true)) true))
    (is (= (lab4.api/constant-value `(:lab4.api/const false)) false))
    (is (not (= (lab4.api/constant-value `(:lab4.api/const true)) false)))))

(deftest variable-test
  (testing "variable"
    (is (= (lab4.api/variable :a) `(:lab4.api/var :a)))
    (is (lab4.api/variable? `(:lab4.api/var :a)))
    (is (not (lab4.api/variable? `(:lab4.api/const true))))
    (is (= (lab4.api/variable-name `(:lab4.api/var :a)) :a))
    (is (not (= (lab4.api/variable-name `(:lab4.api/var :a)) :b)))
    (is (lab4.api/same-variables? `(:lab4.api/var :a) `(:lab4.api/var :a)))
    (is (not (lab4.api/same-variables? `(:lab4.api/const true) `(:lab4.api/var :a))))
    (is (not (lab4.api/same-variables? `(:lab4.api/var :a) `(:lab4.api/var :b))))))

(deftest and-test
  (testing "and"
    (is (= (lab4.api/dnf-and `(:lab4.api/const true)) `(:lab4.api/const true)))
    (is (= (lab4.api/dnf-and `(:lab4.api/const true) `(:lab4.api/const true)) `(:lab4.api/and (:lab4.api/const true) (:lab4.api/const true))))
    (is (= (lab4.api/dnf-and `(:lab4.api/const true) `(:lab4.api/const false)) `(:lab4.api/and (:lab4.api/const true) (:lab4.api/const false))))
    (is (= (lab4.api/dnf-and `(:lab4.api/const true) `(:lab4.api/var :a) `(:lab4.api/var :b)) `(:lab4.api/and (:lab4.api/const true) (:lab4.api/var :a) (:lab4.api/var :b))))
    (is (lab4.api/dnf-and? `(:lab4.api/and (:lab4.api/const true) (:lab4.api/const true))))
    (is (not (lab4.api/dnf-and? `(:lab4.api/const true))))))

(deftest or-test
  (testing "or"
    (is (= (lab4.api/dnf-or `(:lab4.api/const true)) `(:lab4.api/const true)))
    (is (= (lab4.api/dnf-or `(:lab4.api/const true) `(:lab4.api/const true)) `(:lab4.api/or (:lab4.api/const true) (:lab4.api/const true))))
    (is (= (lab4.api/dnf-or `(:lab4.api/const true) `(:lab4.api/const false)) `(:lab4.api/or (:lab4.api/const true) (:lab4.api/const false))))
    (is (= (lab4.api/dnf-or `(:lab4.api/const true) `(:lab4.api/var :a) `(:lab4.api/var :b)) `(:lab4.api/or (:lab4.api/const true) (:lab4.api/var :a) (:lab4.api/var :b))))
    (is (lab4.api/dnf-or? `(:lab4.api/or (:lab4.api/const true) (:lab4.api/const true))))
    (is (not (lab4.api/dnf-or? `(:lab4.api/const true))))))

(deftest not-test
  (testing "not"
    (is (= (lab4.api/dnf-not `(:lab4.api/const true)) `(:lab4.api/not (:lab4.api/const true))))
    (is (= (lab4.api/dnf-not `(:lab4.api/const false)) `(:lab4.api/not (:lab4.api/const false))))
    (is (= (lab4.api/dnf-not `(:lab4.api/var :a)) `(:lab4.api/not (:lab4.api/var :a))))
    (is (lab4.api/dnf-not? `(:lab4.api/not (:lab4.api/const true))))
    (is (not (lab4.api/dnf-not? `(:lab4.api/const true))))))


(deftest impl-test
  (testing "impl"
    (is (= (lab4.api/dnf-impl `(:lab4.api/const true) `(:lab4.api/const true)) `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/const true))))
    (is (= (lab4.api/dnf-impl `(:lab4.api/const true) `(:lab4.api/const false)) `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/const false))))
    (is (= (lab4.api/dnf-impl `(:lab4.api/var :a) `(:lab4.api/var :b)) `(:lab4.api/impl (:lab4.api/var :a) (:lab4.api/var :b))))
    (is (lab4.api/dnf-impl? `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/const true))))
    (is (not (lab4.api/dnf-impl? `(:lab4.api/const true))))
    (is (= (lab4.api/dnf-impl-first `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/const true))) `(:lab4.api/const true)))
    (is (= (lab4.api/dnf-impl-first `(:lab4.api/impl (:lab4.api/var :a) (:lab4.api/const true))) `(:lab4.api/var :a)))
    (is (= (lab4.api/dnf-impl-second `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/const true))) `(:lab4.api/const true)))
    (is (= (lab4.api/dnf-impl-second `(:lab4.api/impl (:lab4.api/const true) (:lab4.api/var :a))) `(:lab4.api/var :a)))))

(deftest args-test
  (testing "not"
    (is (= (lab4.api/args `(:lab4.api/and (:lab4.api/const true) (:lab4.api/const true))) `((:lab4.api/const true) (:lab4.api/const true))))
    (is (= (lab4.api/args `(:lab4.api/and (:lab4.api/const true) (:lab4.api/const false))) `((:lab4.api/const true) (:lab4.api/const false))))
    (is (= (lab4.api/args `(:lab4.api/and (:lab4.api/var :a) (:lab4.api/var :b))) `((:lab4.api/var :a) (:lab4.api/var :b))))
    (is (= (lab4.api/update-args `(:lab4.api/and (:lab4.api/var :a) (:lab4.api/var :b)) `((:lab4.api/var :c) (:lab4.api/var :d))) `(:lab4.api/and (:lab4.api/var :c) (:lab4.api/var :d))))
    (is (= (lab4.api/collect-args `(:lab4.api/and (:lab4.api/var :a) (:lab4.api/and (:lab4.api/var :b) (:lab4.api/and (:lab4.api/var :c) (:lab4.api/var :d)))))
                                  `((:lab4.api/var :a) (:lab4.api/var :b) (:lab4.api/var :c) (:lab4.api/var :d))))))