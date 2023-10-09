(ns fooheads.test-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.test :refer [reduce-are thrown-ex-data]]))


(defn problem-function [x y]
  (let [msg (str "Problem: x is " x)]
    (throw (ex-info msg {:msg msg :x x :y y}))))


(deftest thrown-ex-data-test
  (testing "with all ex-data"
    (is (= {:msg "Problem: x is 6" :x 6 :y [:a :b :c]}
           (thrown-ex-data
             (problem-function 6 [:a :b :c])))))

  (testing "with some ex-data"
    (is (= {:msg "Problem: x is 6" :x 6}
           (thrown-ex-data
             [:msg :x]
             (problem-function 6 [:a :b :c])))))

  (testing "expected exception not thrown"
    (is (= {:expected-exception :was-not-thrown}
           (thrown-ex-data
             [:msg :x]
             (identity 6))))))


(deftest reduce-are-test
  (reduce-are
    1
    2 inc
    3 inc)

  (reduce-are
    [1]

    2 inc
    3 inc)

  (reduce-are
    [1 parse-long]

    "2"  inc
    "13" [inc #(+ 10 %)]
    "12" dec)

  (reduce-are
    [1 identity str]

    "2"  inc
    "13" [inc #(+ 10 %)]
    "12" dec))

