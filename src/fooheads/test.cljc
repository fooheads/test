(ns fooheads.test
  (:require
    [clojure.test :refer [is]]
    [fooheads.skepnad :as skepnad]
    [lambdaisland.deep-diff2 :as ddiff])
  #?(:cljs
     (:require-macros
       [fooheads.test])))


(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:js-globals env)))


(defn ex-symbol
  [env]
  (if (cljs-env? env) 'js/Error 'Exception))


(defmacro thrown-ex-data
  "Catches an exception in `body` and extracts the ex-data, to make it easy to
  test exceptional code. If `ks` is given, these keys are extracted from the
  ex-data map.

  If an exception is not thrown, it returns
  `{:expected-exception :was-not-thrown}`"
  ([body]
   `(thrown-ex-data nil ~body))
  ([ks body]
   `(try
      ~body
      (throw (ex-info "Exception was not thrown" {:expected-exception :was-not-thrown}))
      (catch ~(ex-symbol &env) e#
        (let [ks# ~ks
              data# (ex-data e#)]
          (cond
            (contains? data# :expected-exception)
            data#

            (nil? ks#)
            data#

            :else
            (select-keys data# ks#)))))))


(defmacro should-be
  [expected actual]
  `(let [actual# ~actual
         expected# ~expected
         actual'# (skepnad/intersection expected# actual#)]

     (when-not (is (= expected# actual'#))
       (ddiff/pretty-print (ddiff/diff expected# actual'#)))))


(defmacro reduce-are
  "Like `clojure.test/are`, but reduces over an initial value.

  The reducing value is expected to be on the left side, and on the right side
  there is one or many functions to apply. The reducing value is carried over to
  next pair.

  `init` is either just a single initial value, or a vector with
  [initial-value left-f right-f], where `left-f` and `right-f` are
  functions that are applied to left/right side of each pair.

  Examples:

  `
  (reduce-are 1
     2 inc
     3 inc)

  (reduce-are [1 parse-long]
     \"2\" inc
    \"13\" [inc #(+ 10 %)]
    \"12\" dec)

  (reduce-are [1 identity str]
     2 inc
    13 [inc #(+ 10 %)]
    12 dec))
  `
  "
  [init & pairs]
  `(let [init# ~init
         [x# left-f# right-f#]
         (concat (if (sequential? init#) init# [init#]) (repeat identity))
         pairs# (partition 2 (vector ~@pairs))]
     (reduce
       (fn f# [x# [left# right#]]
         (if-not (sequential? right#)
           (f# x# [left# [right#]])
           (let [res# (reduce (fn [x# f#] (f# x#)) x# right#)]
             (is (= (left-f# left#) (right-f# res#)))
             res#)))

       x#
       pairs#)
     nil))

