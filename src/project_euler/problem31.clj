(ns project-euler.problem31
  (:require [criterium.core :as crit]
            [zprint.core :as zp :refer [czprint]]))

;; ---------------------------------------------------------------------------
;; Section 1: three different, normalized solutions 

;; Recursive solution from Chris Houser
;; Double recur by:
;;   1. keeping amount and removing big coin
;;   2. reducing amount by big coin and keeping coins
(defn make-change-chouser
  [coins amount]
  (cond
    (neg? amount) 0
    (zero? amount) 1
    (= [] coins) 0
    :else (+ (make-change-chouser (drop-last coins) amount)
             (make-change-chouser coins (- amount (last coins))))))


;; Recursive solution from Ben Grabow
;; Multiple recur by as many ways as big coin can be taken from amount.
(defn make-change-bgrabow
  [coins amount]
  (cond
    (neg? amount) 0
    (zero? amount) 1
    (= [] coins) 0
    :else (let [coin (last coins)]
            (apply +
                   (for [n (range (inc (quot amount coin)))]
                     (make-change-bgrabow
                       (drop-last coins) (- amount (* n coin))))))))


;; Recursive solution from Jonathan Claggett
;; Multiple recur by as many coins.
(defn make-change-jclaggett
  [coins amount]
  (cond
    (neg? amount) 0
    (zero? amount) 1
    (= [] coins) 0
    :else (apply +
                 (for [sub-coins (rest (reductions conj [] coins))]
                   (make-change-jclaggett
                     sub-coins (- amount (last sub-coins)))))))


;; ---------------------------------------------------------------------------
;; Section 2: Simple optimizations and macro

(defmacro fn-mc
  [args & body] ;; args ::= [... 'coins 'amount], body ::= clojure-form
  `(fn ~args
     (cond
       (neg? ~'amount) 0
       (zero? ~'amount) 1
       (= [] ~'coins) 0
       #_#_(= [1] ~'coins) 1                         ;; 1st Optimization
       #_#_(= [1 2] ~'coins) (inc (quot ~'amount 2)) ;; 2nd Optimization
       :else (do ~@body))))

(def make-change-chouser-optimized
  (fn-mc [coins amount]
         (+ (make-change-chouser-optimized (drop-last coins) amount)
            (make-change-chouser-optimized coins (- amount (last coins))))))

(def make-change-bgrabow-optimized
  (fn-mc [coins amount]
         (let [coin (last coins)]
           (apply +
                  (for [n (range (inc (quot amount coin)))]
                    (make-change-bgrabow-optimized
                      (drop-last coins) (- amount (* n coin))))))))

(def make-change-jclaggett-optimized
  (fn-mc [coins amount]
         (apply +
                (for [sub-coins (rest (reductions conj [] coins))]
                  (make-change-jclaggett-optimized
                    sub-coins (- amount (last sub-coins)))))))


;; ---------------------------------------------------------------------------
;; Section 3: Memoization versions

(def make-change-chouser-memoized
  (memoize
    (fn-mc [coins amount]
           (+ (make-change-chouser-memoized (drop-last coins) amount)
              (make-change-chouser-memoized coins (- amount (last coins)))))))

(def make-change-bgrabow-memoized
  (memoize
    (fn-mc [coins amount]
           (let [coin (last coins)]
             (apply +
                    (for [n (range (inc (quot amount coin)))]
                      (make-change-bgrabow-memoized
                        (drop-last coins) (- amount (* n coin)))))))))

(def make-change-jclaggett-memoized
  (memoize
    (fn-mc [coins amount]
           (apply +
                  (for [sub-coins (rest (reductions conj [] coins))]
                    (make-change-jclaggett-memoized
                      sub-coins (- amount (last sub-coins))))))))


;; ---------------------------------------------------------------------------
;; Section 4: Updater style recursive solution caching previous calculations
(def get-change
  (fn-mc [cm coins amount]
    (get cm [coins amount])))

(defn calc-change
  [cm coins amount]
  (let [{:keys [cm total]}
        (reduce
          (fn [{:keys [cm total coins] :as state} coin]
            (let [amount (- amount coin)
                  coins (->> (conj coins coin)
                             (take-while #(<= % amount))
                             vec)
                  [cm change]
                  (if-let [change (get-change cm coins amount)]
                    [cm change]
                    (let [coin (last coins)
                          cm (reduce
                               #(calc-change %1 coins %2)
                               cm
                               (range (+ coin (rem amount coin))
                                      (+ coin amount)
                                      coin))
                          change (get-change cm coins amount)]
                      [cm change]))]
              (assoc state
                     :cm cm
                     :total (+ total change)
                     :coins coins)))
          {:cm cm
           :total 0
           :coins []}
          coins)]
    (assoc cm [coins amount] total)))

(defn make-change-updater [coins amount]
  (-> {}
      (calc-change coins amount)
      (get-change coins amount)))

;; ---------------------------------------------------------------------------
;; Section 4: Comparing the solutions

(def coins [1 2 5 10 20 50 100 200])

(def solutions [#'make-change-chouser
                #'make-change-bgrabow
                #'make-change-jclaggett

                #'make-change-chouser-optimized
                #'make-change-bgrabow-optimized
                #'make-change-jclaggett-optimized

                #'make-change-chouser-memoized
                #'make-change-bgrabow-memoized
                #'make-change-jclaggett-memoized

                #'make-change-updater])

(defn make-change-all [coins amount]
  (into {}
        (for [s solutions]
          [(-> s meta :name) (s coins amount)])))

(defn bench-all [coins amount]
  (into {}
        (for [s solutions]
          [(-> s meta :name)
           (crit/quick-benchmark
             (s coins amount)
             {:target-execution-time (* 1000 1000 1000)})])))

