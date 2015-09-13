(ns nonfarmpayrollrevisions.core
  (:require [pl.danieljanus.tagsoup :as ts]
            [net.cgrand.enlive-html :as html]
            [date-clj :as dc]
            [nonfarmpayrollrevisions.fred :as f]
            [nonfarmpayrollrevisions.bls :as b]))

(defn catch-december [month]
  (if (= 0 month)
    (+ 12 month)
    month)
  )

(defn convert-month [number]
  (case number
    1 :jan
    2 :feb
    3 :mar
    4 :apr
    5 :may
    6 :jun
    7 :jul
    8 :aug
    9 :sep
    10 :oct
    11 :nov
    12 :dec
    )
  )

(defn labor-reports-diff [bls-labor-report fred-labor-report]
  "
  [[1 -13] [2 10]] [[1 23] [2 20]] => [[:jan 10] [:feb 10]]
  "
  (as-> (map #(hash-map (convert-month (first %1))
                        [(- (second %2) (second %1))]) bls-labor-report fred-labor-report) %
        (butlast %)
        ))

(defn abs-mean [coll]
  (let [sum (apply + (map #(Math/abs %) coll))
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn f-monthly-avg-revision [diff-reports]
  (as-> diff-reports %
        (apply merge-with concat %)
        (map #(hash-map (first %) (float (abs-mean (second %)))) %)
        (apply merge %)
        ))

(defn f-this-month [bls-labor-report fred-labor-report]
  (let [diff-reports (labor-reports-diff bls-labor-report fred-labor-report)
        next-month-key (as-> (dc/date) %
                             (dc/month %)
                             (inc %) ;want abs avg revisions for next month
                             (convert-month %))
        monthly-revision (f-monthly-avg-revision diff-reports)
        next-avg-revision (next-month-key monthly-revision)
        rank (as-> (sort-by val > monthly-revision) %
                   (map #((comp str first) %) %)
                   (.indexOf % (str next-month-key))
                   )
        previous-quarter (-> (- (count diff-reports) 3)
                             (drop diff-reports)
                             )]
    [next-month-key :avg next-avg-revision :rank rank :previous-quarter previous-quarter]
    ))

(defn -main []
  (println (f-this-month b/f-bls-labor-report f/f-fred-labor-report)))

;(-main)
