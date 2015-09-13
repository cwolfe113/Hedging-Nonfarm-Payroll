(ns nonfarmpayrollrevisions.core
  (:require [pl.danieljanus.tagsoup :as ts]
            [net.cgrand.enlive-html :as html]
            [date-clj :as dc]
            [nonfarmpayrollrevisions.fred :as f]
            [nonfarmpayrollrevisions.bls :as b]))

(defn convert-month [number]
  (case number
    0 :jan
    1 :feb
    2 :mar
    3 :apr
    4 :may
    5 :jun
    6 :jul
    7 :aug
    8 :sep
    9 :oct
    10 :nov
    11 :dec
    )
  )

(defn labor-reports-diff [bls-labor-report fred-labor-report month]
  (as-> ((fn merging [bls-labor-report fred-labor-report month]
           (let [bls (second (first bls-labor-report))
                 fred (first fred-labor-report)
                 this-month (convert-month month)
                 next-month (if (= 12 (inc month))
                              0
                              (inc month))]
             (if (empty? (rest bls-labor-report))
               (hash-map this-month [(- fred bls)])
               (cons (hash-map this-month [(- fred bls)]) (merging (rest bls-labor-report) (rest fred-labor-report) next-month))
               ))) bls-labor-report fred-labor-report month) %
        (butlast %)
        ))

(defn labor-reports-merge [bls-labor-report fred-labor-report month]
  (->> (labor-reports-diff bls-labor-report fred-labor-report month)
      (apply merge-with concat ))
  )

(defn past-year-revisions [bls-labor-report fred-labor-report month]
  (let [past-months (labor-reports-diff bls-labor-report fred-labor-report month)]
    (-> (count past-months)
        (- 3)
        (drop past-months)
        )
    )
  )

(defn abs-mean [coll]
  (let [sum (apply + (map #(Math/abs %) coll))
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn f-monthly-avg-revision [bls-labor-report fred-labor-report]
  (as-> (labor-reports-merge bls-labor-report fred-labor-report 0) %
        (map #(hash-map (first %) (float (abs-mean (second %)))) %)
        (apply merge-with concat %)
        ))

(defn rank-in-months [coll month]
  (if (= month (ffirst coll))
    (second (first coll))
    (rank-in-months (rest coll) month)
    )
  )

(defn f-this-month [bls-labor-report fred-labor-report]
  (let [bls (second (last bls-labor-report))
        next-month (as-> (dc/date) %
                         (dc/month %)
                         ;(inc %) ;want abs avg revisions for next month
                         (convert-month %))
        monthly-revision (f-monthly-avg-revision bls-labor-report fred-labor-report)
        this-avg-revision (next-month monthly-revision)
        rank (as-> (sort-by val > monthly-revision) %
                   (map first %)
                   (interleave % (range))
                   (partition 2 %)
                   (rank-in-months % next-month)
                   )
        past-quarter (past-year-revisions bls-labor-report fred-labor-report 0)]
    [next-month :avg this-avg-revision :rank rank :past-quarter past-quarter]
    ))

(defn -main []
  (println (f-this-month b/f-bls-labor-report f/f-fred-labor-report)))

;(-main)
