(ns nonfarmpayrollrevisions.fred)


(def get-fred-labor-report
  (as-> (slurp "https://research.stlouisfed.org/fred2/data/PAYEMS.txt") %
        (subs % (.indexOf % "2009-12-01"))
        (clojure.string/split % #"\r\n")
        (map #(clojure.string/split % #" ") %)
        (map last %)
        (map #(Integer/parseInt %) %)
        ))

(def f-fred-labor-report
  ((fn fred-labor-report [fred-data]
     (let [past (first fred-data)
           now (second fred-data)]
       (if (< 1 (count (rest fred-data)))
         (cons (- now past) (fred-labor-report (rest fred-data)))
         [(- now past)]
         ))) get-fred-labor-report))

