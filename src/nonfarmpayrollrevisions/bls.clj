(ns nonfarmpayrollrevisions.bls
  (:require [pl.danieljanus.tagsoup :as ts]
            [net.cgrand.enlive-html :as html]
            [date-clj :as dc]))


(def my-years
  (->> (dc/date)
       dc/year
       inc
       (range 2010)))

(defn my-months [year]
  (let [my-format (java.text.SimpleDateFormat. "MMddyyyy")
        my-date  (java.text.SimpleDateFormat. "MMddyyyy")]
    (as-> (range 12) %
          (map #(dc/date :week-day :friday :month % :year year) %)
          (flatten %)
          (map #(.format my-format %) %)
          (map #(case %
                  "04082011" "04012011"
                  "03022012" "03092012"
                  "06082012" "06012012"
                  "02082013" "02012013"
                  "10042013" "10222013"
                  "01032014" "01102014"
                  "07042014" "07032014"
                  "08082014" "08012014"
                  "01022015" "01092015"
                  "07032015" "07022015"
                  %)
               %)
          (remove #(dc/after? (.parse my-date %) (dc/date)) %)
        )))

(def f-fridays
  (->> (map my-months my-years)
      flatten
      (drop 1)))

(defn get-bls-labor-report [year]
  (as-> (ts/parse-xml (str "http://www.bls.gov/news.release/archives/empsit_" year ".htm")) %
        (html/select % [:table#ces_table10 :span.datavalue])
        (map :content %)
        (nth % 3)
        (first %)
        ))

(def f-bls-labor-report
  (->> f-fridays
       (map #(try
               [% (Integer/parseInt (get-bls-labor-report %))]
               (catch Exception e (println (str "caught exception: " %)))) )
       (remove nil? )))

