(ns nonfarmpayrollrevisions.core
  (:require [nonfarmpayrollrevisions.fred :as f]
            [nonfarmpayrollrevisions.bls :as b]
            [nonfarmpayrollrevisions.calcdif :as c]))

(defn -main []
  (println (c/f-this-month b/f-bls-labor-report f/f-fred-labor-report)))

;(-main)
