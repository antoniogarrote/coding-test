(ns coding-test.core
  (:use [coding-test.price-calculator :only [run-plan]]))

(defn price-for
  ([consume plan]
     (let [invoice (run-plan consume plan)]
       (println invoice)
       (:total invoice))))


;; a sample plan

;; (def *plan*
;;   {"supplier_name"  "Scottish Hydro Electric"
;;    "name"  "better plan"
;;    "annual_standing_charge"  10000
;;    "discounts" [
;;                   {
;;                    "applies_to"  "whole-bill"
;;                    "value"  10
;;                    "value_type"  "percentage"
;;                    "cap"  nil
;;                    }
;;                   {
;;                    "applies_to"  "rates"
;;                    "value"  0.6
;;                    "value_type"  "tier-2"
;;                    "cap"  50
;;                    }
;;                   {
;;                    "applies_to"  "rates"
;;                    "value"  1.0
;;                    "value_type"  "tier-2"
;;                    "cap"  30
;;                    }
;;                   ]
;;    "rates"  [
;;               {
;;                "price"  3.5
;;                "threshold"  500
;;                }
;;               {
;;                "price"  3.0
;;                "threshold"  nil
;;                }
;;               ]
;;    })
;;
;; (price-for 600 *plan*)
