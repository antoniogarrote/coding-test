(ns coding-test.price-calculator)

;; types

(defrecord Invoice [consume total subtotals state])


;; utils

(defn- initial-invoice-for-consume
  "Builds an initial invoice for a certain consume"
  ([consume] (Invoice. consume 0 {} {})))

(defn- add-invoice-subtotal
  "Adds a subtotal to the invoice"
  ([invoice subtotal-key subtotal-value]
     (assoc-in invoice [:subtotals subtotal-key] subtotal-value)))

(defn- update-invoice-subtotal
  "Increases a subtotal of the invoice. If the subtotal is currently nil, 0 is taken
   as the initial value"
  ([invoice subtotal-key subtotal-value]
     (update-in invoice [:subtotals subtotal-key]
                (fn [old] (+ (or old 0) subtotal-value)))))

(defn- read-state
  "Returns the state hash for the invoice builder"
  ([invoice key]
     (read-state invoice key nil))
  ([invoice key default]
     (get-in invoice [:state key] default)))

(defn- write-state
  "Updates the value of a key in the state hash for the invoice builder"
  ([invoice key value]
     (update-in invoice [:state key] (fn [_] value))))

(defn- update-total
  "Updates the total value for an invoice"
  ([invoice inc-total]
     (update-in invoice [:total] (fn [old-total] (+ old-total inc-total)))))

(defn- make-token
  "Builds a new token for the plan parser"
  ([k v]
     {:kind (keyword k)
      :content v}))

(defn- invoice-subtotal
  "Different ways of aggregating the subtotals"
  ([invoice subtotal]
     (condp = (keyword subtotal)
         :whole-bill (let [subtotals (:subtotals invoice)]
                        (+ (:tier-1 subtotals) (:tier-2 subtotals) (:annual_standing_charge subtotals)))
         :rates      (let [subtotals (:subtotals invoice)]
                        (+ (:tier-1 subtotals) (:tier-2 subtotals)))
         ((keyword subtotal) (read-state invoice :subtotals)))))


;; plan parsers


; Function used to combine parsers.
; By default we are using plain function composition
(def combinator comp)

;; Common interface for all the parsers
(defmulti parse (fn [token] (:kind token)))


;;; Parser implementations

;; New parsers could be added for additional components
;; of a plan

(defmethod parse :annual_standing_charge
  ([token] (fn [invoice]
             (let [fee (:content token)
                   total (:total invoice)]
               (-> invoice
                   (update-total (+ total fee))
                   (add-invoice-subtotal (:kind token) fee))))))

(defmethod parse :rates
  ([token]
     (let [add-tier-to-rates (fn [rates]
                               (for [i (range 0 (count rates))]
                                 (let [rate (nth rates i)]
                                   (assoc rate :tier (str "tier-" (inc i))))))

           rates-fs (->> (:content token)
                         (sort #(compare (get %1 "threshold") (get %2 "threshold")))
                         (add-tier-to-rates)
                         (map (partial make-token "rate"))
                         (map parse)
                         (apply combinator))]
       (fn [invoice] (rates-fs invoice)))))

(defmethod parse :rate
  ([token] (let [rate (:content token)
                 price (get rate "price")
                 threshold (or (get rate "threshold") 0)]
             (fn [invoice]
               (let [remaining-rate-consume (read-state invoice :remaining-rate-consume (:consume invoice))
                     consumed (if (> remaining-rate-consume threshold) (- remaining-rate-consume threshold) 0)
                     to-pay (* consumed price)]
                 (-> invoice
                     (update-total to-pay)
                     (write-state :remaining-rate-consume (- remaining-rate-consume consumed))
                     (write-state (keyword (str "rate-" (:tier rate))) price)
                     (add-invoice-subtotal (keyword (:tier rate)) to-pay)))))))


(defmethod parse :discount
  ([token] (let [check-cap (fn [discount subtotal]
                             (let [cap (get discount "cap")]
                               (if (nil? cap) subtotal
                                   (if (> (Math/abs subtotal) cap) (- cap) subtotal))))

                 discount (:content token)
                 applied-to (get discount "applies_to")
                 value (get discount "value")
                 value-type (get discount "value_type")]
             (condp = value-type
                 "percentage" (fn [invoice] (let [subtotal (invoice-subtotal invoice applied-to)
                                                 to-discount (- (/ (* subtotal value) 100))
                                                 to-discount (check-cap discount to-discount)]
                                             (-> invoice
                                                 (update-total to-discount)
                                                 (update-invoice-subtotal :discounts to-discount))))
                 "tier-2"     (fn [invoice] (let [rate (read-state invoice :rate-tier-2)
                                                 subtotal (invoice-subtotal invoice :rates)
                                                 effective-rate (* rate value)
                                                 to-discount (- (* effective-rate subtotal))
                                                 to-discount (check-cap discount to-discount)]
                                             (-> invoice
                                                (update-total to-discount)
                                                (update-invoice-subtotal :discounts to-discount))))))))

(defmethod parse :discounts
  ([token]  (let [discounts-fs (->> (:content token)
                                    (map (partial make-token "discount"))
                                    (map parse)
                                    (apply combinator))]
              (fn [invoice] (discounts-fs invoice)))))


(defmethod parse :default
  ([token]  identity))


;; Run plan parser and invoice builder

(defn- make-invoice-builder
  "Generates an invoice-builder function parsing a provided plan"
  ([plan]
     (let [plan-tokens (map (fn [k] (make-token k (get plan k)))
                            ["annual_standing_charge"
                             "rates"
                             "discounts"])
           build-invoice-fn (->> plan-tokens
                                 (map parse)
                                 (reverse)
                                 (apply combinator))]
       (fn [consume] (build-invoice-fn consume)))))


(defn run-plan
  "Generates an invoice-builder and runs it on an invoice initiated with
   a certain consume"
  ([consume plan]
     ((make-invoice-builder plan) (initial-invoice-for-consume consume))))
