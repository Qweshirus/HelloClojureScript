(ns cli.core)

;; --- Тарифы ---

(def ^:private base-rate 200)              ;; Базовая ставка
(def ^:private price-per-km-short 10)      ;; Цена за км до 100 км
(def ^:private price-per-km-long 5)        ;; Цена за км после 100 км
(def ^:private km-threshold 100)           ;; Порог расстояния

(def ^:private weight-tiers
  "Прогрессивные тарифы по весу: [макс-вес цена-за-кг]"
  [[1 100]    ;; до 1 кг — 100 за кг
   [5 50]     ;; 1-5 кг — 50 за кг
   [20 30]    ;; 5-20 кг — 30 за кг
   [nil 20]]) ;; более 20 кг — 20 за кг

(def ^:private delivery-multipliers
  "Коэффициенты типа доставки"
  {"standard" 1.0
   "express"  1.5
   "overnight" 2.0})

(def ^:private fragile-surcharge 0.20)     ;; +20% за хрупкий груз
(def ^:private insurance-rate 0.02)        ;; 2% от стоимости товара

;; --- Вычисление стоимости ---

(defn calculate-weight-cost [weight]
  "Вычисляет стоимость доставки по весу с прогрессивной тарификацией."
  (loop [remaining weight
         prev-tier-max 0
         tiers weight-tiers
         total 0]
    (if (or (<= remaining 0) (empty? tiers))
      total
      (let [[tier-max price-per-kg] (first tiers)
            effective-max (or tier-max weight)
            tier-width (- effective-max prev-tier-max)
            weight-in-tier (min remaining tier-width)
            cost (* weight-in-tier price-per-kg)]
        (recur (- remaining weight-in-tier)
               effective-max
               (rest tiers)
               (+ total cost))))))

(defn calculate-distance-cost [distance]
  "Вычисляет стоимость доставки по расстоянию."
  (if (<= distance km-threshold)
    (* distance price-per-km-short)
    (+ (* km-threshold price-per-km-short)
       (* (- distance km-threshold) price-per-km-long))))

(defn calculate-delivery-cost [weight distance delivery-type fragile? insurance-value]
  "Вычисляет полную стоимость доставки."
  (let [weight-cost (calculate-weight-cost weight)
        distance-cost (calculate-distance-cost distance)
        subtotal (+ base-rate weight-cost distance-cost)
        multiplier (get delivery-multipliers delivery-type 1.0)
        after-type (* subtotal multiplier)
        after-fragile (if fragile?
                        (* after-type (+ 1 fragile-surcharge))
                        after-type)
        insurance-cost (if (pos? insurance-value)
                         (* insurance-value insurance-rate)
                         0)
        total (+ after-fragile insurance-cost)]
    {:base-rate base-rate
     :weight-cost weight-cost
     :distance-cost distance-cost
     :subtotal subtotal
     :multiplier multiplier
     :after-type after-type
     :fragile-cost (if fragile? (* after-type fragile-surcharge) 0)
     :after-fragile after-fragile
     :insurance-cost insurance-cost
     :total total}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки."
  (loop [remaining args
         result {:weight nil :distance nil :type nil
                 :fragile false :insurance nil}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--weight")
          (recur (rest remaining) result :weight)

          (= current "--distance")
          (recur (rest remaining) result :distance)

          (= current "--type")
          (recur (rest remaining) result :type)

          (= current "--insurance")
          (recur (rest remaining) result :insurance)

          (= current "--fragile")
          (recur (rest remaining) (assoc result :fragile true) nil)

          :else
          (if current-flag
            (recur (rest remaining)
                   (assoc result current-flag current)
                   nil)
            (recur (rest remaining) result nil)))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-positive-number [s]
  (let [n (js/Number s)]
    (if (or (js/isNaN n) (<= n 0))
      {:ok false :raw s :reason "must be a positive number"}
      {:ok true :value n})))

(defn parse-non-negative-number [s]
  (let [n (js/Number s)]
    (if (or (js/isNaN n) (< n 0))
      {:ok false :raw s :reason "must be a non-negative number"}
      {:ok true :value n})))

(defn parse-delivery-type [s]
  (if (contains? delivery-multipliers s)
    {:ok true :value s}
    {:ok false :raw s :reason (str "must be one of: " (clojure.string/join ", " (keys delivery-multipliers)))}))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --weight <kg> --distance <km> [options]")
  (println)
  (println "Calculates delivery cost with progressive pricing.")
  (println)
  (println "Required options:")
  (println "  --weight <kg>         Cargo weight in kilograms (positive number)")
  (println "  --distance <km>       Delivery distance in kilometers (positive number)")
  (println)
  (println "Optional:")
  (println "  --type <type>         Delivery type: standard, express, overnight")
  (println "                        (default: standard)")
  (println "  --fragile             Mark cargo as fragile (+20%)")
  (println "  --insurance <amount>  Cargo value for insurance (2% of value)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --weight 5 --distance 50")
  (println "  node target/main.js --weight 15 --distance 200 --type express")
  (println "  node target/main.js --weight 3 --distance 30 --fragile --insurance 10000"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

(defn fmt [n]
  "Форматирует число с 2 знаками и разделителями тысяч."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        weight-str (:weight parsed)
        distance-str (:distance parsed)
        type-str (or (:type parsed) "standard")
        fragile? (:fragile parsed)
        insurance-str (:insurance parsed)]
    
    (cond
      ;; Обязательные параметры не указаны
      (or (nil? weight-str) (nil? distance-str))
      (do
        (println "Error: --weight and --distance are required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [weight-parsed (parse-positive-number weight-str)
            distance-parsed (parse-positive-number distance-str)
            type-parsed (parse-delivery-type type-str)
            insurance-parsed (if insurance-str
                               (parse-non-negative-number insurance-str)
                               {:ok true :value 0})]
        
        (cond
          (not (:ok weight-parsed))
          (do
            (println (str "Error: Invalid weight '" (:raw weight-parsed) "' — " (:reason weight-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok distance-parsed))
          (do
            (println (str "Error: Invalid distance '" (:raw distance-parsed) "' — " (:reason distance-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok type-parsed))
          (do
            (println (str "Error: Invalid delivery type '" (:raw type-parsed) "' — " (:reason type-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok insurance-parsed))
          (do
            (println (str "Error: Invalid insurance value '" (:raw insurance-parsed) "' — " (:reason insurance-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          :else
          (let [weight (:value weight-parsed)
                distance (:value distance-parsed)
                delivery-type (:value type-parsed)
                insurance-value (:value insurance-parsed)
                result (calculate-delivery-cost weight distance delivery-type fragile? insurance-value)]
            
            (println "=== Delivery Cost Calculator ===")
            (println)
            (println "--- Cargo details ---")
            (println (str "Weight:            " (fmt weight) " kg"))
            (println (str "Distance:          " (fmt distance) " km"))
            (println (str "Delivery type:     " delivery-type
                         (when (not= delivery-type "standard")
                           (str " (×" (:multiplier result) ")"))))
            (println (str "Fragile:           " (if fragile? "Yes (+20%)" "No")))
            (when (pos? insurance-value)
              (println (str "Insured value:     " (fmt insurance-value) " (2%)")))
            (println)
            
            (println "--- Cost breakdown ---")
            (println (str (pad-left "Base rate:" 22) " " (fmt (:base-rate result))))
            (println (str (pad-left "Weight cost:" 22) " " (fmt (:weight-cost result))))
            (println (str (pad-left "Distance cost:" 22) " " (fmt (:distance-cost result))))
            (println (str (pad-left "Subtotal:" 22) " " (fmt (:subtotal result))))
            (when (not= delivery-type "standard")
              (println (str (pad-left "Delivery surcharge:" 22) " "
                           (fmt (- (:after-type result) (:subtotal result))))))
            (when fragile?
              (println (str (pad-left "Fragile surcharge:" 22) " " (fmt (:fragile-cost result)))))
            (when (pos? insurance-value)
              (println (str (pad-left "Insurance:" 22) " " (fmt (:insurance-cost result)))))
            (println (apply str (repeat 35 "-")))
            (println (str (pad-left "TOTAL:" 22) " " (fmt (:total result))))
            
            (js/process.exit 0)))))))
