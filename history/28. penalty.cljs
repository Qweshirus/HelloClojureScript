(ns cli.core)

;; --- Шкала штрафов ---
;; Каждая запись: [мин-превышение макс-превышение штраф категория]

(def ^:private fine-tiers-first
  "Штрафы за первое нарушение."
  [[0   20   0     "No violation"]
   [20  40   500   "Minor"]
   [40  60   1500  "Moderate"]
   [60  80   2500  "Serious"]
   [80  nil 5000   "Severe"]])

(def ^:private fine-tiers-repeat
  "Штрафы за повторное нарушение (в течение года)."
  [[0   20   0      "No violation"]
   [20  40   1500   "Minor (repeat)"]
   [40  60   2500   "Moderate (repeat)"]
   [60  80   5000   "Serious (repeat)"]
   [80  nil 5000    "Severe (repeat)"]])

;; --- Вычисление штрафа ---

(defn find-tier [excess tiers]
  "Находит подходящую категорию нарушения по величине превышения."
  (->> tiers
       (filter (fn [[min-val max-val _ _]]
                 (and (>= excess min-val)
                      (or (nil? max-val) (< excess max-val)))))
       first))

(defn calculate-fine [limit speed repeat?]
  "Вычисляет штраф за превышение скорости.
   Возвращает карту с результатами."
  (let [excess (- speed limit)
        tiers (if repeat? fine-tiers-repeat fine-tiers-first)
        tier (find-tier excess tiers)
        fine-amount (if tier (nth tier 2) 0)
        category (if tier (nth tier 3) "Unknown")
        ;; Дополнительные санкции
        license-suspension? (and (>= excess 60) repeat?)
        max-fine? (and (>= excess 80) (not repeat?))]
    {:excess excess
     :fine fine-amount
     :category category
     :license-suspension? license-suspension?
     :max-fine? max-fine?}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки."
  (loop [remaining args
         result {:limit nil :speed nil :repeat false}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--limit")
          (recur (rest remaining) result :limit)

          (= current "--speed")
          (recur (rest remaining) result :speed)

          (= current "--repeat")
          (recur (rest remaining) (assoc result :repeat true) nil)

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

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --limit <km/h> --speed <km/h> [--repeat]")
  (println)
  (println "Calculates fine for speeding.")
  (println)
  (println "Required options:")
  (println "  --limit <km/h>    Speed limit (positive number)")
  (println "  --speed <km/h>    Actual speed (positive number)")
  (println)
  (println "Optional:")
  (println "  --repeat          Mark as repeat offense (within 1 year)")
  (println)
  (println "Fine schedule (first offense):")
  (println "   0-20 km/h over  — no fine")
  (println "  20-40 km/h over  — 500")
  (println "  40-60 km/h over  — 1500")
  (println "  60-80 km/h over  — 2500")
  (println "  80+   km/h over  — 5000")
  (println)
  (println "Examples:")
  (println "  node target/main.js --limit 60 --speed 75")
  (println "  node target/main.js --limit 90 --speed 180")
  (println "  node target/main.js --limit 60 --speed 100 --repeat"))

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
        limit-str (:limit parsed)
        speed-str (:speed parsed)
        repeat? (:repeat parsed)]
    
    (cond
      ;; Обязательные параметры не указаны
      (or (nil? limit-str) (nil? speed-str))
      (do
        (println "Error: --limit and --speed are required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [limit-parsed (parse-positive-number limit-str)
            speed-parsed (parse-positive-number speed-str)]
        
        (cond
          (not (:ok limit-parsed))
          (do
            (println (str "Error: Invalid limit '" (:raw limit-parsed) "' — " (:reason limit-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok speed-parsed))
          (do
            (println (str "Error: Invalid speed '" (:raw speed-parsed) "' — " (:reason speed-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          :else
          (let [limit (:value limit-parsed)
                speed (:value speed-parsed)
                result (calculate-fine limit speed repeat?)]
            
            (println "=== Speeding Fine Calculator ===")
            (println)
            (println "--- Details ---")
            (println (str (pad-left "Speed limit:" 20) " " (fmt limit) " km/h"))
            (println (str (pad-left "Actual speed:" 20) " " (fmt speed) " km/h"))
            (println (str (pad-left "Excess:" 20) " " (fmt (:excess result)) " km/h"))
            (println (str (pad-left "Repeat offense:" 20) " " (if repeat? "Yes" "No")))
            (println (str (pad-left "Category:" 20) " " (:category result)))
            (println)
            
            (println "--- Penalty ---")
            (if (zero? (:fine result))
              (println "No fine. Speeding is within the tolerance (up to 20 km/h).")
              (do
                (println (str (pad-left "Fine:" 20) " " (fmt (:fine result))))
                (when (:license-suspension? result)
                  (println (str (pad-left "Additional:" 20) " License suspension up to 1 year")))))
            
            (js/process.exit 0)))))))
