(ns cli.core)

;; --- Вычисление ROI ---

(defn calculate-roi [initial final]
  "Вычисляет окупаемость инвестиций (ROI).
   Формула: ROI = ((final - initial) / initial) × 100
   Возвращает карту с результатами."
  (let [profit (- final initial)
        roi (* (/ profit initial) 100)]
    {:profit profit
     :roi roi}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --initial и --final."
  (loop [remaining args
         result {:initial nil :final nil}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--initial")
          (recur (rest remaining) result :initial)

          (= current "--final")
          (recur (rest remaining) result :final)

          :else
          (recur (rest remaining) result nil))))))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-positive-number [s]
  "Пытается преобразовать строку в положительное число."
  (let [n (js/Number s)]
    (if (or (js/isNaN n) (<= n 0))
      {:ok false :raw s :reason "must be a positive number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --initial <amount> --final <amount>")
  (println)
  (println "Calculates Return on Investment (ROI).")
  (println "Formula: ROI = ((final - initial) / initial) × 100%")
  (println)
  (println "Options:")
  (println "  --initial <amount>    Initial investment amount (positive number)")
  (println "  --final <amount>      Final amount after investment (positive number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --initial 1000 --final 1200")
  (println "  node target/main.js --initial 5000 --final 4500")
  (println "  node target/main.js --initial 10000 --final 15000"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

(defn roi-label [roi]
  "Возвращает текстовую оценку ROI."
  (cond
    (> roi 0) "Profit"
    (< roi 0) "Loss"
    :else "Break-even"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        initial-str (:initial parsed)
        final-str (:final parsed)]
    
    (cond
      ;; Не все параметры указаны
      (or (nil? initial-str) (nil? final-str))
      (do
        (println "Error: Both parameters are required: --initial and --final.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [initial-parsed (parse-positive-number initial-str)
            final-parsed (parse-positive-number final-str)]
        
        (cond
          ;; Ошибка в начальной сумме
          (not (:ok initial-parsed))
          (do
            (println (str "Error: Invalid initial amount '" (:raw initial-parsed) "' — " (:reason initial-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в конечной сумме
          (not (:ok final-parsed))
          (do
            (println (str "Error: Invalid final amount '" (:raw final-parsed) "' — " (:reason final-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [initial (:value initial-parsed)
                final (:value final-parsed)
                result (calculate-roi initial final)
                roi (:roi result)
                profit (:profit result)]
            
            (println "=== Investment ROI Calculator ===")
            (println (str "Initial investment:  " (round2 initial)))
            (println (str "Final amount:        " (round2 final)))
            (println (str "Profit/Loss:         " (round2 profit)))
            (println (str "ROI:                 " (round2 roi) "%"))
            (println (str "Status:              " (roi-label roi)))
            
            (js/process.exit 0)))))))
