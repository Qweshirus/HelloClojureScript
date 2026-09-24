(ns cli.core)

;; --- Вычисление расхода топлива ---

(defn calculate-fuel-consumption [liters kilometers]
  "Вычисляет расход топлива в литрах на 100 км.
   Формула: (литры / километры) * 100"
  (let [consumption (* (/ liters kilometers) 100)]
    consumption))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --liters и --kilometers."
  (loop [remaining args
         result {:liters nil :kilometers nil}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--liters")
          (recur (rest remaining) result :liters)

          (= current "--kilometers")
          (recur (rest remaining) result :kilometers)

          :else
          (if current-flag
            (recur (rest remaining)
                   (assoc result current-flag current)
                   nil)
            (recur (rest remaining) result nil)))))))

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
  (println "Usage: node target/main.js --liters <liters> --kilometers <kilometers>")
  (println)
  (println "Calculates fuel consumption in liters per 100 km.")
  (println)
  (println "Options:")
  (println "  --liters <liters>        Fuel consumed in liters (positive number)")
  (println "  --kilometers <kilometers> Distance traveled in kilometers (positive number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --liters 50 --kilometers 500")
  (println "  node target/main.js --kilometers 1000 --liters 80")
  (println "  node target/main.js --liters 30.5 --kilometers 400"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        liters-str (:liters parsed)
        kilometers-str (:kilometers parsed)]
    
    (cond
      ;; Не все параметры указаны
      (or (nil? liters-str) (nil? kilometers-str))
      (do
        (println "Error: Both parameters are required: --liters and --kilometers.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [liters-parsed (parse-positive-number liters-str)
            kilometers-parsed (parse-positive-number kilometers-str)]
        
        (cond
          ;; Ошибка в литрах
          (not (:ok liters-parsed))
          (do
            (println (str "Error: Invalid liters '" (:raw liters-parsed) "' — " (:reason liters-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в километрах
          (not (:ok kilometers-parsed))
          (do
            (println (str "Error: Invalid kilometers '" (:raw kilometers-parsed) "' — " (:reason kilometers-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [liters (:value liters-parsed)
                kilometers (:value kilometers-parsed)
                consumption (calculate-fuel-consumption liters kilometers)]
            (println (str "Fuel consumed:    " (round2 liters) " liters"))
            (println (str "Distance:         " (round2 kilometers) " km"))
            (println (str "Consumption:      " (round2 consumption) " l/100km"))
            (js/process.exit 0)))))))
