(ns cli.core)

;; --- Вычисление простых процентов ---

(defn calculate-simple-interest [principal rate-percent years]
  "Вычисляет простые проценты.
   Формула: Interest = P × r × t
   где P — сумма, r — ставка (в долях), t — время в годах.
   Возвращает карту {:interest :total}."
  (let [rate (/ rate-percent 100)
        interest (* principal rate years)
        total (+ principal interest)]
    {:interest interest :total total}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --years и --rate,
   а также неименованные параметры (сумма)."
  (loop [remaining args
         result {:years nil :rate nil :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--years")
          (recur (rest remaining) result :years)

          (= current "--rate")
          (recur (rest remaining) result :rate)

          :else
          (if current-flag
            (recur (rest remaining)
                   (assoc result current-flag current)
                   nil)
            (recur (rest remaining)
                   (update result :positional conj current)
                   nil)))))))

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

(defn parse-percentage [s]
  "Пытается преобразовать строку в процент (число от 0 до 100)."
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a number"}
      (if (or (< n 0) (> n 100))
        {:ok false :raw s :reason "must be between 0 and 100"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js [--years <years>] [--rate <percent>] <principal>")
  (println)
  (println "Calculates simple interest.")
  (println)
  (println "Options:")
  (println "  --years <years>       Time in years (positive number, optional, default: 1)")
  (println "  --rate <percent>      Annual interest rate (0-100, optional, default: 0)")
  (println)
  (println "Arguments:")
  (println "  <principal>           Initial amount (positive number, required)")
  (println)
  (println "Note: Principal can be placed anywhere in the command line.")
  (println)
  (println "Examples:")
  (println "  node target/main.js 1000")
  (println "  node target/main.js --rate 5 1000")
  (println "  node target/main.js --years 3 --rate 5 1000")
  (println "  node target/main.js 1000 --years 3 --rate 5"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        years-str (:years parsed)
        rate-str (:rate parsed)
        positional (:positional parsed)
        principal-str (first positional)]
    
    (cond
      ;; Сумма не указана
      (nil? principal-str)
      (do
        (println "Error: Principal amount is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много неименованных параметров
      (> (count positional) 1)
      (do
        (println "Error: Only one principal argument is allowed.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [;; Значения по умолчанию
            years-val (or years-str "1")
            rate-val (or rate-str "0")
            
            years-parsed (parse-positive-number years-val)
            rate-parsed (parse-percentage rate-val)
            principal-parsed (parse-positive-number principal-str)]
        
        (cond
          ;; Ошибка в времени
          (not (:ok years-parsed))
          (do
            (println (str "Error: Invalid years '" (:raw years-parsed) "' — " (:reason years-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в ставке
          (not (:ok rate-parsed))
          (do
            (println (str "Error: Invalid rate '" (:raw rate-parsed) "' — " (:reason rate-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в сумме
          (not (:ok principal-parsed))
          (do
            (println (str "Error: Invalid principal '" (:raw principal-parsed) "' — " (:reason principal-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [principal (:value principal-parsed)
                rate (:value rate-parsed)
                years (:value years-parsed)
                result (calculate-simple-interest principal rate years)]
            (println (str "Principal:         " (round2 principal)))
            (println (str "Annual rate:       " rate "%"))
            (println (str "Time:              " (round2 years) " years"))
            (println (str "Interest earned:   " (round2 (:interest result))))
            (println (str "Total amount:      " (round2 (:total result))))
            (js/process.exit 0)))))))
