(ns cli.core)

;; --- Вычисление аннуитетного платежа ---

(defn calculate-monthly-payment [principal monthly-rate months]
  "Вычисляет ежемесячный аннуитетный платёж.
   Формула: PMT = P × r × (1+r)^n / ((1+r)^n - 1)
   где P — сумма, r — месячная ставка, n — количество месяцев."
  (if (zero? monthly-rate)
    (/ principal months)
    (let [pow (js/Math.pow (+ 1 monthly-rate) months)]
      (/ (* principal monthly-rate pow)
         (- pow 1)))))

;; --- Генерация графика платежей ---

(defn generate-schedule [principal annual-rate years]
  "Генерирует график ежемесячных платежей.
   Возвращает вектор карт {:month :payment :interest :principal :balance}."
  (let [monthly-rate (/ annual-rate 100 12)
        months (* years 12)
        payment (calculate-monthly-payment principal monthly-rate months)]
    (loop [month 1
           balance principal
           result []]
      (if (> month months)
        result
        (let [interest (* balance monthly-rate)
              principal-part (- payment interest)
              new-balance (max 0 (- balance principal-part))]
          (recur (inc month)
                 new-balance
                 (conj result {:month month
                               :payment payment
                               :interest interest
                               :principal principal-part
                               :balance new-balance})))))))

;; --- Форматирование ---

(defn fmt [n]
  "Форматирует число с 2 знаками после запятой и разделителями тысяч."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        ;; Добавляем пробелы как разделители тысяч
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

(defn pad-right [s width]
  "Дополняет строку пробелами справа до заданной ширины."
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str s (apply str (repeat (- width (count s)) " "))))))

(defn pad-left [s width]
  "Дополняет строку пробелами слева до заданной ширины."
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

(defn print-schedule [schedule principal annual-rate years]
  "Печатает график платежей в виде таблицы."
  (let [months (* years 12)
        monthly-rate (/ annual-rate 100 12)
        payment (:payment (first schedule))
        total-paid (* payment months)
        total-interest (- total-paid principal)]
    
    ;; Заголовок
    (println "=== Loan Amortization Schedule ===")
    (println (str "Principal:       " (fmt principal)))
    (println (str "Annual rate:     " annual-rate "%"))
    (println (str "Monthly rate:    " (.toFixed monthly-rate 4) "%"))
    (println (str "Term:            " years " years (" months " months)"))
    (println (str "Monthly payment: " (fmt payment)))
    (println (str "Total paid:      " (fmt total-paid)))
    (println (str "Total interest:  " (fmt total-interest)))
    (println)
    
    ;; Разделитель
    (let [separator (apply str (repeat 75 "-"))]
      (println separator)
      
      ;; Заголовки колонок
      (println (str (pad-left "Month" 6) " | "
                   (pad-left "Payment" 14) " | "
                   (pad-left "Interest" 14) " | "
                   (pad-left "Principal" 14) " | "
                   (pad-left "Balance" 14)))
      (println separator)
      
      ;; Строки графика
      (doseq [row schedule]
        (println (str (pad-left (:month row) 6) " | "
                     (pad-left (fmt (:payment row)) 14) " | "
                     (pad-left (fmt (:interest row)) 14) " | "
                     (pad-left (fmt (:principal row)) 14) " | "
                     (pad-left (fmt (:balance row)) 14))))
      
      (println separator)
      
      ;; Итоги
      (println (str (pad-left "Total" 6) " | "
                   (pad-left (fmt total-paid) 14) " | "
                   (pad-left (fmt total-interest) 14) " | "
                   (pad-left (fmt principal) 14) " | "
                   (pad-left "0.00" 14)))
      (println separator))))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки."
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

(defn parse-positive-int [s]
  (let [n (js/Number s)]
    (if (or (js/isNaN n)
            (not (js/Number.isInteger n))
            (<= n 0))
      {:ok false :raw s :reason "must be a positive integer"}
      {:ok true :value n})))

(defn parse-percentage [s]
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a number"}
      (if (or (< n 0) (> n 100))
        {:ok false :raw s :reason "must be between 0 and 100"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js [--years <y>] [--rate <percent>] <principal>")
  (println)
  (println "Prints monthly payment schedule with monthly interest compounding.")
  (println)
  (println "Options:")
  (println "  --years <years>    Loan term in years (positive integer, optional, default: 1)")
  (println "  --rate <percent>   Annual interest rate (0-100, optional, default: 0)")
  (println)
  (println "Arguments:")
  (println "  <principal>        Loan amount (positive number, required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --years 2 --rate 12 100000")
  (println "  node target/main.js 500000 --years 5 --rate 10"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        years-str (:years parsed)
        rate-str (:rate parsed)
        positional (:positional parsed)
        principal-str (first positional)]
    
    (cond
      (nil? principal-str)
      (do
        (println "Error: Principal amount is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      (> (count positional) 1)
      (do
        (println "Error: Only one principal argument is allowed.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [years-val (or years-str "1")
            rate-val (or rate-str "0")
            
            years-parsed (parse-positive-int years-val)
            rate-parsed (parse-percentage rate-val)
            principal-parsed (parse-positive-number principal-str)]
        
        (cond
          (not (:ok years-parsed))
          (do
            (println (str "Error: Invalid years '" (:raw years-parsed) "' — " (:reason years-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok rate-parsed))
          (do
            (println (str "Error: Invalid rate '" (:raw rate-parsed) "' — " (:reason rate-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok principal-parsed))
          (do
            (println (str "Error: Invalid principal '" (:raw principal-parsed) "' — " (:reason principal-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          :else
          (let [principal (:value principal-parsed)
                rate (:value rate-parsed)
                years (:value years-parsed)
                schedule (generate-schedule principal rate years)]
            (print-schedule schedule principal rate years)
            (js/process.exit 0)))))))
