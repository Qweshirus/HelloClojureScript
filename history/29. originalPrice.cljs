(ns cli.core)

;; --- Вычисление ---

(defn calculate-original-price [final-price tax-percent]
  "Восстанавливает исходную цену товара без налога.
   Формула: original = final / (1 + tax/100)
   Возвращает карту с результатами."
  (let [tax-multiplier (+ 1 (/ tax-percent 100))
        original-price (/ final-price tax-multiplier)
        tax-amount (- final-price original-price)]
    {:original-price original-price
     :tax-amount tax-amount
     :tax-multiplier tax-multiplier}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флага --tax,
   а также неименованные параметры (финальная цена)."
  (loop [remaining args
         result {:tax nil :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--tax")
          (recur (rest remaining) result :tax)

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
  (println "Usage: node target/main.js [--tax <percent>] <final-price>")
  (println)
  (println "Recovers the original price without tax from the final price with tax.")
  (println "Formula: original = final / (1 + tax/100)")
  (println)
  (println "Options:")
  (println "  --tax <percent>      Tax percentage (0-100, optional, default: 20)")
  (println)
  (println "Arguments:")
  (println "  <final-price>        Final price with tax included (positive number, required)")
  (println)
  (println "Note: Final price can be placed anywhere in the command line.")
  (println)
  (println "Examples:")
  (println "  node target/main.js 1200")
  (println "  node target/main.js --tax 18 1180")
  (println "  node target/main.js 1000 --tax 10"))

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
        tax-str (:tax parsed)
        positional (:positional parsed)
        final-price-str (first positional)]
    
    (cond
      ;; Финальная цена не указана
      (nil? final-price-str)
      (do
        (println "Error: Final price is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много неименованных параметров
      (> (count positional) 1)
      (do
        (println "Error: Only one final price argument is allowed.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [;; Значение по умолчанию для налога: 20%
            tax-val (or tax-str "20")
            
            tax-parsed (parse-percentage tax-val)
            final-price-parsed (parse-positive-number final-price-str)]
        
        (cond
          ;; Ошибка в налоге
          (not (:ok tax-parsed))
          (do
            (println (str "Error: Invalid tax '" (:raw tax-parsed) "' — " (:reason tax-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в финальной цене
          (not (:ok final-price-parsed))
          (do
            (println (str "Error: Invalid final price '" (:raw final-price-parsed) "' — " (:reason final-price-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [final-price (:value final-price-parsed)
                tax-percent (:value tax-parsed)
                result (calculate-original-price final-price tax-percent)]
            
            (println "=== Price Recovery Calculator ===")
            (println)
            (println (str (pad-left "Final price (with tax):" 28) " " (fmt final-price)))
            (println (str (pad-left "Tax rate:" 28) " " tax-percent "%"))
            (println (str (pad-left "Tax multiplier:" 28) " " (:tax-multiplier result)))
            (println)
            (println "--- Breakdown ---")
            (println (str (pad-left "Original price (without tax):" 28) " " (fmt (:original-price result))))
            (println (str (pad-left "Tax amount:" 28) " " (fmt (:tax-amount result))))
            (println)
            (println "--- Verification ---")
            (println (str (pad-left "Original + Tax =" 28) " " 
                         (fmt (:original-price result)) " + " 
                         (fmt (:tax-amount result)) " = " 
                         (fmt final-price)))
            
            (js/process.exit 0)))))))
