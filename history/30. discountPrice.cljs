(ns cli.core)

;; --- Вычисление ---

(defn calculate-discount [price discount-percent]
  "Вычисляет сумму скидки и итоговую цену.
   Возвращает карту с результатами."
  (let [discount-amount (* price (/ discount-percent 100))
        final-price (- price discount-amount)]
    {:discount-amount discount-amount
     :final-price final-price}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флага --discount,
   а также неименованные параметры (цена)."
  (loop [remaining args
         result {:discount nil :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--discount")
          (recur (rest remaining) result :discount)

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
  (println "Usage: node target/main.js [--discount <percent>] <price>")
  (println)
  (println "Calculates discount amount and final price.")
  (println)
  (println "Options:")
  (println "  --discount <percent>   Discount percentage (0-100, optional, default: 0)")
  (println)
  (println "Arguments:")
  (println "  <price>                Original price (positive number, required)")
  (println)
  (println "Note: Price can be placed anywhere in the command line.")
  (println)
  (println "Examples:")
  (println "  node target/main.js 1000")
  (println "  node target/main.js --discount 20 1000")
  (println "  node target/main.js 1000 --discount 15"))

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
        discount-str (:discount parsed)
        positional (:positional parsed)
        price-str (first positional)]
    
    (cond
      ;; Цена не указана
      (nil? price-str)
      (do
        (println "Error: Price is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много неименованных параметров
      (> (count positional) 1)
      (do
        (println "Error: Only one price argument is allowed.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [;; Значение по умолчанию для скидки: 0%
            discount-val (or discount-str "0")
            
            discount-parsed (parse-percentage discount-val)
            price-parsed (parse-positive-number price-str)]
        
        (cond
          ;; Ошибка в скидке
          (not (:ok discount-parsed))
          (do
            (println (str "Error: Invalid discount '" (:raw discount-parsed) "' — " (:reason discount-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в цене
          (not (:ok price-parsed))
          (do
            (println (str "Error: Invalid price '" (:raw price-parsed) "' — " (:reason price-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [price (:value price-parsed)
                discount-percent (:value discount-parsed)
                result (calculate-discount price discount-percent)]
            
            (println "=== Discount Calculator ===")
            (println)
            (println (str (pad-left "Original price:" 20) " " (fmt price)))
            (println (str (pad-left "Discount:" 20) " " discount-percent "%"))
            (println)
            (println "--- Breakdown ---")
            (println (str (pad-left "Discount amount:" 20) " " (fmt (:discount-amount result))))
            (println (str (pad-left "Final price:" 20) " " (fmt (:final-price result))))
            (println)
            (println "--- Verification ---")
            (println (str (pad-left "Original - Discount =" 20) " " 
                         (fmt price) " - " 
                         (fmt (:discount-amount result)) " = " 
                         (fmt (:final-price result))))
            
            (js/process.exit 0)))))))
