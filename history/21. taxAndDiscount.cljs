(ns cli.core)

;; --- Вычисление финальной цены ---

(defn calculate-final-price [price discount-percent tax-percent]
  "Вычисляет финальную цену с учётом скидки и налога.
   Сначала применяется скидка, затем налог к цене со скидкой."
  (let [price-after-discount (* price (- 1 (/ discount-percent 100)))
        final-price (* price-after-discount (+ 1 (/ tax-percent 100)))]
    {:price-after-discount price-after-discount
     :final-price final-price}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --discount и --tax,
   а также неименованные параметры (цена)."
  (loop [remaining args
         result {:discount nil :tax nil :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--discount")
          (recur (rest remaining) result :discount)

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
  (println "Usage: node target/main.js [--discount <percent>] [--tax <percent>] <price>")
  (println)
  (println "Calculates final price with optional discount and tax applied.")
  (println)
  (println "Options:")
  (println "  --discount <percent>    Discount percentage (0-100, optional, default: 0)")
  (println "  --tax <percent>         Tax percentage (0-100, optional, default: 0)")
  (println)
  (println "Arguments:")
  (println "  <price>                 Original price (positive number, required)")
  (println)
  (println "Note: Price can be placed anywhere in the command line.")
  (println)
  (println "Examples:")
  (println "  node target/main.js 100")
  (println "  node target/main.js --discount 10 100")
  (println "  node target/main.js 100 --discount 10")
  (println "  node target/main.js --discount 10 --tax 20 100")
  (println "  node target/main.js 100 --discount 10 --tax 20")
  (println "  node target/main.js --discount 10 100 --tax 20"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        discount-str (:discount parsed)
        tax-str (:tax parsed)
        positional (:positional parsed)
        price-str (first positional)]
    
    (cond
      ;; Цена не указана
      (nil? price-str)
      (do
        (println "Error: Price argument is required.")
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
      (let [;; Значения по умолчанию: 0% для скидки и налога
            discount-val (or discount-str "0")
            tax-val (or tax-str "0")
            
            discount-parsed (parse-percentage discount-val)
            tax-parsed (parse-percentage tax-val)
            price-parsed (parse-positive-number price-str)]
        
        (cond
          ;; Ошибка в скидке
          (not (:ok discount-parsed))
          (do
            (println (str "Error: Invalid discount '" (:raw discount-parsed) "' — " (:reason discount-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в налоге
          (not (:ok tax-parsed))
          (do
            (println (str "Error: Invalid tax '" (:raw tax-parsed) "' — " (:reason tax-parsed)))
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
                discount (:value discount-parsed)
                tax (:value tax-parsed)
                result (calculate-final-price price discount tax)]
            (println (str "Original price:       " (round2 price)))
            (println (str "Discount:             " discount "%"))
            (println (str "Price after discount: " (round2 (:price-after-discount result))))
            (println (str "Tax:                  " tax "%"))
            (println (str "Final price:          " (round2 (:final-price result))))
            (js/process.exit 0)))))))
