(ns cli.core)

;; --- Ограничение числа ---

(defn clamp [value min-val max-val]
  "Ограничивает число в диапазоне [min, max].
   Если min или max не указаны (nil), соответствующее ограничение не применяется.
   Возвращает карту с результатами."
  (let [clamped (cond
                  (and min-val (< value min-val)) min-val
                  (and max-val (> value max-val)) max-val
                  :else value)
        status (cond
                 (and min-val (< value min-val)) "below min"
                 (and max-val (> value max-val)) "above max"
                 (or min-val max-val) "within range"
                 :else "no limits applied")]
    {:clamped clamped
     :status status}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --min и --max,
   а также неименованные параметры (число)."
  (loop [remaining args
         result {:min nil :max nil :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--min")
          (recur (rest remaining) result :min)

          (= current "--max")
          (recur (rest remaining) result :max)

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

(defn parse-number [s]
  "Пытается преобразовать строку в число."
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js [--min <value>] [--max <value>] <number>")
  (println)
  (println "Clamps a number to the specified range.")
  (println "Both --min and --max are optional. If omitted, the corresponding")
  (println "limit is not applied.")
  (println)
  (println "Options:")
  (println "  --min <value>    Minimum allowed value (optional)")
  (println "  --max <value>    Maximum allowed value (optional, must be >= min)")
  (println)
  (println "Arguments:")
  (println "  <number>         The number to clamp (required)")
  (println)
  (println "Note: Number can be placed anywhere in the command line.")
  (println)
  (println "Examples:")
  (println "  node target/main.js --min 0 --max 100 50")
  (println "  node target/main.js --max 100 150")
  (println "  node target/main.js --min -10 -25")
  (println "  node target/main.js 42"))

(defn fmt [n]
  "Форматирует число с 2 знаками после запятой."
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
        min-str (:min parsed)
        max-str (:max parsed)
        positional (:positional parsed)
        number-str (first positional)]
    
    (cond
      ;; Число не указано
      (nil? number-str)
      (do
        (println "Error: Number argument is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много неименованных параметров
      (> (count positional) 1)
      (do
        (println "Error: Only one number argument is allowed.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [min-parsed (when min-str (parse-number min-str))
            max-parsed (when max-str (parse-number max-str))
            number-parsed (parse-number number-str)]
        
        (cond
          ;; Ошибка в min
          (and min-parsed (not (:ok min-parsed)))
          (do
            (println (str "Error: Invalid min '" (:raw min-parsed) "' — " (:reason min-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в max
          (and max-parsed (not (:ok max-parsed)))
          (do
            (println (str "Error: Invalid max '" (:raw max-parsed) "' — " (:reason max-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в числе
          (not (:ok number-parsed))
          (do
            (println (str "Error: Invalid number '" (:raw number-parsed) "' — " (:reason number-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; min > max (проверяем только если оба указаны)
          (and min-parsed max-parsed
               (> (:value min-parsed) (:value max-parsed)))
          (do
            (println (str "Error: min (" (:value min-parsed) ") must be less than or equal to max (" (:value max-parsed) ")."))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — ограничиваем
          :else
          (let [number (:value number-parsed)
                min-val (when min-parsed (:value min-parsed))
                max-val (when max-parsed (:value max-parsed))
                result (clamp number min-val max-val)]
            
            (println "=== Number Clamping Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Number:" 20) " " (fmt number)))
            (println (str (pad-left "Min:" 20) " " (if min-val (fmt min-val) "no limit")))
            (println (str (pad-left "Max:" 20) " " (if max-val (fmt max-val) "no limit")))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "Clamped value:" 20) " " (fmt (:clamped result))))
            (println (str (pad-left "Status:" 20) " " (:status result)))
            
            (when (not= number (:clamped result))
              (println)
              (println (str "Note: Original value was outside the range and has been clamped to "
                           (fmt (:clamped result)) ".")))
            
            (js/process.exit 0)))))))
