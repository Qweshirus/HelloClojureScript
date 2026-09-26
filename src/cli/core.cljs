;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Вычисление ---

(defn radians->degrees [radians]
  "Конвертирует радианы в десятичные градусы.
   Формула: degrees = radians * (180 / π)"
  (* radians (/ 180 js/Math.PI)))

(defn decimal->dms [decimal-degrees]
  "Конвертирует десятичные градусы в формат Градусы° Минуты' Секунды''."
  (let [sign (if (neg? decimal-degrees) "-" "")
        abs-deg (js/Math.abs decimal-degrees)
        deg (js/Math.floor abs-deg)
        min-float (* (- abs-deg deg) 60)
        min (js/Math.floor min-float)
        sec-float (* (- min-float min) 60)
        ;; Округляем секунды до 2 знаков, чтобы избежать 60.00''
        sec (js/Math.round (* sec-float 100) / 100)]
    (str sign deg "° " min "' " sec "''")))

;; --- Парсинг и валидация ---

(defn parse-number [s]
  "Пытается преобразовать строку в число."
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a valid number"}
      {:ok true :value n})))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js <radians>")
  (println)
  (println "Converts radians to decimal degrees and DMS (Degrees, Minutes, Seconds).")
  (println)
  (println "Arguments:")
  (println "  <radians>    Angle in radians (any real number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js 3.14159265")
  (println "  node target/main.js 1.5708")
  (println "  node target/main.js -0.5"))

(defn fmt [n]
  "Форматирует число с 4 знаками после запятой для точности."
  (.toFixed n 4))

(defn pad-left [s width]
  "Дополняет строку пробелами слева до заданной ширины."
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)]
    (cond
      ;; Слишком мало аргументов
      (< (count js-args) 1)
      (do
        (println "Error: Missing argument. Expected exactly 1 number (radians).")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много аргументов
      (> (count js-args) 1)
      (do
        (println (str "Error: Too many arguments. Expected 1 number, got " (count js-args) "."))
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [arg-str (first js-args)
            parsed (parse-number arg-str)]
        
        (cond
          ;; Ошибка в числе
          (not (:ok parsed))
          (do
            (println (str "Error: Invalid value '" (:raw parsed) "' — " (:reason parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — конвертируем
          :else
          (let [radians (:value parsed)
                degrees (radians->degrees radians)
                dms (decimal->dms degrees)]
            
            (println "=== Radians to Degrees Converter ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Radians:" 15) " " (fmt radians) " rad"))
            (println)
            (println "--- Output ---")
            (println (str (pad-left "Decimal:" 15) " " (fmt degrees) " °"))
            (println (str (pad-left "DMS:" 15) " " dms))
            
            ;; Дополнительная справка для популярных значений
            (println)
            (println "--- Reference ---")
            (println "  π rad  = 180°")
            (println "  π/2 rad = 90°")
            (println "  π/4 rad = 45°")
            (println "  2π rad = 360°")
            
            (js/process.exit 0)))))))
