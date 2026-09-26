;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Вычисление ---

(defn radians->degrees [radians]
  "Конвертирует радианы в десятичные градусы.
   Формула: degrees = radians * (180 / π)"
  (* radians (/ 180 js/Math.PI)))

(defn degrees->radians [degrees]
  "Конвертирует градусы в радианы.
   Формула: radians = degrees * (π / 180)"
  (* degrees (/ js/Math.PI 180)))

(defn decimal->dms [decimal-degrees]
  "Конвертирует десятичные градусы в формат Градусы° Минуты' Секунды''."
  (let [sign (if (neg? decimal-degrees) "-" "")
        abs-deg (js/Math.abs decimal-degrees)
        deg (js/Math.floor abs-deg)
        min-float (* (- abs-deg deg) 60)
        min (js/Math.floor min-float)
        sec-float (* (- min-float min) 60)
        sec (js/Math.round (* sec-float 100) / 100)]
    (str sign deg "° " min "' " sec "''")))

(defn radians->pi-format [radians]
  "Представляет радианы как множитель π (например, 0.5π)."
  (let [pi-coeff (/ radians js/Math.PI)
        rounded (js/Math.round (* pi-coeff 1000) / 1000)]
    (if (zero? rounded)
      "0"
      (let [coeff-str (if (= rounded 1) "" (if (= rounded -1) "-" (str rounded)))
            pi-str (if (zero? rounded) "" "π")]
        (str coeff-str pi-str)))))

;; --- Парсинг аргументов ---

(def ^:private valid-flags #{:radians :degrees})

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --radians и --degrees.
   Отслеживает неизвестные флаги и неименованные параметры."
  (loop [remaining args
         result {:radians nil :degrees nil
                 :unknown-flags []
                 :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (let [flag-name (keyword (subs current 2))]
            (if (contains? valid-flags flag-name)
              (recur (rest remaining) result flag-name)
              (recur (rest remaining)
                     (update result :unknown-flags conj current)
                     nil)))

          :else
          (if current-flag
            (if (contains? valid-flags current-flag)
              (recur (rest remaining)
                     (assoc result current-flag current)
                     nil)
              (recur (rest remaining) result nil))
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
      {:ok false :raw s :reason "must be a valid number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --radians <value>")
  (println "       node target/main.js --degrees <value>")
  (println)
  (println "Converts between radians and degrees.")
  (println)
  (println "Options (exactly one required):")
  (println "  --radians <value>    Input in radians, output in degrees")
  (println "  --degrees <value>    Input in degrees, output in radians")
  (println)
  (println "Examples:")
  (println "  node target/main.js --radians 3.14159265")
  (println "  node target/main.js --radians 1.5708")
  (println "  node target/main.js --degrees 180")
  (println "  node target/main.js --degrees 90")
  (println "  node target/main.js --degrees -45"))

(defn fmt [n]
  "Форматирует число с 4 знаками после запятой."
  (.toFixed n 4))

(defn fmt6 [n]
  "Форматирует число с 6 знаками после запятой."
  (.toFixed n 6))

(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        radians-str (:radians parsed)
        degrees-str (:degrees parsed)
        unknown-flags (:unknown-flags parsed)
        positional (:positional parsed)]
    
    (cond
      ;; Есть неизвестные флаги
      (seq unknown-flags)
      (do
        (println (str "Error: Unknown option(s): " (clojure.string/join ", " unknown-flags)))
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Есть неименованные параметры
      (seq positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " positional)))
        (println "       This program only accepts named parameters: --radians, --degrees")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Ни один параметр не указан
      (and (nil? radians-str) (nil? degrees-str))
      (do
        (println "Error: One of --radians or --degrees is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Оба параметра указаны
      (and radians-str degrees-str)
      (do
        (println "Error: Specify only one of --radians or --degrees, not both.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (cond
        ;; Конвертация радиан в градусы
        radians-str
        (let [parsed (parse-number radians-str)]
          (if (not (:ok parsed))
            (do
              (println (str "Error: Invalid value for --radians '" (:raw parsed) "' — " (:reason parsed)))
              (println)
              (print-usage)
              (js/process.exit 1))
            (let [radians (:value parsed)
                  degrees (radians->degrees radians)
                  dms (decimal->dms degrees)
                  pi-format (radians->pi-format radians)]
              
              (println "=== Radians to Degrees Converter ===")
              (println)
              (println "--- Input ---")
              (println (str (pad-left "Radians:" 20) " " (fmt6 radians) " rad"))
              (println (str (pad-left "As multiple of π:" 20) " " pi-format))
              (println)
              (println "--- Output ---")
              (println (str (pad-left "Decimal degrees:" 20) " " (fmt degrees) " °"))
              (println (str (pad-left "DMS:" 20) " " dms))
              (js/process.exit 0))))

        ;; Конвертация градусов в радианы
        degrees-str
        (let [parsed (parse-number degrees-str)]
          (if (not (:ok parsed))
            (do
              (println (str "Error: Invalid value for --degrees '" (:raw parsed) "' — " (:reason parsed)))
              (println)
              (print-usage)
              (js/process.exit 1))
            (let [degrees (:value parsed)
                  radians (degrees->radians degrees)
                  pi-format (radians->pi-format radians)]
              
              (println "=== Degrees to Radians Converter ===")
              (println)
              (println "--- Input ---")
              (println (str (pad-left "Degrees:" 20) " " (fmt degrees) " °"))
              (println (str (pad-left "DMS:" 20) " " (decimal->dms degrees)))
              (println)
              (println "--- Output ---")
              (println (str (pad-left "Radians:" 20) " " (fmt6 radians) " rad"))
              (println (str (pad-left "As multiple of π:" 20) " " pi-format))
              (js/process.exit 0))))))))
