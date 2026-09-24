(ns cli.core)

;; --- Проверка високосности ---

(defn leap-year? [year]
  "Проверяет, является ли год високосным по правилам Григорианского календаря.
   Год високосный, если:
   - делится на 400, ИЛИ
   - делится на 4, но не делится на 100."
  (or (zero? (mod year 400))
      (and (zero? (mod year 4))
           (not (zero? (mod year 100))))))

;; --- Парсинг и валидация ---

(defn parse-int [s]
  "Пытается преобразовать строку в целое число."
  (let [n (js/Number s)]
    (if (or (js/isNaN n)
            (not (js/Number.isInteger n)))
      {:ok false :raw s}
      {:ok true :value n})))

(defn parse-positive-int [s]
  "Пытается преобразовать строку в положительное целое число."
  (let [parsed (parse-int s)]
    (if (and (:ok parsed) (pos? (:value parsed)))
      parsed
      {:ok false :raw s})))

;; --- CLI ---

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  (println "Usage: node target/main.js <year>")
  (println)
  (println "Checks if a given year is a leap year.")
  (println)
  (println "Arguments:")
  (println "  <year>    A positive integer year (e.g. 2024)")
  (println)
  (println "Examples:")
  (println "  node target/main.js 2024")
  (println "  node target/main.js 1900")
  (println "  node target/main.js 2000"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        year-str (first js-args)]
    
    (cond
      ;; Аргумент не указан
      (nil? year-str)
      (do
        (println "Error: Year argument is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [parsed (parse-positive-int year-str)]
        (if (not (:ok parsed))
          ;; Ошибка валидации
          (do
            (println (str "Error: '" (:raw parsed) "' is not a valid positive integer."))
            (println)
            (print-usage)
            (js/process.exit 1))
          
          ;; Всё ок — проверяем високосность
          (let [year (:value parsed)
                is-leap (leap-year? year)]
            (println (str "Year: " year))
            (println (str "Leap year: " (if is-leap "Yes" "No")))
            (js/process.exit 0)))))))
