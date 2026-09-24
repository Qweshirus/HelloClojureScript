(ns cli.core)

;; --- Константы ---

(def ^:private seconds-in-minute 60)
(def ^:private seconds-in-hour (* 60 seconds-in-minute))
(def ^:private seconds-in-day (* 24 seconds-in-hour))
(def ^:private seconds-in-year (* 365 seconds-in-day))

;; --- Конвертация ---

(defn seconds->duration [total-seconds]
  "Конвертирует секунды в года, дни, часы, минуты и секунды.
   Возвращает карту {:years :days :hours :minutes :seconds}."
  (let [years (quot total-seconds seconds-in-year)
        rem1 (mod total-seconds seconds-in-year)
        days (quot rem1 seconds-in-day)
        rem2 (mod rem1 seconds-in-day)
        hours (quot rem2 seconds-in-hour)
        rem3 (mod rem2 seconds-in-hour)
        minutes (quot rem3 seconds-in-minute)
        seconds (mod rem3 seconds-in-minute)]
    {:years years
     :days days
     :hours hours
     :minutes minutes
     :seconds seconds}))

;; --- Форматирование ---

(defn pad2 [n]
  "Дополняет число нулями слева до 2 знаков."
  (let [s (str n)]
    (if (< (count s) 2)
      (str "0" s)
      s)))

(defn pluralize [n singular plural]
  "Возвращает правильную форму слова в зависимости от числа.
   Для английского языка: 1 -> singular, иначе -> plural."
  (if (= n 1)
    singular
    plural))

(defn format-time [hours minutes seconds]
  "Форматирует время в виде HH:MM:SS."
  (str (pad2 hours) ":" (pad2 minutes) ":" (pad2 seconds)))

(defn format-duration [duration]
  "Форматирует длительность в человекочитаемую строку.
   - Если years > 0: 'X year(s), Y day(s), HH:MM:SS'
   - Если years == 0 и days > 0: 'Y day(s), HH:MM:SS'
   - Если years == 0 и days == 0: 'HH:MM:SS'"
  (let [{:keys [years days hours minutes seconds]} duration
        time-part (format-time hours minutes seconds)
        year-word (pluralize years "year" "years")
        day-word (pluralize days "day" "days")]
    (cond
      (pos? years)
      (str years " " year-word ", " days " " day-word ", " time-part)

      (pos? days)
      (str days " " day-word ", " time-part)

      :else
      time-part)))

;; --- Парсинг и валидация ---

(defn parse-int [s]
  "Пытается преобразовать строку в целое число."
  (let [n (js/Number s)]
    (if (or (js/isNaN n)
            (not (js/Number.isInteger n)))
      {:ok false :raw s}
      {:ok true :value n})))

(defn parse-non-negative-int [s]
  "Пытается преобразовать строку в неотрицательное целое число."
  (let [parsed (parse-int s)]
    (if (and (:ok parsed) (>= (:value parsed) 0))
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
  (println "Usage: node target/main.js <seconds>")
  (println)
  (println "Converts seconds to years, days, hours, minutes and seconds.")
  (println)
  (println "Arguments:")
  (println "  <seconds>    Number of seconds (non-negative integer)")
  (println)
  (println "Examples:")
  (println "  node target/main.js 3661")
  (println "  node target/main.js 86400")
  (println "  node target/main.js 31536000"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        seconds-str (first js-args)]
    
    (cond
      ;; Аргумент не указан
      (nil? seconds-str)
      (do
        (println "Error: Seconds argument is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [parsed (parse-non-negative-int seconds-str)]
        (if (not (:ok parsed))
          ;; Ошибка валидации
          (do
            (println (str "Error: '" (:raw parsed) "' is not a valid non-negative integer."))
            (println)
            (print-usage)
            (js/process.exit 1))
          
          ;; Всё ок — конвертируем
          (let [total-seconds (:value parsed)
                duration (seconds->duration total-seconds)
                formatted (format-duration duration)]
            (println (str total-seconds " seconds = " formatted))
            (js/process.exit 0)))))))
