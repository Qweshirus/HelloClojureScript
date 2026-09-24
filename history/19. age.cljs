(ns cli.core)

;; --- Парсинг даты ---

(defn parse-date [s]
  "Парсит строку даты в формате dd-mm-yyyy.
   Возвращает {:ok true :day d :month m :year y} или {:ok false :raw s :reason ...}."
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "date is empty"}
    (let [matches (re-matches #"^(\d{2})-(\d{2})-(\d{4})$" s)]
      (if (nil? matches)
        {:ok false :raw s :reason "expected format dd-mm-yyyy (e.g. 15-06-1990)"}
        (let [day (js/parseInt (nth matches 1) 10)
              month (js/parseInt (nth matches 2) 10)
              year (js/parseInt (nth matches 3) 10)]
          (cond
            ;; Проверка диапазонов
            (or (< month 1) (> month 12))
            {:ok false :raw s :reason "month must be between 01 and 12"}

            (or (< day 1) (> day 31))
            {:ok false :raw s :reason "day must be between 01 and 31"}

            (< year 1)
            {:ok false :raw s :reason "year must be positive"}

            ;; Проверка реальной даты через js/Date
            :else
            (let [date (js/Date. year (dec month) day)]
              (if (or (not= (.getFullYear date) year)
                      (not= (.getMonth date) (dec month))
                      (not= (.getDate date) day))
                {:ok false :raw s :reason "invalid date (e.g. Feb 30 does not exist)"}
                {:ok true :day day :month month :year year}))))))))

;; --- Вычисление возраста ---

(defn calculate-age [birth-day birth-month birth-year]
  "Вычисляет возраст на текущую дату.
   Возвращает карту {:age :is-adult}."
  (let [now (js/Date.)
        current-year (.getFullYear now)
        current-month (inc (.getMonth now))
        current-day (.getDate now)
        ;; Базовый возраст — разница лет
        age-base (- current-year birth-year)
        ;; Если день рождения ещё не наступил в этом году, вычитаем 1
        birthday-not-yet (or (< current-month birth-month)
                             (and (= current-month birth-month)
                                  (< current-day birth-day)))
        age (if birthday-not-yet (dec age-base) age-base)
        is-adult (>= age 18)]
    {:age age :is-adult is-adult}))

;; --- CLI ---

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  (println "Usage: node target/main.js <birth-date>")
  (println)
  (println "Calculates age and checks if a person is an adult (18+).")
  (println)
  (println "Arguments:")
  (println "  <birth-date>    Date of birth in format dd-mm-yyyy")
  (println)
  (println "Examples:")
  (println "  node target/main.js 15-06-1990")
  (println "  node target/main.js 01-01-2010"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        date-str (first js-args)]
    
    (cond
      ;; Аргумент не указан
      (nil? date-str)
      (do
        (println "Error: Birth date is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [parsed (parse-date date-str)]
        (if (not (:ok parsed))
          ;; Ошибка валидации
          (do
            (println (str "Error: Invalid date '" (:raw parsed) "' — " (:reason parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))
          
          ;; Проверка, что дата не в будущем
          (let [{:keys [day month year]} parsed
                birth-date (js/Date. year (dec month) day)
                now (js/Date.)]
            (if (> (.getTime birth-date) (.getTime now))
              (do
                (println (str "Error: Date '" date-str "' is in the future."))
                (println)
                (print-usage)
                (js/process.exit 1))
              
              ;; Всё ок — вычисляем возраст
              (let [result (calculate-age day month year)
                    age (:age result)
                    is-adult (:is-adult result)]
                (println (str "Birth date:   " date-str))
                (println (str "Age:          " age " years old"))
                (println (str "Adult (18+):  " (if is-adult "Yes" "No")))
                (js/process.exit 0)))))))))