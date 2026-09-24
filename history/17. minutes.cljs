(ns cli.core)

;; --- Конвертация ---

(defn minutes->hours-minutes [total-minutes]
  "Конвертирует общее количество минут в часы и оставшиеся минуты.
   Возвращает карту {:hours :minutes}."
  (let [hours (quot total-minutes 60)
        minutes (mod total-minutes 60)]
    {:hours hours :minutes minutes}))

;; --- Парсинг и валидация ---

(defn parse-int [s]
  "Пытается преобразовать строку в целое число.
   Возвращает {:ok true :value n} или {:ok false :raw s}."
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
  (println "Usage: node target/main.js <minutes>")
  (println)
  (println "Converts minutes to hours and minutes.")
  (println)
  (println "Arguments:")
  (println "  <minutes>    Number of minutes (non-negative integer)")
  (println)
  (println "Examples:")
  (println "  node target/main.js 125")
  (println "  node target/main.js 60")
  (println "  node target/main.js 45"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        minutes-str (first js-args)]
    
    (cond
      ;; Аргумент не указан
      (nil? minutes-str)
      (do
        (println "Error: Minutes argument is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [parsed (parse-non-negative-int minutes-str)]
        (if (not (:ok parsed))
          ;; Ошибка валидации
          (do
            (println (str "Error: '" (:raw parsed) "' is not a valid non-negative integer."))
            (println)
            (print-usage)
            (js/process.exit 1))
          
          ;; Всё ок — конвертируем
          (let [total-minutes (:value parsed)
                result (minutes->hours-minutes total-minutes)]
            (println (str total-minutes " minutes = " 
                         (:hours result) " hours " 
                         (:minutes result) " minutes"))
            (js/process.exit 0)))))))
