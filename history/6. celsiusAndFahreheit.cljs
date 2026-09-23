(ns cli.core)

;; --- Функции конвертации ---

(defn celsius->fahrenheit [c]
  "Преобразует градусы Цельсия в Фаренгейты."
  (+ (* c (/ 9 5)) 32))

(defn fahrenheit->celsius [f]
  "Преобразует градусы Фаренгейта в Цельсии."
  (* (- f 32) (/ 5 9)))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Ищет флаги --celsius и --fahrenheit, возвращает карту с найденным флагом и значением."
  (loop [remaining args
         result {}]
    (if (empty? remaining)
      result
      (let [current (first remaining)
            next-val (second remaining)]
        (cond
          (= current "--celsius")
          (recur (drop 2 remaining) (assoc result :celsius next-val))

          (= current "--fahrenheit")
          (recur (drop 2 remaining) (assoc result :fahrenheit next-val))

          :else
          (recur (rest remaining) result))))))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js [option] <value>")
  (println "Options:")
  (println "  --celsius <value>       Convert Celsius to Fahrenheit")
  (println "  --fahrenheit <value>    Convert Fahrenheit to Celsius")
  (println)
  (println "Examples:")
  (println "  node target/main.js --celsius 100")
  (println "  node target/main.js --fahrenheit 212"))

(defn round2 [n]
  "Округляет число до 2 знаков после запятой."
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  "Точка входа приложения."
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        c-val (:celsius parsed)
        f-val (:fahrenheit parsed)]
    (cond
      ;; Оба флага указаны одновременно
      (and c-val f-val)
      (do
        (println "Error: Specify only one of --celsius or --fahrenheit.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Конвертация Цельсий → Фаренгейт
      c-val
      (let [num (js/Number c-val)]
        (if (js/isNaN num)
          (do
            (println (str "Error: '" c-val "' is not a valid number."))
            (js/process.exit 1))
          (do
            (println (str num " °C = " (round2 (celsius->fahrenheit num)) " °F"))
            (js/process.exit 0))))

      ;; Конвертация Фаренгейт → Цельсий
      f-val
      (let [num (js/Number f-val)]
        (if (js/isNaN num)
          (do
            (println (str "Error: '" f-val "' is not a valid number."))
            (js/process.exit 1))
          (do
            (println (str num " °F = " (round2 (fahrenheit->celsius num)) " °C"))
            (js/process.exit 0))))

      ;; Ничего не указано
      :else
      (do
        (println "Error: No conversion option specified.")
        (println)
        (print-usage)
        (js/process.exit 1)))))
