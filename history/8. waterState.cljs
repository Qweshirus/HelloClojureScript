(ns cli.core)

;; --- Константы (в Цельсиях) ---

(def ^:private freezing-point-c 0)    ;; 0°C
(def ^:private boiling-point-c 100)   ;; 100°C
(def ^:private plasma-point-c 3000)   ;; ~3000°C

;; --- Функции конвертации ---

(defn fahrenheit->celsius [f]
  "Преобразует градусы Фаренгейта в Цельсии."
  (* (- f 32) (/ 5 9)))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Ищет флаги --celsius и --fahrenheit, возвращает карту."
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

;; --- Определение состояния ---

(defn water-state [temp-c]
  "Определяет агрегатное состояние воды по температуре в Цельсиях."
  (cond
    (< temp-c freezing-point-c) :ice
    (< temp-c boiling-point-c)  :water
    (< temp-c plasma-point-c)   :steam
    :else                       :plasma))

(defn state-name [state]
  "Возвращает человекочитаемое название состояния."
  (case state
    :ice    "Лёд"
    :water  "Вода"
    :steam  "Пар"
    :plasma "Плазма"))

;; --- Вспомогательные функции ---

(defn print-usage []
  (println "Usage: node target/main.js [option] <temperature>")
  (println)
  (println "Options:")
  (println "  --celsius <value>       Temperature in Celsius")
  (println "  --fahrenheit <value>    Temperature in Fahrenheit")
  (println)
  (println "Examples:")
  (println "  node target/main.js --celsius -10")
  (println "  node target/main.js --fahrenheit 212")
  (println "  node target/main.js --celsius 5000"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
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

      ;; Ни один флаг не указан
      (and (nil? c-val) (nil? f-val))
      (do
        (print-usage)
        (js/process.exit 1))

      ;; Цельсии
      c-val
      (let [num (js/Number c-val)]
        (if (js/isNaN num)
          (do
            (println (str "Error: '" c-val "' is not a valid number."))
            (js/process.exit 1))
          (let [state (water-state num)]
            (println (str "Температура: " (round2 num) " °C"))
            (println (str "Состояние:   " (state-name state)))
            (js/process.exit 0))))

      ;; Фаренгейты
      f-val
      (let [num (js/Number f-val)
            temp-c (fahrenheit->celsius num)]
        (if (js/isNaN num)
          (do
            (println (str "Error: '" f-val "' is not a valid number."))
            (js/process.exit 1))
          (let [state (water-state temp-c)]
            (println (str "Температура: " (round2 num) " °F"))
            (println (str "В Цельсиях:  " (round2 temp-c) " °C"))
            (println (str "Состояние:   " (state-name state)))
            (js/process.exit 0)))))))
