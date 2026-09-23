;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Константы (в Цельсиях) ---

(def ^:private freezing-point-c 0)
(def ^:private boiling-point-c 100)
(def ^:private plasma-point-c 3000)

;; --- Функции конвертации ---

(defn fahrenheit->celsius [f]
  "Преобразует градусы Фаренгейта в Цельсии."
  (* (- f 32) (/ 5 9)))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает все значения после --celsius и --fahrenheit в векторы."
  (loop [remaining args
         result {:celsius [] :fahrenheit []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--celsius")
          (recur (rest remaining) result :celsius)

          (= current "--fahrenheit")
          (recur (rest remaining) result :fahrenheit)

          :else
          (if current-flag
            (recur (rest remaining)
                   (update result current-flag conj current)
                   current-flag)
            ;; Если значение идёт без флага — игнорируем (или можно считать ошибкой)
            (recur (rest remaining) result current-flag)))))))

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
  (println "Usage: node target/main.js [options]")
  (println)
  (println "Options:")
  (println "  --celsius <value> [<value> ...]       Temperatures in Celsius")
  (println "  --fahrenheit <value> [<value> ...]    Temperatures in Fahrenheit")
  (println)
  (println "Examples:")
  (println "  node target/main.js --celsius -10 25 100 5000")
  (println "  node target/main.js --fahrenheit 32 212 5432")
  (println "  node target/main.js --celsius 0 100 --fahrenheit 32 212"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

(defn parse-number [s]
  "Пытается преобразовать строку в число. Возвращает nil при ошибке."
  (let [n (js/Number s)]
    (when-not (js/isNaN n) n)))

(defn process-values [values unit-label convert-fn]
  "Обрабатывает список значений: парсит, конвертирует, выводит.
   Возвращает true, если всё успешно, и false, если была ошибка."
  (let [parsed (mapv (fn [s]
                       (let [n (parse-number s)]
                         (if n
                           {:ok true :raw s :value n}
                           {:ok false :raw s})))
                     values)]
    (if (some #(not (:ok %)) parsed)
      ;; Есть ошибка — выводим все невалидные значения
      (do
        (doseq [p parsed
                :when (not (:ok p))]
          (println (str "Error: '" (:raw p) "' is not a valid number.")))
        false)
      ;; Всё ок — выводим результаты
      (do
        (doseq [p parsed]
          (let [num (:value p)
                temp-c (convert-fn num)
                state (water-state temp-c)]
            (println (str unit-label ": " (round2 num)
                         "  →  " (round2 temp-c) " °C  →  " (state-name state)))))
        true))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        c-vals (:celsius parsed)
        f-vals (:fahrenheit parsed)]
    
    (cond
      ;; Ни одного значения не передано
      (and (empty? c-vals) (empty? f-vals))
      (do
        (print-usage)
        (js/process.exit 1))

      :else
      (let [c-ok (if (seq c-vals)
                   (process-values c-vals "°C" identity)
                   true)
            f-ok (if (seq f-vals)
                   (process-values f-vals "°F" fahrenheit->celsius)
                   true)]
        (if (and c-ok f-ok)
          (js/process.exit 0)
          (do
            (println)
            (print-usage)
            (js/process.exit 1)))))))
