;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Константы ---

(def ^:private phi
  "Золотое сечение: φ = (1 + √5) / 2"
  (/ (+ 1 (js/Math.sqrt 5)) 2))

;; --- Вычисление ---

(defn analyze-golden-ratio [a b]
  "Анализирует пропорцию a/b на близость к золотому сечению.
   Возвращает карту с результатами."
  (let [ratio (/ a b)
        abs-deviation (js/Math.abs (- ratio phi))
        pct-deviation (* (/ abs-deviation phi) 100)
        verdict (cond
                  (< pct-deviation 0.1)  "Perfect golden ratio"
                  (< pct-deviation 1)    "Very close to golden ratio"
                  (< pct-deviation 5)    "Close to golden ratio"
                  (< pct-deviation 10)   "Approximate golden ratio"
                  (< pct-deviation 25)   "Weak resemblance"
                  :else                  "Not a golden ratio")]
    {:ratio ratio
     :abs-deviation abs-deviation
     :pct-deviation pct-deviation
     :verdict verdict}))

;; --- Парсинг и валидация ---

(defn parse-number [s]
  "Пытается преобразовать строку в число."
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a valid number"}
      {:ok true :value n})))

(defn parse-positive-number [s]
  "Пытается преобразовать строку в положительное число."
  (let [parsed (parse-number s)]
    (if (and (:ok parsed) (pos? (:value parsed)))
      parsed
      {:ok false :raw s :reason "must be a positive number"})))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js <a> <b>")
  (println)
  (println "Calculates how close the ratio a/b is to the golden ratio φ ≈ 1.618")
  (println)
  (println "Arguments:")
  (println "  <a>    Larger value (positive number)")
  (println "  <b>    Smaller value (positive number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js 1.618 1")
  (println "  node target/main.js 21 13")
  (println "  node target/main.js 100 61.8"))

(defn fmt [n]
  "Форматирует число с 2 знаками после запятой и разделителями тысяч."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

(defn fmt4 [n]
  "Форматирует число с 4 знаками после запятой."
  (.toFixed n 4))

(defn fmt6 [n]
  "Форматирует число с 6 знаками после запятой."
  (.toFixed n 6))

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
      (< (count js-args) 2)
      (do
        (println "Error: Not enough arguments. Expected exactly 2 positive numbers.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Слишком много аргументов
      (> (count js-args) 2)
      (do
        (println (str "Error: Too many arguments. Expected 2 numbers, got " (count js-args) "."))
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [[a-str b-str] js-args
            a-parsed (parse-positive-number a-str)
            b-parsed (parse-positive-number b-str)]
        
        (cond
          ;; Ошибка в a
          (not (:ok a-parsed))
          (do
            (println (str "Error: Invalid value for a '" (:raw a-parsed) "' — " (:reason a-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в b
          (not (:ok b-parsed))
          (do
            (println (str "Error: Invalid value for b '" (:raw b-parsed) "' — " (:reason b-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — анализируем
          :else
          (let [a (:value a-parsed)
                b (:value b-parsed)
                result (analyze-golden-ratio a b)]
            
            (println "=== Golden Ratio Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "a:" 20) " " (fmt a)))
            (println (str (pad-left "b:" 20) " " (fmt b)))
            (println)
            (println "--- Analysis ---")
            (println (str (pad-left "Ratio a/b:" 20) " " (fmt6 (:ratio result))))
            (println (str (pad-left "Golden ratio φ:" 20) " " (fmt6 phi)))
            (println)
            (println "--- Deviation ---")
            (println (str (pad-left "Absolute:" 20) " " (fmt6 (:abs-deviation result))))
            (println (str (pad-left "Percentage:" 20) " " (fmt4 (:pct-deviation result)) "%"))
            (println)
            (println "--- Verdict ---")
            (println (str "  " (:verdict result)))
            
            (js/process.exit 0)))))))
