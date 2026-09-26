(ns cli.core)

;; --- Вычисление ---

(defn absolute-difference [a b]
  "Вычисляет абсолютную разницу между двумя числами: |a - b|"
  (js/Math.abs (- a b)))

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
  (println "Usage: node target/main.js <number1> <number2>")
  (println)
  (println "Calculates the absolute difference between two numbers: |a - b|")
  (println)
  (println "Arguments:")
  (println "  <number1>    First number")
  (println "  <number2>    Second number")
  (println)
  (println "Examples:")
  (println "  node target/main.js 10 5")
  (println "  node target/main.js -5 3")
  (println "  node target/main.js 3.14 2.71"))

(defn fmt [n]
  "Форматирует число с 2 знаками после запятой и разделителями тысяч."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

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
        (println "Error: Not enough arguments. Expected exactly 2 numbers.")
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
      ;; Ровно 2 аргумента
      (let [[arg1-str arg2-str] js-args
            parsed1 (parse-number arg1-str)
            parsed2 (parse-number arg2-str)]
        
        (cond
          ;; Ошибка в первом числе
          (not (:ok parsed1))
          (do
            (println (str "Error: Invalid first number '" (:raw parsed1) "' — " (:reason parsed1)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка во втором числе
          (not (:ok parsed2))
          (do
            (println (str "Error: Invalid second number '" (:raw parsed2) "' — " (:reason parsed2)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [a (:value parsed1)
                b (:value parsed2)
                diff (absolute-difference a b)]
            
            (println "=== Absolute Difference Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Number 1:" 15) " " (fmt a)))
            (println (str (pad-left "Number 2:" 15) " " (fmt b)))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "|a - b|:" 15) " " (fmt diff)))
            
            (js/process.exit 0)))))))
