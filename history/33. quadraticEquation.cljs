(ns cli.core)

;; --- Вычисление ---

(defn calculate-quadratic [a b c]
  "Вычисляет дискриминант и корни квадратного уравнения ax² + bx + c = 0.
   Возвращает карту с результатами."
  (let [discriminant (- (* b b) (* 4 a c))
        roots (cond
                (> discriminant 0)
                (let [sqrt-d (js/Math.sqrt discriminant)
                      x1 (/ (+ (- b) sqrt-d) (* 2 a))
                      x2 (/ (- (- b) sqrt-d) (* 2 a))]
                  {:count 2 :x1 x1 :x2 x2})

                (zero? discriminant)
                (let [x (/ (- b) (* 2 a))]
                  {:count 1 :x x})

                :else
                {:count 0})]
    {:discriminant discriminant
     :roots roots}))

;; --- Парсинг аргументов ---

(def ^:private valid-flags #{:a :b :c})

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --a, --b, --c.
   Отслеживает неизвестные флаги и неименованные параметры."
  (loop [remaining args
         result {:a nil :b nil :c nil
                 :unknown-flags []
                 :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          ;; Проверяем, является ли текущий аргумент флагом (начинается с --)
          (.startsWith current "--")
          (let [flag-name (keyword (subs current 2))]
            (if (contains? valid-flags flag-name)
              ;; Известный флаг
              (recur (rest remaining) result flag-name)
              ;; Неизвестный флаг
              (recur (rest remaining)
                     (update result :unknown-flags conj current)
                     nil)))

          :else
          (if current-flag
            ;; Есть активный флаг — присваиваем значение
            (if (contains? valid-flags current-flag)
              (recur (rest remaining)
                     (assoc result current-flag current)
                     nil)
              (recur (rest remaining) result nil))
            ;; Нет активного флага — это неименованный параметр
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
      {:ok false :raw s :reason "must be a number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --a <value> --b <value> --c <value>")
  (println)
  (println "Calculates discriminant and roots of quadratic equation: ax² + bx + c = 0")
  (println)
  (println "Options:")
  (println "  --a <value>    Coefficient a (non-zero number)")
  (println "  --b <value>    Coefficient b (number)")
  (println "  --c <value>    Coefficient c (number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --a 1 --b -5 --c 6")
  (println "  node target/main.js --a 1 --b 2 --c 1")
  (println "  node target/main.js --a 1 --b 0 --c 1"))

(defn fmt [n]
  "Форматирует число с 2 знаками после запятой."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        a-str (:a parsed)
        b-str (:b parsed)
        c-str (:c parsed)
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
        (println "       This program only accepts named parameters: --a, --b, --c")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Не все параметры указаны
      (or (nil? a-str) (nil? b-str) (nil? c-str))
      (let [missing (cond-> []
                      (nil? a-str) (conj "--a")
                      (nil? b-str) (conj "--b")
                      (nil? c-str) (conj "--c"))]
        (do
          (println (str "Error: Missing required parameter(s): " (clojure.string/join ", " missing)))
          (println)
          (print-usage)
          (js/process.exit 1)))

      :else
      (let [a-parsed (parse-number a-str)
            b-parsed (parse-number b-str)
            c-parsed (parse-number c-str)]
        
        (cond
          ;; Ошибка в a
          (not (:ok a-parsed))
          (do
            (println (str "Error: Invalid value for --a '" (:raw a-parsed) "' — " (:reason a-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в b
          (not (:ok b-parsed))
          (do
            (println (str "Error: Invalid value for --b '" (:raw b-parsed) "' — " (:reason b-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в c
          (not (:ok c-parsed))
          (do
            (println (str "Error: Invalid value for --c '" (:raw c-parsed) "' — " (:reason c-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; a = 0
          (zero? (:value a-parsed))
          (do
            (println "Error: Coefficient --a must be non-zero (otherwise it's not a quadratic equation).")
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [a (:value a-parsed)
                b (:value b-parsed)
                c (:value c-parsed)
                result (calculate-quadratic a b c)
                roots (:roots result)]
            
            (println "=== Quadratic Equation Solver ===")
            (println)
            (println (str "Equation: " (fmt a) "x² + " (fmt b) "x + " (fmt c) " = 0"))
            (println)
            (println "--- Coefficients ---")
            (println (str (pad-left "a:" 15) " " (fmt a)))
            (println (str (pad-left "b:" 15) " " (fmt b)))
            (println (str (pad-left "c:" 15) " " (fmt c)))
            (println)
            (println "--- Discriminant ---")
            (println (str (pad-left "D = b² - 4ac:" 15) " " (fmt (:discriminant result))))
            (println)
            
            (cond
              (= (:count roots) 2)
              (do
                (println "--- Roots (D > 0) ---")
                (println (str (pad-left "Two real roots:" 15)))
                (println (str (pad-left "x₁:" 15) " " (fmt (:x1 roots))))
                (println (str (pad-left "x₂:" 15) " " (fmt (:x2 roots)))))

              (= (:count roots) 1)
              (do
                (println "--- Roots (D = 0) ---")
                (println (str (pad-left "One real root:" 15)))
                (println (str (pad-left "x:" 15) " " (fmt (:x roots)))))

              :else
              (do
                (println "--- Roots (D < 0) ---")
                (println (str (pad-left "No real roots:" 15) " (discriminant is negative)"))))
            
            (js/process.exit 0)))))))
