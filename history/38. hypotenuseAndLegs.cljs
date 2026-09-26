(ns cli.core)

;; --- Вычисление ---

(defn calculate-hypotenuse [a b]
  "Вычисляет гипотенузу по двум катетам: c = √(a² + b²)"
  (js/Math.sqrt (+ (* a a) (* b b))))

(defn calculate-leg [hypotenuse leg]
  "Вычисляет катет по гипотенузе и другому катету: a = √(c² - b²)"
  (let [diff (- (* hypotenuse hypotenuse) (* leg leg))]
    (if (neg? diff)
      {:ok false :reason "hypotenuse must be greater than the leg"}
      {:ok true :value (js/Math.sqrt diff)})))

;; --- Парсинг аргументов ---

(def ^:private valid-flags #{:a :b :c})

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --a, --b, --c.
   --a и --b это катеты, --c это гипотенуза."
  (loop [remaining args
         result {:a nil :b nil :c nil
                 :unknown-flags []
                 :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (let [flag-name (keyword (subs current 2))]
            (if (contains? valid-flags flag-name)
              (recur (rest remaining) result flag-name)
              (recur (rest remaining)
                     (update result :unknown-flags conj current)
                     nil)))

          :else
          (if current-flag
            (if (contains? valid-flags current-flag)
              (recur (rest remaining)
                     (assoc result current-flag current)
                     nil)
              (recur (rest remaining) result nil))
            (recur (rest remaining)
                   (update result :positional conj current)
                   nil)))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-positive-number [s]
  "Пытается преобразовать строку в положительное число."
  (let [n (js/Number s)]
    (if (or (js/isNaN n) (<= n 0))
      {:ok false :raw s :reason "must be a positive number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage:")
  (println "  node target/main.js --a <value> --b <value>     # Calculate hypotenuse c")
  (println "  node target/main.js --c <value> --b <value>     # Calculate leg a")
  (println "  node target/main.js --c <value> --a <value>     # Calculate leg b")
  (println)
  (println "Calculates the missing side of a right triangle using Pythagorean theorem.")
  (println)
  (println "Options (exactly 2 required):")
  (println "  --a <value>    First leg (катет)")
  (println "  --b <value>    Second leg (катет)")
  (println "  --c <value>    Hypotenuse (гипотенуза)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --a 3 --b 4        # c = 5")
  (println "  node target/main.js --c 13 --b 5       # a = 12")
  (println "  node target/main.js --c 10 --a 6       # b = 8"))

(defn fmt [n]
  "Форматирует число с 4 знаками после запятой."
  (.toFixed n 4))

(defn fmt6 [n]
  "Форматирует число с 6 знаками после запятой."
  (.toFixed n 6))

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

      ;; Ни один параметр не указан
      (and (nil? a-str) (nil? b-str) (nil? c-str))
      (do
        (println "Error: At least two parameters are required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Только один параметр указан
      (let [provided (cond-> []
                       a-str (conj "--a")
                       b-str (conj "--b")
                       c-str (conj "--c"))]
        (< (count provided) 2))
      (do
        (println "Error: Exactly two parameters are required (one side is unknown).")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Все три параметра указаны
      (and a-str b-str c-str)
      (do
        (println "Error: Only two parameters should be provided (the third side is calculated).")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      ;; Ровно два параметра указаны
      (let [;; Валидация предоставленных параметров
            a-parsed (when a-str (parse-positive-number a-str))
            b-parsed (when b-str (parse-positive-number b-str))
            c-parsed (when c-str (parse-positive-number c-str))
            
            ;; Сбор ошибок валидации
            errors (cond-> []
                     (and a-parsed (not (:ok a-parsed))) (conj (str "Invalid --a: " (:reason a-parsed)))
                     (and b-parsed (not (:ok b-parsed))) (conj (str "Invalid --b: " (:reason b-parsed)))
                     (and c-parsed (not (:ok c-parsed))) (conj (str "Invalid --c: " (:reason c-parsed))))]
        
        (cond
          ;; Есть ошибки валидации
          (seq errors)
          (do
            (println "Error: Invalid input detected:")
            (doseq [e errors]
              (println (str "  • " e)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Вычисление гипотенузы (даны два катета)
          (and a-str b-str (nil? c-str))
          (let [a (:value a-parsed)
                b (:value b-parsed)
                c (calculate-hypotenuse a b)]
            
            (println "=== Right Triangle Calculator ===")
            (println)
            (println "--- Known sides ---")
            (println (str (pad-left "Leg a:" 20) " " (fmt6 a)))
            (println (str (pad-left "Leg b:" 20) " " (fmt6 b)))
            (println)
            (println "--- Calculated ---")
            (println (str (pad-left "Hypotenuse c:" 20) " " (fmt6 c)))
            (println)
            (println "--- Verification ---")
            (println (str "  a² + b² = c²"))
            (println (str "  " (fmt a) "² + " (fmt b) "² = " (fmt c) "²"))
            (println (str "  " (fmt (* a a)) " + " (fmt (* b b)) " = " (fmt (* c c))))
			(js/process.exit 0))

          ;; Вычисление катета a (даны гипотенуза и катет b)
          (and c-str b-str (nil? a-str))
          (let [c (:value c-parsed)
                b (:value b-parsed)
                result (calculate-leg c b)]
            (if (not (:ok result))
              (do
                (println (str "Error: " (:reason result)))
                (println "       Hypotenuse must be greater than either leg.")
                (println)
                (print-usage)
                (js/process.exit 1))
              (let [a (:value result)]
                (println "=== Right Triangle Calculator ===")
                (println)
                (println "--- Known sides ---")
                (println (str (pad-left "Hypotenuse c:" 20) " " (fmt6 c)))
                (println (str (pad-left "Leg b:" 20) " " (fmt6 b)))
                (println)
                (println "--- Calculated ---")
                (println (str (pad-left "Leg a:" 20) " " (fmt6 a)))
                (println)
                (println "--- Verification ---")
                (println (str "  a² + b² = c²"))
                (println (str "  " (fmt a) "² + " (fmt b) "² = " (fmt c) "²"))
                (println (str "  " (fmt (* a a)) " + " (fmt (* b b)) " = " (fmt (* c c))))
				(js/process.exit 0))))

          ;; Вычисление катета b (даны гипотенуза и катет a)
          (and c-str a-str (nil? b-str))
          (let [c (:value c-parsed)
                a (:value a-parsed)
                result (calculate-leg c a)]
            (if (not (:ok result))
              (do
                (println (str "Error: " (:reason result)))
                (println "       Hypotenuse must be greater than either leg.")
                (println)
                (print-usage)
                (js/process.exit 1))
              (let [b (:value result)]
                (println "=== Right Triangle Calculator ===")
                (println)
                (println "--- Known sides ---")
                (println (str (pad-left "Hypotenuse c:" 20) " " (fmt6 c)))
                (println (str (pad-left "Leg a:" 20) " " (fmt6 a)))
                (println)
                (println "--- Calculated ---")
                (println (str (pad-left "Leg b:" 20) " " (fmt6 b)))
                (println)
                (println "--- Verification ---")
                (println (str "  a² + b² = c²"))
                (println (str "  " (fmt a) "² + " (fmt b) "² = " (fmt c) "²"))
                (println (str "  " (fmt (* a a)) " + " (fmt (* b b)) " = " (fmt (* c c))))
				(js/process.exit 0)))))))))
