(ns cli.core)

;; --- Конвертация буквенных оценок ---

(def ^:private letter-grades
  "Таблица конвертации буквенных оценок в числовые (шкала 4.0)."
  {"A+"  4.0  "A"   4.0  "A-"  3.7
   "B+"  3.3  "B"   3.0  "B-"  2.7
   "C+"  2.3  "C"   2.0  "C-"  1.7
   "D+"  1.3  "D"   1.0  "D-"  0.7
   "F"   0.0})

(defn letter->grade [s]
  "Конвертирует буквенную оценку в числовую. Возвращает nil, если не найдена."
  (get letter-grades (.toUpperCase s)))

;; --- Вычисление GPA ---

(defn calculate-gpa [entries]
  "Вычисляет взвешенный средний балл.
   entries — вектор карт {:grade :credits :original}
   Возвращает карту с результатами."
  (let [total-credits (reduce + (map :credits entries))
        weighted-sum (reduce + (map #(* (:grade %) (:credits %)) entries))
        gpa (if (zero? total-credits) 0 (/ weighted-sum total-credits))]
    {:entries entries
     :total-credits total-credits
     :weighted-sum weighted-sum
     :gpa gpa}))

;; --- Парсинг аргументов ---

(defn parse-entry [grade-str credits-str]
  "Парсит пару (оценка, кредиты).
   Возвращает {:ok true :grade :credits :original} или {:ok false :reason}."
  (let [letter-grade (letter->grade grade-str)
        numeric-grade (when (nil? letter-grade) (js/Number grade-str))
        grade (or letter-grade numeric-grade)
        credits (js/Number credits-str)]
    (cond
      (nil? grade)
      {:ok false :reason (str "invalid grade '" grade-str "' (expected number or A-F)")}

      (js/isNaN grade)
      {:ok false :reason (str "invalid grade '" grade-str "'")}

      (or (js/isNaN credits) (<= credits 0))
      {:ok false :reason (str "invalid credits '" credits-str "' (must be positive)")}

      (not (js/Number.isInteger credits))
      {:ok false :reason (str "credits '" credits-str "' must be an integer")}

      :else
      {:ok true
       :grade grade
       :credits credits
       :original grade-str})))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js <grade> <credits> [<grade> <credits> ...]")
  (println)
  (println "Calculates GPA (Grade Point Average) with weighted credits.")
  (println)
  (println "Arguments (pairs):")
  (println "  <grade>     Numeric grade (e.g. 4.5) or letter grade (A, B+, C-, F, etc.)")
  (println "  <credits>   Course credits (positive integer)")
  (println)
  (println "Letter grade scale (4.0):")
  (println "  A+/A = 4.0, A- = 3.7")
  (println "  B+ = 3.3, B = 3.0, B- = 2.7")
  (println "  C+ = 2.3, C = 2.0, C- = 1.7")
  (println "  D+ = 1.3, D = 1.0, D- = 0.7")
  (println "  F = 0.0")
  (println)
  (println "Examples:")
  (println "  node target/main.js 5 3 4 2 3 4")
  (println "  node target/main.js A 3 B+ 4 C 2")
  (println "  node target/main.js 4.5 3 3.8 2 5 4"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

(defn fmt [n]
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

(defn pad-right [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str s (apply str (repeat (- width (count s)) " "))))))

(defn gpa-label [gpa]
  "Возвращает текстовую оценку GPA."
  (cond
    (>= gpa 3.7) "Excellent"
    (>= gpa 3.0) "Good"
    (>= gpa 2.0) "Satisfactory"
    (>= gpa 1.0) "Poor"
    :else "Failing"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)]
    
    (cond
      ;; Нет аргументов
      (empty? js-args)
      (do
        (println "Error: No grades provided.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Нечётное количество аргументов
      (odd? (count js-args))
      (do
        (println "Error: Arguments must come in pairs: <grade> <credits>")
        (println (str "       Got " (count js-args) " argument(s), expected even number."))
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      ;; Парсим все пары
      (let [pairs (partition 2 js-args)
            parsed (mapv (fn [[grade-str credits-str]]
                           (let [res (parse-entry grade-str credits-str)]
                             (assoc res :grade-str grade-str :credits-str credits-str)))
                         pairs)
            errors (filterv #(not (:ok %)) parsed)
            ok-entries (filterv :ok parsed)]
        
        (cond
          ;; Есть ошибки парсинга
          (seq errors)
          (do
            (println "Error: Invalid input detected:")
            (doseq [e errors]
              (println (str "  • " (:grade-str e) " / " (:credits-str e) " — " (:reason e))))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем GPA
          :else
          (let [entries (mapv (fn [e]
                                {:grade (:grade e)
                                 :credits (:credits e)
                                 :original (:grade-str e)})
                              ok-entries)
                result (calculate-gpa entries)]
            
            (println "=== GPA Calculator ===")
            (println)
            (println "--- Courses ---")
            (println (str (pad-right "Grade" 10) " | "
                         (pad-left "Credits" 8) " | "
                         (pad-left "Points" 10)))
            (println (apply str (repeat 35 "-")))
            
            (doseq [e (:entries result)]
              (println (str (pad-right (:original e) 10) " | "
                           (pad-left (:credits e) 8) " | "
                           (pad-left (fmt (* (:grade e) (:credits e))) 10))))
            
            (println (apply str (repeat 35 "-")))
            (println (str (pad-right "TOTAL" 10) " | "
                         (pad-left (:total-credits result) 8) " | "
                         (pad-left (fmt (:weighted-sum result)) 10)))
            (println)
            
            (println "--- Result ---")
            (println (str (pad-left "Total credits:" 20) " " (:total-credits result)))
            (println (str (pad-left "Weighted sum:" 20) " " (fmt (:weighted-sum result))))
            (println (str (pad-left "GPA:" 20) " " (fmt (:gpa result))))
            (println (str (pad-left "Status:" 20) " " (gpa-label (:gpa result))))
            
            (js/process.exit 0)))))))
