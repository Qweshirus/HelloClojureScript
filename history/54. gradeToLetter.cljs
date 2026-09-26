(ns cli.core)

(defn grade-to-letter [grade]
  (cond
    (>= grade 90) {:letter "A" :desc "Excellent"}
    (>= grade 80) {:letter "B" :desc "Good"}
    (>= grade 70) {:letter "C" :desc "Average"}
    (>= grade 60) {:letter "D" :desc "Below Average"}
    :else {:letter "F" :desc "Fail"}))

(defn parse-args [args]
  (loop [remaining args result {} current-key nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (recur (rest remaining) (assoc result (keyword (subs current 2)) nil) (keyword (subs current 2)))
          current-key
          (recur (rest remaining) (assoc result current-key current) nil)
          :else
          (recur (rest remaining) (update result :extra-positional conj current) nil))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn parse-number [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (js/isNaN n)
        {:ok false :raw s :reason "must be a valid number"}
        {:ok true :value n}))))

(defn print-usage []
  (println "Usage: node target/main.js --grade <number>")
  (println)
  (println "Converts numeric grade (0-100) to letter grade.")
  (println)
  (println "Options:")
  (println "  --grade <number>    Numeric grade (0-100, required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --grade 95")
  (println "  node target/main.js --grade 75"))

(defn fmt [n] (.toFixed n 2))
(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width) s
        (str (apply str (repeat (- width (count s)) " ")) s))))
(defn exit [code] (js/process.exit code))

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        grade-str (:grade parsed)
        extra-positional (:extra-positional parsed)]
    (cond
      (seq extra-positional)
      (do (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
          (println) (print-usage) (exit 1))
      (nil? grade-str)
      (do (println "Error: --grade is required.")
          (println) (print-usage) (exit 1))
      :else
      (let [grade-parsed (parse-number grade-str)]
        (if (not (:ok grade-parsed))
          (do (println (str "Error: Invalid --grade: " (:reason grade-parsed)))
              (println) (print-usage) (exit 1))
          (let [grade (:value grade-parsed)]
            (cond
              (< grade 0)
              (do (println "Error: Grade cannot be negative.")
                  (println) (print-usage) (exit 1))
              (> grade 100)
              (do (println "Error: Grade cannot exceed 100.")
                  (println) (print-usage) (exit 1))
              :else
              (let [result (grade-to-letter grade)]
                (println "=== Grade to Letter Converter ===")
                (println)
                (println "--- Input ---")
                (println (str (pad-left "Grade:" 20) " " (fmt grade)))
                (println)
                (println "--- Result ---")
                (println (str (pad-left "Letter:" 20) " " (:letter result)))
                (println (str (pad-left "Description:" 20) " " (:desc result)))
                (println)
                (println "--- Scale ---")
                (println "  A: 90-100 (Excellent)")
                (println "  B: 80-89  (Good)")
                (println "  C: 70-79  (Average)")
                (println "  D: 60-69  (Below Average)")
                (println "  F: 0-59   (Fail)")
                (exit 0)))))))))
