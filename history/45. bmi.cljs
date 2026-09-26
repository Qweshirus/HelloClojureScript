(ns cli.core)

;; --- Calculation ---

(defn calculate-bmi [weight height]
  "Calculates BMI and returns classification.
   BMI = weight (kg) / (height (m))²"
  (let [height-m (/ height 100)  ;; Convert cm to meters
        bmi (/ weight (* height-m height-m))
        classification (cond
                         (< bmi 16) "Severe Thinness"
                         (< bmi 17) "Moderate Thinness"
                         (< bmi 18.5) "Mild Thinness"
                         (< bmi 25) "Normal Weight"
                         (< bmi 30) "Overweight"
                         (< bmi 35) "Obese Class I"
                         (< bmi 40) "Obese Class II"
                         :else "Obese Class III")
        risk-level (cond
                     (< bmi 18.5) "Increased (underweight)"
                     (< bmi 25) "Average (healthy weight)"
                     (< bmi 30) "Increased (overweight)"
                     (< bmi 35) "High"
                     (< bmi 40) "Very High"
                     :else "Extremely High")]
    {:bmi bmi
     :weight weight
     :height height
     :height-m height-m
     :classification classification
     :risk-level risk-level}))

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   Returns map {:weight \"70\" :height \"175\"}"
  (loop [remaining args
         result {:weight nil :height nil}
         current-key nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          ;; This is a flag (starts with --)
          (.startsWith current "--")
          (let [key-name (subs current 2)
                key (keyword key-name)]
            (recur (rest remaining)
                   (assoc result key nil)
                   key))
          
          ;; This is a value for previous flag
          current-key
          (recur (rest remaining)
                 (assoc result current-key current)
                 nil)
          
          ;; Unexpected positional argument
          :else
          (recur (rest remaining)
                 (update result :extra-positional conj current)
                 nil))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Validation ---

(defn parse-positive-number [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (<= n 0))
        {:ok false :raw s :reason "must be a positive number"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --weight <kg> --height <cm>")
  (println)
  (println "Calculates Body Mass Index (BMI) with WHO classification.")
  (println)
  (println "Options:")
  (println "  --weight <kg>    Body weight in kilograms (required)")
  (println "  --height <cm>    Height in centimeters (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --weight 70 --height 175")
  (println "  node target/main.js --weight 85.5 --height 180"))

(defn fmt [n]
  (.toFixed n 2))

(defn fmt1 [n]
  (.toFixed n 1))

(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

(defn exit [code]
  (js/process.exit code))

;; --- Entry Point ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        weight-str (:weight parsed)
        height-str (:height parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      ;; Extra positional arguments
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      ;; Missing required parameters
      (or (nil? weight-str) (nil? height-str))
      (do
        (println "Error: Both --weight and --height are required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [weight-parsed (parse-positive-number weight-str)
            height-parsed (parse-positive-number height-str)
            
            errors (cond-> []
                     (not (:ok weight-parsed)) (conj (str "Invalid --weight: " (:reason weight-parsed)))
                     (not (:ok height-parsed)) (conj (str "Invalid --height: " (:reason height-parsed))))]
        
        (cond
          ;; Has validation errors
          (seq errors)
          (do
            (println "Error: Invalid parameters:")
            (doseq [e errors]
              (println (str "  • " e)))
            (println)
            (print-usage)
            (exit 1))

          ;; All OK — calculate
          :else
          (let [weight (:value weight-parsed)
                height (:value height-parsed)
                result (calculate-bmi weight height)]
            
            (println "=== Body Mass Index (BMI) Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Weight:" 20) " " (fmt weight) " kg"))
            (println (str (pad-left "Height:" 20) " " (fmt height) " cm (" (fmt1 (:height-m result)) " m)"))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "BMI:" 20) " " (fmt (:bmi result))))
            (println (str (pad-left "Classification:" 20) " " (:classification result)))
            (println (str (pad-left "Health Risk:" 20) " " (:risk-level result)))
            (println)
            (println "--- WHO BMI Classification ---")
            (println (str (pad-left "Severe Thinness:" 20) " BMI < 16.0"))
            (println (str (pad-left "Moderate Thinness:" 20) " BMI 16.0 - 16.9"))
            (println (str (pad-left "Mild Thinness:" 20) " BMI 17.0 - 18.4"))
            (println (str (pad-left "Normal Weight:" 20) " BMI 18.5 - 24.9"))
            (println (str (pad-left "Overweight:" 20) " BMI 25.0 - 29.9"))
            (println (str (pad-left "Obese Class I:" 20) " BMI 30.0 - 34.9"))
            (println (str (pad-left "Obese Class II:" 20) " BMI 35.0 - 39.9"))
            (println (str (pad-left "Obese Class III:" 20) " BMI ≥ 40.0"))
            (println)
            (println "Note: BMI is a screening tool and does not diagnose body fatness")
            (println "      or health. Athletes may have high BMI due to muscle mass.")
            (println "      Consult a healthcare professional for personalized advice.")
            
            (exit 0)))))))
