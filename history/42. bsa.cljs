(ns cli.core)

;; --- Calculation ---

(defn calculate-bsa [height weight system]
  "Calculates Body Surface Area using the Mosteller formula.
   Metric: BSA = sqrt((height_cm * weight_kg) / 3600)
   Imperial: BSA = sqrt((height_in * weight_lbs) / 3131)"
  (let [divisor (if (= system "imperial") 3131 3600)
        bsa (js/Math.sqrt (/ (* height weight) divisor))]
    {:bsa bsa
     :system system
     :height height
     :weight weight}))

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   Returns map {:height \"175\" :weight \"70\" :system \"metric\"}"
  (loop [remaining args
         result {:height nil :weight nil :system "metric"}
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

(defn parse-system [s]
  (if (or (= s "metric") (= s "imperial"))
    {:ok true :value s}
    {:ok false :raw s :reason "must be 'metric' or 'imperial'"}))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --height <value> --weight <value> [--system <metric|imperial>]")
  (println)
  (println "Calculates Body Surface Area (BSA) using the Mosteller formula.")
  (println)
  (println "Options:")
  (println "  --height <value>    Height (cm for metric, inches for imperial)")
  (println "  --weight <value>    Weight (kg for metric, lbs for imperial)")
  (println "  --system <value>    Measurement system: 'metric' (default) or 'imperial'")
  (println)
  (println "Examples:")
  (println "  node target/main.js --height 175 --weight 70")
  (println "  node target/main.js --height 175 --weight 70 --system metric")
  (println "  node target/main.js --height 69 --weight 154 --system imperial"))

(defn fmt [n]
  (.toFixed n 2))

(defn fmt4 [n]
  (.toFixed n 4))

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
        height-str (:height parsed)
        weight-str (:weight parsed)
        system-str (:system parsed)
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
      (or (nil? height-str) (nil? weight-str))
      (do
        (println "Error: Both --height and --weight are required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [height-parsed (parse-positive-number height-str)
            weight-parsed (parse-positive-number weight-str)
            system-parsed (parse-system system-str)
            
            errors (cond-> []
                     (not (:ok height-parsed)) (conj (str "Invalid --height: " (:reason height-parsed)))
                     (not (:ok weight-parsed)) (conj (str "Invalid --weight: " (:reason weight-parsed)))
                     (not (:ok system-parsed)) (conj (str "Invalid --system: " (:reason system-parsed))))]
        
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
          (let [height (:value height-parsed)
                weight (:value weight-parsed)
                system (:value system-parsed)
                result (calculate-bsa height weight system)
                height-unit (if (= system "imperial") "in" "cm")
                weight-unit (if (= system "imperial") "lbs" "kg")
                formula (if (= system "imperial")
                          "BSA = √((height_in × weight_lbs) / 3131)"
                          "BSA = √((height_cm × weight_kg) / 3600)")]
            
            (println "=== Body Surface Area (BSA) Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Height:" 20) " " (fmt height) " " height-unit))
            (println (str (pad-left "Weight:" 20) " " (fmt weight) " " weight-unit))
            (println (str (pad-left "System:" 20) " " (clojure.string/capitalize system)))
            (println)
            (println "--- Formula (Mosteller) ---")
            (println (str "  " formula))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "BSA:" 20) " " (fmt4 (:bsa result)) " m²"))
            (println)
            (println "Note: This is a mathematical estimation. Always consult")
            (println "      a healthcare professional for medical decisions.")
            
            (exit 0)))))))
