(ns cli.core)

;; --- Calculation ---

(defn calculate-heart-rate-zones [age resting-hr]
  "Calculates target heart rate zones based on age.
   Max HR = 220 - age
   Zones are percentages of max HR.
   If resting-hr is provided, uses Karvonen formula for more accuracy."
  (let [max-hr (- 220 age)
        hrr (when resting-hr (- max-hr resting-hr))  ;; Heart Rate Reserve
        
        ;; Simple percentage method
        zone1-simple [(* max-hr 0.50) (* max-hr 0.60)]
        zone2-simple [(* max-hr 0.60) (* max-hr 0.70)]
        zone3-simple [(* max-hr 0.70) (* max-hr 0.80)]
        zone4-simple [(* max-hr 0.80) (* max-hr 0.90)]
        zone5-simple [(* max-hr 0.90) (* max-hr 1.00)]
        
        ;; Karvonen formula (more accurate if resting HR provided)
        zone1-karvonen (when hrr [(+ resting-hr (* hrr 0.50)) (+ resting-hr (* hrr 0.60))])
        zone2-karvonen (when hrr [(+ resting-hr (* hrr 0.60)) (+ resting-hr (* hrr 0.70))])
        zone3-karvonen (when hrr [(+ resting-hr (* hrr 0.70)) (+ resting-hr (* hrr 0.80))])
        zone4-karvonen (when hrr [(+ resting-hr (* hrr 0.80)) (+ resting-hr (* hrr 0.90))])
        zone5-karvonen (when hrr [(+ resting-hr (* hrr 0.90)) (+ resting-hr (* hrr 1.00))])]
    
    {:age age
     :resting-hr resting-hr
     :max-hr max-hr
     :hrr hrr
     :zones {:zone1 {:name "Very Light" :desc "Warm-up / Recovery" :simple zone1-simple :karvonen zone1-karvonen}
             :zone2 {:name "Light" :desc "Fat Burn / Endurance" :simple zone2-simple :karvonen zone2-karvonen}
             :zone3 {:name "Moderate" :desc "Aerobic / Cardio" :simple zone3-simple :karvonen zone3-karvonen}
             :zone4 {:name "Hard" :desc "Anaerobic / Performance" :simple zone4-simple :karvonen zone4-karvonen}
             :zone5 {:name "Maximum" :desc "Peak Effort / Sprint" :simple zone5-simple :karvonen zone5-karvonen}}}))

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   Returns map {:age \"30\" :resting-hr \"60\"}"
  (loop [remaining args
         result {:age nil :resting-hr nil}
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

(defn parse-positive-integer [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (<= n 0) (not (js/Number.isInteger n)))
        {:ok false :raw s :reason "must be a positive integer"}
        {:ok true :value n}))))

(defn parse-resting-hr [s]
  (if (nil? s)
    {:ok true :value nil}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (<= n 0) (not (js/Number.isInteger n)))
        {:ok false :raw s :reason "must be a positive integer"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --age <years> [--resting-hr <bpm>]")
  (println)
  (println "Calculates target heart rate zones for cardio training.")
  (println)
  (println "Options:")
  (println "  --age <years>       Your age in years (required, 1-100)")
  (println "  --resting-hr <bpm>  Resting heart rate in bpm (optional, improves accuracy)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --age 30")
  (println "  node target/main.js --age 30 --resting-hr 60")
  (println "  node target/main.js --age 45 --resting-hr 55"))

(defn fmt0 [n]
  (.toFixed n 0))

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

(defn exit [code]
  (js/process.exit code))

;; --- Entry Point ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        age-str (:age parsed)
        resting-hr-str (:resting-hr parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      ;; Extra positional arguments
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      ;; Missing required age
      (nil? age-str)
      (do
        (println "Error: --age is required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [age-parsed (parse-positive-integer age-str)
            resting-hr-parsed (parse-resting-hr resting-hr-str)
            
            errors (cond-> []
                     (not (:ok age-parsed)) (conj (str "Invalid --age: " (:reason age-parsed)))
                     (not (:ok resting-hr-parsed)) (conj (str "Invalid --resting-hr: " (:reason resting-hr-parsed))))
            
            ;; Additional age validation
            age-valid (and (:ok age-parsed) (>= (:value age-parsed) 1) (<= (:value age-parsed) 100))]
        
        (cond
          ;; Has validation errors
          (or (seq errors) (not age-valid))
          (do
            (if (not age-valid)
              (println "Error: --age must be between 1 and 100 years."))
            (when (seq errors)
              (println "Error: Invalid parameters:")
              (doseq [e errors]
                (println (str "  • " e))))
            (println)
            (print-usage)
            (exit 1))

          ;; All OK — calculate
          :else
          (let [age (:value age-parsed)
                resting-hr (:value resting-hr-parsed)
                result (calculate-heart-rate-zones age resting-hr)
                use-karvonen? (some? resting-hr)]
            
            (println "=== Target Heart Rate Zone Calculator ===")
            (println)
            (println "--- Profile ---")
            (println (str (pad-left "Age:" 20) " " age " years"))
            (println (str (pad-left "Max Heart Rate:" 20) " " (fmt0 (:max-hr result)) " bpm"))
            (if use-karvonen?
              (println (str (pad-left "Resting Heart Rate:" 20) " " resting-hr " bpm"))
              (println (str (pad-left "Resting Heart Rate:" 20) " Not provided")))
            (when use-karvonen?
              (println (str (pad-left "Heart Rate Reserve:" 20) " " (fmt0 (:hrr result)) " bpm")))
            (println)
            (println "--- Training Zones ---")
            (println (str (pad-right "Zone" 12) " | " 
                         (pad-right "Name" 14) " | " 
                         (pad-right "Target HR" 12) " | " 
                         "Description"))
            (println (apply str (repeat 60 "-")))
            
            (doseq [[zone-key zone-data] [[:zone1 :zone2 :zone3 :zone4 :zone5]]]
              (let [zone (get (:zones result) zone-key)
                    hr-range (if (and use-karvonen? (:karvonen zone))
                               (str (fmt0 (first (:karvonen zone))) "-" (fmt0 (second (:karvonen zone))))
                               (str (fmt0 (first (:simple zone))) "-" (fmt0 (second (:simple zone)))))]
                (println (str (pad-right (name zone-key) 12) " | "
                             (pad-right (:name zone) 14) " | "
                             (pad-left (str hr-range " bpm") 12) " | "
                             (:desc zone)))))
            
            (println)
            (if use-karvonen?
              (do
                (println "Note: Using Karvonen formula (more accurate with resting HR).")
                (println "      Target HR = ((Max HR - Resting HR) × Intensity%) + Resting HR"))
              (do
                (println "Note: Using simple percentage method (Max HR = 220 - age).")
                (println "      For more accuracy, provide --resting-hr.")))
            (println)
            (println "Disclaimer: Consult a healthcare professional before starting")
            (println "            any new exercise program, especially if you have")
            (println "            pre-existing health conditions.")
            
            (exit 0)))))))
