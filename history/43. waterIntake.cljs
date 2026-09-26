(ns cli.core)

;; --- Calculation ---

(defn calculate-water-intake [weight activity climate]
  "Calculates daily water intake based on weight, activity, and climate.
   Base: 30 ml per kg of body weight.
   Activity adjustments: sedentary (0), light (+300ml), moderate (+500ml), high (+800ml).
   Climate adjustments: normal (0), hot (+500ml)."
  (let [base-ml (* weight 30)
        activity-ml (case activity
                      "sedentary" 0
                      "light" 300
                      "moderate" 500
                      "high" 800
                      0)
        climate-ml (if (= climate "hot") 500 0)
        total-ml (+ base-ml activity-ml climate-ml)]
    {:base-ml base-ml
     :activity-ml activity-ml
     :climate-ml climate-ml
     :total-ml total-ml
     :total-l (/ total-ml 1000)}))

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   Returns map {:weight \"70\" :activity \"moderate\" :climate \"normal\"}"
  (loop [remaining args
         result {:weight nil :activity "sedentary" :climate "normal"}
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

(defn parse-activity [s]
  (if (#{"sedentary" "light" "moderate" "high"} s)
    {:ok true :value s}
    {:ok false :raw s :reason "must be 'sedentary', 'light', 'moderate', or 'high'"}))

(defn parse-climate [s]
  (if (#{"normal" "hot"} s)
    {:ok true :value s}
    {:ok false :raw s :reason "must be 'normal' or 'hot'"}))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --weight <kg> [--activity <level>] [--climate <type>]")
  (println)
  (println "Calculates recommended daily water intake.")
  (println)
  (println "Options:")
  (println "  --weight <kg>       Body weight in kilograms (required)")
  (println "  --activity <level>  Activity level: sedentary, light, moderate, high (default: sedentary)")
  (println "  --climate <type>    Climate: normal, hot (default: normal)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --weight 70")
  (println "  node target/main.js --weight 70 --activity moderate")
  (println "  node target/main.js --weight 85 --activity high --climate hot"))

(defn fmt [n]
  (.toFixed n 2))

(defn fmt0 [n]
  (.toFixed n 0))

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
        activity-str (:activity parsed)
        climate-str (:climate parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      ;; Extra positional arguments
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      ;; Missing required weight
      (nil? weight-str)
      (do
        (println "Error: --weight is required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [weight-parsed (parse-positive-number weight-str)
            activity-parsed (parse-activity activity-str)
            climate-parsed (parse-climate climate-str)
            
            errors (cond-> []
                     (not (:ok weight-parsed)) (conj (str "Invalid --weight: " (:reason weight-parsed)))
                     (not (:ok activity-parsed)) (conj (str "Invalid --activity: " (:reason activity-parsed)))
                     (not (:ok climate-parsed)) (conj (str "Invalid --climate: " (:reason climate-parsed))))]
        
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
                activity (:value activity-parsed)
                climate (:value climate-parsed)
                result (calculate-water-intake weight activity climate)]
            
            (println "=== Daily Water Intake Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Weight:" 20) " " (fmt weight) " kg"))
            (println (str (pad-left "Activity Level:" 20) " " (clojure.string/capitalize activity)))
            (println (str (pad-left "Climate:" 20) " " (clojure.string/capitalize climate)))
            (println)
            (println "--- Breakdown ---")
            (println (str (pad-left "Base requirement:" 20) " " (fmt0 (:base-ml result)) " ml  (30 ml/kg)"))
            (when (pos? (:activity-ml result))
              (println (str (pad-left "Activity adjustment:" 20) " +" (fmt0 (:activity-ml result)) " ml")))
            (when (pos? (:climate-ml result))
              (println (str (pad-left "Climate adjustment:" 20) " +" (fmt0 (:climate-ml result)) " ml")))
            (println)
            (println "--- Recommended Daily Intake ---")
            (println (str (pad-left "Total:" 20) " " (fmt0 (:total-ml result)) " ml"))
            (println (str (pad-left "Total:" 20) " " (fmt (:total-l result)) " liters"))
            (println)
            (println "Note: This is a general estimation. Individual needs may vary based")
            (println "      on health conditions, pregnancy, or specific medical advice.")
            
            (exit 0)))))))
