(ns cli.core)

;; --- Calculation ---

(defn calculate-speed [distance-km time-h]
  "Calculates speed in km/h and m/s."
  (let [speed-kmh (/ distance-km time-h)
        speed-ms (/ (* distance-km 1000) (* time-h 3600))]
    {:speed-kmh speed-kmh
     :speed-ms speed-ms}))

;; --- Argument Parsing ---

(defn parse-args [args]
  (loop [remaining args
         result {}
         current-key nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (let [key-name (subs current 2)
                key (keyword key-name)]
            (recur (rest remaining)
                   (assoc result key nil)
                   key))
          current-key
          (recur (rest remaining)
                 (assoc result current-key current)
                 nil)
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
  (println "Usage: node target/main.js --distance <km> --time <hours>")
  (println)
  (println "Calculates speed.")
  (println)
  (println "Options:")
  (println "  --distance <km>    Distance in kilometers (required)")
  (println "  --time <hours>     Time in hours (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --distance 100 --time 2")
  (println "  node target/main.js --distance 42.2 --time 3.5"))

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
        distance-str (:distance parsed)
        time-str (:time parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (or (nil? distance-str) (nil? time-str))
      (do
        (println "Error: Both --distance and --time are required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [distance-parsed (parse-positive-number distance-str)
            time-parsed (parse-positive-number time-str)
            errors (cond-> []
                     (not (:ok distance-parsed)) (conj (str "Invalid --distance: " (:reason distance-parsed)))
                     (not (:ok time-parsed)) (conj (str "Invalid --time: " (:reason time-parsed))))]
        
        (cond
          (seq errors)
          (do
            (println "Error: Invalid parameters:")
            (doseq [e errors]
              (println (str "  • " e)))
            (println)
            (print-usage)
            (exit 1))

          :else
          (let [distance (:value distance-parsed)
                time (:value time-parsed)
                result (calculate-speed distance time)]
            
            (println "=== Speed Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Distance:" 20) " " (fmt distance) " km"))
            (println (str (pad-left "Time:" 20) " " (fmt time) " hours"))
            (println)
            (println "--- Results ---")
            (println (str (pad-left "Speed:" 20) " " (fmt (:speed-kmh result)) " km/h"))
            (println (str (pad-left "Speed:" 20) " " (fmt4 (:speed-ms result)) " m/s"))
            
            (exit 0)))))))
