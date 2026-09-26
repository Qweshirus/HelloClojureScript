(ns cli.core)

;; --- Calculation ---

(defn calculate-pace [distance-m time-s]
  "Calculates running pace in min/km and m/s."
  (let [distance-km (/ distance-m 1000)
        time-min (/ time-s 60)
        pace-min-per-km (/ time-min distance-km)
        speed-m-per-s (/ distance-m time-s)
        pace-min (js/Math.floor pace-min-per-km)
        pace-sec (* (- pace-min-per-km pace-min) 60)]
    {:distance-km distance-km
     :time-min time-min
     :pace-min-per-km pace-min-per-km
     :speed-m-per-s speed-m-per-s
     :pace-min pace-min
     :pace-sec pace-sec}))

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
  (println "Usage: node target/main.js --distance <meters> --time <seconds>")
  (println)
  (println "Calculates running pace.")
  (println)
  (println "Options:")
  (println "  --distance <m>    Distance in meters (required)")
  (println "  --time <s>        Time in seconds (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --distance 5000 --time 1500")
  (println "  node target/main.js --distance 10000 --time 3600"))

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
                result (calculate-pace distance time)]
            
            (println "=== Running Pace Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Distance:" 20) " " (fmt distance) " m (" (fmt (:distance-km result)) " km)"))
            (println (str (pad-left "Time:" 20) " " (fmt time) " s (" (fmt (:time-min result)) " min)"))
            (println)
            (println "--- Results ---")
            (println (str (pad-left "Pace:" 20) " " (:pace-min result) " min " (fmt (:pace-sec result)) " s per km"))
            (println (str (pad-left "Speed:" 20) " " (fmt4 (:speed-m-per-s result)) " m/s"))
            (println (str (pad-left "Speed:" 20) " " (fmt (* (:speed-m-per-s result) 3.6)) " km/h"))
            
            (exit 0)))))))
