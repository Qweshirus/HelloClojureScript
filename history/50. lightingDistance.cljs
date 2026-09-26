;; ============================================
;; 4. Lightning Distance Calculator
;; ============================================

(ns cli.core)

;; --- Constants ---

(def ^:private speed-of-sound-m-per-s 343.0)  ;; Speed of sound in air at 20°C

;; --- Calculation ---

(defn calculate-lightning-distance [delay-seconds]
  "Calculates distance to lightning strike based on thunder delay.
   Distance = speed of sound × time delay."
  (let [distance-m (* speed-of-sound-m-per-s delay-seconds)
        distance-km (/ distance-m 1000)
        distance-miles (* distance-m 0.000621371)]
    {:distance-m distance-m
     :distance-km distance-km
     :distance-miles distance-miles
     :delay-seconds delay-seconds}))

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

(defn parse-non-negative-number [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (< n 0))
        {:ok false :raw s :reason "must be a non-negative number"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --delay <seconds>")
  (println)
  (println "Calculates distance to lightning strike based on thunder delay.")
  (println)
  (println "Options:")
  (println "  --delay <s>    Time delay between lightning and thunder in seconds (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --delay 5")
  (println "  node target/main.js --delay 10"))

(defn fmt [n]
  (.toFixed n 2))

(defn fmt3 [n]
  (.toFixed n 3))

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
        delay-str (:delay parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (nil? delay-str)
      (do
        (println "Error: --delay is required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [delay-parsed (parse-non-negative-number delay-str)
            errors (cond-> []
                     (not (:ok delay-parsed)) (conj (str "Invalid --delay: " (:reason delay-parsed))))]
        
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
          (let [delay (:value delay-parsed)
                result (calculate-lightning-distance delay)]
            
            (println "=== Lightning Distance Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Thunder delay:" 20) " " (fmt (:delay-seconds result)) " seconds"))
            (println)
            (println "--- Constants ---")
            (println (str (pad-left "Speed of sound:" 20) " " (fmt speed-of-sound-m-per-s) " m/s (at 20°C)"))
            (println)
            (println "--- Results ---")
            (println (str (pad-left "Distance:" 20) " " (fmt (:distance-m result)) " meters"))
            (println (str (pad-left "Distance:" 20) " " (fmt3 (:distance-km result)) " kilometers"))
            (println (str (pad-left "Distance:" 20) " " (fmt3 (:distance-miles result)) " miles"))
            (println)
            (println "--- Safety Note ---")
            (if (< (:distance-km result) 10)
              (println "  WARNING: Lightning is close! Seek shelter immediately!")
              (println "  Lightning is at a safe distance. Continue monitoring."))
            
            (exit 0)))))))
