(ns cli.core)

;; --- Calculation ---

(defn calculate-kinetic-energy [mass-kg velocity-ms]
  "Calculates kinetic energy: E = 0.5 × m × v²"
  (let [energy-j (* 0.5 mass-kg velocity-ms velocity-ms)
        energy-kj (/ energy-j 1000)
        energy-cal (* energy-j 0.239006)
        energy-kcal (/ energy-cal 1000)]
    {:energy-j energy-j
     :energy-kj energy-kj
     :energy-cal energy-cal
     :energy-kcal energy-kcal
     :mass-kg mass-kg
     :velocity-ms velocity-ms}))

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
  (println "Usage: node target/main.js --mass <kg> --velocity <m/s>")
  (println)
  (println "Calculates kinetic energy: E = 0.5 × m × v²")
  (println)
  (println "Options:")
  (println "  --mass <kg>        Mass in kilograms (required)")
  (println "  --velocity <m/s>   Velocity in meters per second (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --mass 10 --velocity 5")
  (println "  node target/main.js --mass 70 --velocity 3"))

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
        mass-str (:mass parsed)
        velocity-str (:velocity parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (or (nil? mass-str) (nil? velocity-str))
      (do
        (println "Error: Both --mass and --velocity are required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [mass-parsed (parse-positive-number mass-str)
            velocity-parsed (parse-positive-number velocity-str)
            errors (cond-> []
                     (not (:ok mass-parsed)) (conj (str "Invalid --mass: " (:reason mass-parsed)))
                     (not (:ok velocity-parsed)) (conj (str "Invalid --velocity: " (:reason velocity-parsed))))]
        
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
          (let [mass (:value mass-parsed)
                velocity (:value velocity-parsed)
                result (calculate-kinetic-energy mass velocity)]
            
            (println "=== Kinetic Energy Calculator ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Mass:" 20) " " (fmt (:mass-kg result)) " kg"))
            (println (str (pad-left "Velocity:" 20) " " (fmt (:velocity-ms result)) " m/s"))
            (println)
            (println "--- Formula ---")
            (println "  E = 0.5 × m × v²")
            (println (str "  E = 0.5 × " (fmt mass) " × " (fmt velocity) "²"))
            (println)
            (println "--- Results ---")
            (println (str (pad-left "Energy:" 20) " " (fmt (:energy-j result)) " J (joules)"))
            (println (str (pad-left "Energy:" 20) " " (fmt4 (:energy-kj result)) " kJ (kilojoules)"))
            (println (str (pad-left "Energy:" 20) " " (fmt (:energy-cal result)) " cal (calories)"))
            (println (str (pad-left "Energy:" 20) " " (fmt4 (:energy-kcal result)) " kcal (kilocalories)"))
            
            (exit 0)))))))
