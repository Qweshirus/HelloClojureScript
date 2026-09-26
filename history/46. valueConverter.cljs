(ns cli.core)

;; --- Conversion Factors ---

(def ^:private conversions
  "Conversion table: [category source-unit target-unit multiplier]"
  {;; Length conversions
   :length {:m-to-ft 3.28084
            :m-to-in 39.3701
            :m-to-yd 1.09361
            :m-to-mi 0.000621371
            :km-to-mi 0.621371
            :km-to-ft 3280.84
            :cm-to-in 0.393701
            :mm-to-in 0.0393701
            :ft-to-m (/ 1 3.28084)
            :in-to-m (/ 1 39.3701)
            :yd-to-m (/ 1 1.09361)
            :mi-to-km (/ 1 0.621371)
            :mi-to-m (/ 1 0.000621371)}
   
   ;; Weight/Mass conversions
   :weight {:kg-to-lb 2.20462
            :kg-to-oz 35.274
            :g-to-oz 0.035274
            :t-to-lb 2204.62
            :lb-to-kg (/ 1 2.20462)
            :oz-to-g (/ 1 0.035274)
            :oz-to-kg (/ 1 35.274)
            :lb-to-t (/ 1 2204.62)}
   
   ;; Volume conversions
   :volume {:l-to-gal 0.264172
            :l-to-qt 1.05669
            :l-to-pt 2.11338
            :l-to-cup 4.22675
            :ml-to-floz 0.033814
            :gal-to-l (/ 1 0.264172)
            :qt-to-l (/ 1 1.05669)
            :pt-to-l (/ 1 2.11338)
            :cup-to-l (/ 1 4.22675)
            :floz-to-ml (/ 1 0.033814)}})

(defn convert-temperature [value from to]
  "Converts temperature between Celsius and Fahrenheit.
   Uses division instead of ratio literals (ClojureScript doesn't support 9/5)."
  (cond
    (and (= from "celsius") (= to "fahrenheit"))
    (+ (* value (/ 9 5)) 32)  ;; Fixed: was 9/5
    
    (and (= from "fahrenheit") (= to "celsius"))
    (* (- value 32) (/ 5 9))  ;; Fixed: was 5/9
    
    :else nil))

(defn convert-unit [value from to]
  "Converts a value between units in the same category."
  (let [key (keyword (str from "-to-" to))
        ;; Try each category
        length-mult (get-in conversions [:length key])
        weight-mult (get-in conversions [:weight key])
        volume-mult (get-in conversions [:volume key])]
    (cond
      length-mult {:value (* value length-mult) :category :length}
      weight-mult {:value (* value weight-mult) :category :weight}
      volume-mult {:value (* value volume-mult) :category :volume}
      :else nil)))

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments."
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

(defn parse-number [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (js/isNaN n)
        {:ok false :raw s :reason "must be a valid number"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --value <number> --from <unit> --to <unit>")
  (println)
  (println "Converts between metric and imperial units.")
  (println)
  (println "Options:")
  (println "  --value <number>    The value to convert (required)")
  (println "  --from <unit>       Source unit (required)")
  (println "  --to <unit>         Target unit (required)")
  (println)
  (println "Supported conversions:")
  (println "  Length:     m, km, cm, mm ↔ ft, in, yd, mi")
  (println "  Weight:     kg, g, t ↔ lb, oz")
  (println "  Volume:     l, ml ↔ gal, qt, pt, cup, floz")
  (println "  Temperature: celsius ↔ fahrenheit")
  (println)
  (println "Examples:")
  (println "  node target/main.js --value 100 --from cm --to in")
  (println "  node target/main.js --value 5 --from km --to mi")
  (println "  node target/main.js --value 70 --from kg --to lb")
  (println "  node target/main.js --value 1 --from gal --to l")
  (println "  node target/main.js --value 100 --from celsius --to fahrenheit"))

(defn fmt [n]
  (if (js/Number.isInteger n)
    (str n)
    (.toFixed n 4)))

(defn fmt2 [n]
  (.toFixed n 2))

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
        value-str (:value parsed)
        from-str (:from parsed)
        to-str (:to parsed)
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
      (or (nil? value-str) (nil? from-str) (nil? to-str))
      (do
        (println "Error: --value, --from, and --to are all required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [value-parsed (parse-number value-str)
            errors (cond-> []
                     (not (:ok value-parsed)) (conj (str "Invalid --value: " (:reason value-parsed))))]
        
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

          ;; All OK — perform conversion
          :else
          (let [value (:value value-parsed)
                from-unit from-str
                to-unit to-str
                
                ;; Check if it's temperature conversion
                is-temp? (and (contains? #{"celsius" "fahrenheit"} from-unit)
                              (contains? #{"celsius" "fahrenheit"} to-unit))
                
                ;; Try standard unit conversion
                standard-result (when (not is-temp?) (convert-unit value from-unit to-unit))
                
                ;; Try temperature conversion
                temp-result (when is-temp? (convert-temperature value from-unit to-unit))]
            
            (cond
              ;; Temperature conversion
              is-temp?
              (if temp-result
                (do
                  (println "=== Unit Converter ===")
                  (println)
                  (println "--- Input ---")
                  (println (str (pad-left "Value:" 20) " " value " °"))
                  (println (str (pad-left "From:" 20) " " (clojure.string/capitalize from-unit)))
                  (println)
                  (println "--- Output ---")
                  (println (str (pad-left "Value:" 20) " " (fmt2 temp-result) " °"))
                  (println (str (pad-left "To:" 20) " " (clojure.string/capitalize to-unit)))
                  (exit 0))
                (do
                  (println "Error: Unsupported temperature conversion.")
                  (println)
                  (print-usage)
                  (exit 1)))
              
              ;; Standard conversion
              standard-result
              (do
                (println "=== Unit Converter ===")
                (println)
                (println "--- Input ---")
                (println (str (pad-left "Value:" 20) " " (fmt value) " " from-unit))
                (println (str (pad-left "From:" 20) " " from-unit))
                (println)
                (println "--- Output ---")
                (println (str (pad-left "Value:" 20) " " (fmt (:value standard-result)) " " to-unit))
                (println (str (pad-left "To:" 20) " " to-unit))
                (println)
                (println "--- Formula ---")
                (println (str "  " (fmt value) " " from-unit " × factor = " (fmt (:value standard-result)) " " to-unit))
                (exit 0))
              
              ;; Unknown conversion
              :else
              (do
                (println (str "Error: Unsupported conversion from '" from-unit "' to '" to-unit "'"))
                (println)
                (print-usage)
                (exit 1)))))))))
