;; ============================================
;; 3. km/h to m/s Converter
;; ============================================

(ns cli.core)

;; --- Calculation ---

(defn kmh-to-ms [kmh]
  "Converts km/h to m/s."
  (/ kmh 3.6))

(defn ms-to-kmh [ms]
  "Converts m/s to km/h."
  (* ms 3.6))

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
  (println "Usage: node target/main.js --kmh <value>")
  (println "       node target/main.js --ms <value>")
  (println)
  (println "Converts between km/h and m/s.")
  (println)
  (println "Options (exactly one required):")
  (println "  --kmh <value>    Input in km/h, output in m/s")
  (println "  --ms <value>     Input in m/s, output in km/h")
  (println)
  (println "Examples:")
  (println "  node target/main.js --kmh 72")
  (println "  node target/main.js --ms 20"))

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
        kmh-str (:kmh parsed)
        ms-str (:ms parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (and (nil? kmh-str) (nil? ms-str))
      (do
        (println "Error: One of --kmh or --ms is required.")
        (println)
        (print-usage)
        (exit 1))

      (and kmh-str ms-str)
      (do
        (println "Error: Specify only one of --kmh or --ms, not both.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (cond
        kmh-str
        (let [parsed (parse-positive-number kmh-str)]
          (if (not (:ok parsed))
            (do
              (println (str "Error: Invalid value for --kmh '" (:raw parsed) "' — " (:reason parsed)))
              (println)
              (print-usage)
              (exit 1))
            (let [kmh (:value parsed)
                  ms (kmh-to-ms kmh)]
              (println "=== Speed Converter: km/h to m/s ===")
              (println)
              (println "--- Input ---")
              (println (str (pad-left "Value:" 20) " " (fmt kmh) " km/h"))
              (println)
              (println "--- Output ---")
              (println (str (pad-left "Value:" 20) " " (fmt4 ms) " m/s"))
              (println)
              (println "--- Formula ---")
              (println (str "  " (fmt kmh) " km/h / 3.6 = " (fmt4 ms) " m/s"))
              (exit 0))))

        ms-str
        (let [parsed (parse-positive-number ms-str)]
          (if (not (:ok parsed))
            (do
              (println (str "Error: Invalid value for --ms '" (:raw parsed) "' — " (:reason parsed)))
              (println)
              (print-usage)
              (exit 1))
            (let [ms (:value parsed)
                  kmh (ms-to-kmh ms)]
              (println "=== Speed Converter: m/s to km/h ===")
              (println)
              (println "--- Input ---")
              (println (str (pad-left "Value:" 20) " " (fmt4 ms) " m/s"))
              (println)
              (println "--- Output ---")
              (println (str (pad-left "Value:" 20) " " (fmt kmh) " km/h"))
              (println)
              (println "--- Formula ---")
              (println (str "  " (fmt4 ms) " m/s × 3.6 = " (fmt kmh) " km/h"))
              (exit 0))))))))
