;; ============================================
;; 6. Username Length Validator
;; ============================================

(ns cli.core)

;; --- Validation ---

(defn validate-username-length [username min-length max-length]
  "Validates that username length is within the specified range."
  (let [length (count username)]
    {:username username
     :length length
     :min-length min-length
     :max-length max-length
     :is-valid (and (>= length min-length) (<= length max-length))
     :status (cond
               (< length min-length) "too short"
               (> length max-length) "too long"
               :else "valid")}))

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

;; --- Validation Helpers ---

(defn parse-positive-integer [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (<= n 0) (not (js/Number.isInteger n)))
        {:ok false :raw s :reason "must be a positive integer"}
        {:ok true :value n}))))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --username <name> [--min <length>] [--max <length>]")
  (println)
  (println "Validates username length against specified range.")
  (println)
  (println "Options:")
  (println "  --username <name>   Username to validate (required)")
  (println "  --min <length>      Minimum allowed length (optional, default: 3)")
  (println "  --max <length>      Maximum allowed length (optional, default: 20)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --username john_doe")
  (println "  node target/main.js --username ab --min 3 --max 20")
  (println "  node target/main.js --username verylongusernamehere --min 5 --max 15"))

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
        username (:username parsed)
        min-str (:min parsed)
        max-str (:max parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (nil? username)
      (do
        (println "Error: --username is required.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [min-val (or min-str "3")
            max-val (or max-str "20")
            min-parsed (parse-positive-integer min-val)
            max-parsed (parse-positive-integer max-val)
            errors (cond-> []
                     (not (:ok min-parsed)) (conj (str "Invalid --min: " (:reason min-parsed)))
                     (not (:ok max-parsed)) (conj (str "Invalid --max: " (:reason max-parsed))))]
        
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
          (let [min-length (:value min-parsed)
                max-length (:value max-parsed)]
            
            ;; Validate min <= max
            (if (> min-length max-length)
              (do
                (println (str "Error: min (" min-length ") must be less than or equal to max (" max-length ")."))
                (println)
                (print-usage)
                (exit 1)))
            
            (let [result (validate-username-length username min-length max-length)]
              
              (println "=== Username Length Validator ===")
              (println)
              (println "--- Input ---")
              (println (str (pad-left "Username:" 20) " \"" username "\""))
              (println (str (pad-left "Length:" 20) " " (:length result) " characters"))
              (println)
              (println "--- Constraints ---")
              (println (str (pad-left "Min length:" 20) " " min-length))
              (println (str (pad-left "Max length:" 20) " " max-length))
              (println)
              (println "--- Result ---")
              (println (str (pad-left "Status:" 20) " " (clojure.string/capitalize (:status result))))
              (if (:is-valid result)
                (println "  ✓ Username length is valid")
                (do
                  (println "  ✗ Username length is invalid")
                  (if (= (:status result) "too short")
                    (println (str "  → Need at least " (- min-length (:length result)) " more characters"))
                    (println (str "  → Need to remove " (- (:length result) max-length) " characters")))))
              
              (exit (if (:is-valid result) 0 1)))))))))
