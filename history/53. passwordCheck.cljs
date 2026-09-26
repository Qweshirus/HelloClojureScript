(ns cli.core)

;; --- Password Analysis ---

(defn check-password-strength [password]
  "Analyzes password strength based on multiple criteria."
  (let [length (count password)
        has-lowercase (boolean (re-find #"[a-z]" password))
        has-uppercase (boolean (re-find #"[A-Z]" password))
        has-digit (boolean (re-find #"[0-9]" password))
        has-special (boolean (re-find #"[^a-zA-Z0-9]" password))
        
        ;; Calculate score
        score (cond-> 0
                (>= length 8) inc
                (>= length 12) inc
                (>= length 16) inc
                has-lowercase inc
                has-uppercase inc
                has-digit inc
                has-special inc)
        
        ;; Determine strength level
        strength (cond
                   (< score 3) "Very Weak"
                   (< score 5) "Weak"
                   (< score 6) "Moderate"
                   (< score 7) "Strong"
                   :else "Very Strong")
        
        ;; Recommendations
        recommendations (cond-> []
                          (< length 8) (conj "Use at least 8 characters")
                          (< length 12) (conj "Use 12+ characters for better security")
                          (not has-lowercase) (conj "Add lowercase letters (a-z)")
                          (not has-uppercase) (conj "Add uppercase letters (A-Z)")
                          (not has-digit) (conj "Add digits (0-9)")
                          (not has-special) (conj "Add special characters (!@#$%)"))]
    
    {:password password
     :length length
     :has-lowercase has-lowercase
     :has-uppercase has-uppercase
     :has-digit has-digit
     :has-special has-special
     :score score
     :strength strength
     :recommendations recommendations}))

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

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --password <password>")
  (println)
  (println "Analyzes password strength.")
  (println)
  (println "Options:")
  (println "  --password <pwd>    Password to analyze (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --password \"abc123\"")
  (println "  node target/main.js --password \"MyP@ssw0rd2024\"")
  (println "  node target/main.js --password \"SuperSecure!Pass#2024\""))

(defn bool-to-str [b]
  (if b "Yes" "No"))

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
        password-str (:password parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println)
        (print-usage)
        (exit 1))

      (nil? password-str)
      (do
        (println "Error: --password is required.")
        (println)
        (print-usage)
        (exit 1))

      (empty? password-str)
      (do
        (println "Error: Password cannot be empty.")
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [result (check-password-strength password-str)]
        
        (println "=== Password Strength Checker ===")
        (println)
        (println "--- Analysis ---")
        (println (str (pad-left "Password:" 20) " \"" (:password result) "\""))
        (println (str (pad-left "Length:" 20) " " (:length result) " characters"))
        (println)
        (println "--- Character Checks ---")
        (println (str (pad-left "Lowercase (a-z):" 20) " " (bool-to-str (:has-lowercase result))))
        (println (str (pad-left "Uppercase (A-Z):" 20) " " (bool-to-str (:has-uppercase result))))
        (println (str (pad-left "Digits (0-9):" 20) " " (bool-to-str (:has-digit result))))
        (println (str (pad-left "Special chars:" 20) " " (bool-to-str (:has-special result))))
        (println)
        (println "--- Score ---")
        (println (str (pad-left "Score:" 20) " " (:score result) " / 7"))
        (println (str (pad-left "Strength:" 20) " " (:strength result)))
        (println)
        
        (if (seq (:recommendations result))
          (do
            (println "--- Recommendations ---")
            (doseq [rec (:recommendations result)]
              (println (str "  • " rec))))
          (println "  ✓ Password meets all security recommendations"))
        
        (exit 0)))))
