(ns cli.core)

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn classify-aspect-ratio [width height]
  (let [g (gcd width height)
        ratio-w (/ width g)
        ratio-h (/ height g)
        decimal-ratio (/ width height)
        classification (cond
                         (and (= ratio-w 16) (= ratio-h 9)) "16:9 - Widescreen (HD/FHD/4K)"
                         (and (= ratio-w 4) (= ratio-h 3)) "4:3 - Classic (Old TV/Monitor)"
                         (and (= ratio-w 21) (= ratio-h 9)) "21:9 - Ultrawide (Cinema)"
                         (and (= ratio-w 32) (= ratio-h 9)) "32:9 - Super Ultrawide"
                         (and (= ratio-w 1) (= ratio-h 1)) "1:1 - Square"
                         (and (= ratio-w 3) (= ratio-h 2)) "3:2 - Classic Photo"
                         (and (= ratio-w 2) (= ratio-h 1)) "2:1 - Univisium"
                         (and (= ratio-w 5) (= ratio-h 4)) "5:4 - Old Monitor"
                         :else (str ratio-w ":" ratio-h " - Custom"))]
    {:ratio-w ratio-w
     :ratio-h ratio-h
     :decimal-ratio decimal-ratio
     :classification classification}))

(defn parse-args [args]
  (loop [remaining args result {} current-key nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (recur (rest remaining) (assoc result (keyword (subs current 2)) nil) (keyword (subs current 2)))
          current-key
          (recur (rest remaining) (assoc result current-key current) nil)
          :else
          (recur (rest remaining) (update result :extra-positional conj current) nil))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn parse-positive-integer [s]
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "value is empty"}
    (let [n (js/Number s)]
      (if (or (js/isNaN n) (<= n 0) (not (js/Number.isInteger n)))
        {:ok false :raw s :reason "must be a positive integer"}
        {:ok true :value n}))))

(defn print-usage []
  (println "Usage: node target/main.js --width <px> --height <px>")
  (println)
  (println "Classifies screen aspect ratio.")
  (println)
  (println "Options:")
  (println "  --width <px>    Screen width in pixels (required)")
  (println "  --height <px>   Screen height in pixels (required)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --width 1920 --height 1080")
  (println "  node target/main.js --width 3440 --height 1440")
  (println "  node target/main.js --width 1024 --height 768"))

(defn fmt [n] (.toFixed n 2))
(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width) s
        (str (apply str (repeat (- width (count s)) " ")) s))))
(defn exit [code] (js/process.exit code))

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        width-str (:width parsed)
        height-str (:height parsed)
        extra-positional (:extra-positional parsed)]
    (cond
      (seq extra-positional)
      (do (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
          (println) (print-usage) (exit 1))
      (or (nil? width-str) (nil? height-str))
      (do (println "Error: Both --width and --height are required.")
          (println) (print-usage) (exit 1))
      :else
      (let [width-parsed (parse-positive-integer width-str)
            height-parsed (parse-positive-integer height-str)
            errors (cond-> []
                     (not (:ok width-parsed)) (conj (str "Invalid --width: " (:reason width-parsed)))
                     (not (:ok height-parsed)) (conj (str "Invalid --height: " (:reason height-parsed))))]
        (if (seq errors)
          (do (println "Error: Invalid parameters:")
              (doseq [e errors] (println (str "  • " e)))
              (println) (print-usage) (exit 1))
          (let [width (:value width-parsed)
                height (:value height-parsed)
                result (classify-aspect-ratio width height)]
            (println "=== Screen Aspect Ratio Classifier ===")
            (println)
            (println "--- Input ---")
            (println (str (pad-left "Resolution:" 20) " " width " × " height " px"))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "Aspect Ratio:" 20) " " (:ratio-w result) ":" (:ratio-h result)))
            (println (str (pad-left "Decimal:" 20) " " (fmt (:decimal-ratio result)) ":1"))
            (println (str (pad-left "Classification:" 20) " " (:classification result)))
            (exit 0)))))))
