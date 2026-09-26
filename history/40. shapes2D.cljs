(ns cli.core)

;; --- Constants ---

(def ^:private PI js/Math.PI)

;; --- Shape Formulas ---

(defn square [side]
  {:area (* side side)
   :perimeter (* 4 side)
   :params {:side side}})

(defn rectangle [width height]
  {:area (* width height)
   :perimeter (* 2 (+ width height))
   :params {:width width :height height}})

(defn parallelogram [base side height]
  {:area (* base height)
   :perimeter (* 2 (+ base side))
   :params {:base base :side side :height height}})

(defn rhombus [side height]
  {:area (* side height)
   :perimeter (* 4 side)
   :params {:side side :height height}})

(defn rhombus-diagonals [d1 d2]
  (let [side (/ (js/Math.sqrt (+ (* d1 d1) (* d2 d2))) 2)]
    {:area (/ (* d1 d2) 2)
     :perimeter (* 4 side)
     :params {:diagonal1 d1 :diagonal2 d2 :side side}}))

(defn trapezoid [a b h]
  (let [leg (js/Math.sqrt (+ (* h h) (* (/ (- b a) 2) (/ (- b a) 2))))]
    {:area (* (/ (+ a b) 2) h)
     :perimeter (+ a b (* 2 leg))
     :params {:base1 a :base2 b :height h :leg leg}}))

(defn right-trapezoid [a b h]
  (let [slant (js/Math.sqrt (+ (* h h) (* (- b a) (- b a))))]
    {:area (* (/ (+ a b) 2) h)
     :perimeter (+ a b h slant)
     :params {:base1 a :base2 b :height h :slant slant}}))

(defn isosceles-trapezoid [a b c]
  (let [h (js/Math.sqrt (- (* c c) (* (/ (- b a) 2) (/ (- b a) 2))))]
    {:area (* (/ (+ a b) 2) h)
     :perimeter (+ a b (* 2 c))
     :params {:base1 a :base2 b :leg c :height h}}))

(defn deltoid [d1 d2]
  {:area (/ (* d1 d2) 2)
   :perimeter nil
   :params {:diagonal1 d1 :diagonal2 d2}
   :note "Perimeter requires side lengths"})

(defn deltoid-sides [a b]
  {:area nil
   :perimeter (* 2 (+ a b))
   :params {:side1 a :side2 b}
   :note "Area requires diagonals or angle"})

(defn triangle-heron [a b c]
  (let [s (/ (+ a b c) 2)
        area (js/Math.sqrt (* s (- s a) (- s b) (- s c)))]
    {:area area
     :perimeter (+ a b c)
     :params {:side1 a :side2 b :side3 c :semiperimeter s}}))

(defn triangle-base-height [base height]
  {:area (* 0.5 base height)
   :perimeter nil
   :params {:base base :height height}
   :note "Perimeter requires all three sides"})

(defn right-triangle [a b]
  (let [c (js/Math.sqrt (+ (* a a) (* b b)))]
    {:area (* 0.5 a b)
     :perimeter (+ a b c)
     :params {:leg1 a :leg2 b :hypotenuse c}}))

(defn equilateral-triangle [side]
  (let [area (/ (* (js/Math.sqrt 3) side side) 4)]
    {:area area
     :perimeter (* 3 side)
     :params {:side side}}))

(defn isosceles-triangle [base leg]
  (let [height (js/Math.sqrt (- (* leg leg) (* (/ base 2) (/ base 2))))
        area (* 0.5 base height)]
    {:area area
     :perimeter (+ base (* 2 leg))
     :params {:base base :leg leg :height height}}))

(defn circle [radius]
  {:area (* PI radius radius)
   :perimeter (* 2 PI radius)
   :params {:radius radius}})

(defn semicircle [radius]
  {:area (/ (* PI radius radius) 2)
   :perimeter (+ (* PI radius) (* 2 radius))
   :params {:radius radius}})

(defn circular-sector [radius angle-degrees]
  (let [angle-rad (* angle-degrees (/ PI 180))
        arc-length (* radius angle-rad)
        area (* 0.5 radius radius angle-rad)]
    {:area area
     :perimeter (+ arc-length (* 2 radius))
     :params {:radius radius :angle-degrees angle-degrees :angle-radians angle-rad :arc-length arc-length}}))

(defn circular-segment [radius height]
  (let [d (- radius height)
        chord (* 2 (js/Math.sqrt (- (* radius radius) (* d d))))
        theta (* 2 (js/Math.acos (/ d radius)))
        area (- (* 0.5 radius radius theta) (* d (js/Math.sqrt (- (* radius radius) (* d d)))))
        arc (* radius theta)]
    {:area area
     :perimeter (+ arc chord)
     :params {:radius radius :height height :chord chord :arc arc :angle-radians theta}}))

(defn ring [r1 r2]
  (let [R (max r1 r2)
        r (min r1 r2)]
    {:area (* PI (- (* R R) (* r r)))
     :perimeter (+ (* 2 PI R) (* 2 PI r))
     :params {:outer-radius R :inner-radius r}}))

(defn half-ring [r1 r2]
  (let [R (max r1 r2)
        r (min r1 r2)]
    {:area (/ (* PI (- (* R R) (* r r))) 2)
     :perimeter (+ (* PI R) (* PI r) (* 2 (- R r)))
     :params {:outer-radius R :inner-radius r}}))

(defn ellipse [a b]
  (let [h (/ (* (- a b) (- a b)) (* (+ a b) (+ a b)))
        perimeter (* PI (+ a b) (+ 1 (* 3 h) (/ (* 10 h) (+ 4 (* 3 h)))))]
    {:area (* PI a b)
     :perimeter perimeter
     :params {:semi-major a :semi-minor b}}))

(defn regular-pentagon [side]
  (let [area (/ (* 5 side side) (* 4 (js/Math.tan (/ PI 5))))]
    {:area area
     :perimeter (* 5 side)
     :params {:side side}}))

(defn regular-hexagon [side]
  (let [area (/ (* 3 (js/Math.sqrt 3) side side) 2)]
    {:area area
     :perimeter (* 6 side)
     :params {:side side}}))

(defn regular-octagon [side]
  (let [area (* 2 (+ 1 (js/Math.sqrt 2)) side side)]
    {:area area
     :perimeter (* 8 side)
     :params {:side side}}))

(defn capsule [radius length]
  (let [area (+ (* PI radius (+ radius length)) (* 2 radius length))
        perimeter (+ (* 2 PI radius) (* 2 length))]
    {:area area
     :perimeter perimeter
     :params {:radius radius :cylinder-length length}}))

;; --- Shapes Registry ---

(def ^:private shapes
  {"square" {:fn square :params [:side] :desc "Square"}
   "rectangle" {:fn rectangle :params [:width :height] :desc "Rectangle"}
   "parallelogram" {:fn parallelogram :params [:base :side :height] :desc "Parallelogram"}
   "rhombus" {:fn rhombus :params [:side :height] :desc "Rhombus (side and height)"}
   "rhombus-diagonals" {:fn rhombus-diagonals :params [:d1 :d2] :desc "Rhombus by diagonals"}
   "trapezoid" {:fn trapezoid :params [:a :b :h] :desc "Trapezoid (bases and height)"}
   "right-trapezoid" {:fn right-trapezoid :params [:a :b :h] :desc "Right trapezoid"}
   "isosceles-trapezoid" {:fn isosceles-trapezoid :params [:a :b :c] :desc "Isosceles trapezoid"}
   "deltoid" {:fn deltoid :params [:d1 :d2] :desc "Deltoid by diagonals"}
   "deltoid-sides" {:fn deltoid-sides :params [:a :b] :desc "Deltoid by sides"}
   "triangle" {:fn triangle-heron :params [:a :b :c] :desc "Arbitrary triangle (3 sides)"}
   "triangle-bh" {:fn triangle-base-height :params [:base :height] :desc "Triangle (base and height)"}
   "right-triangle" {:fn right-triangle :params [:a :b] :desc "Right triangle (legs)"}
   "equilateral-triangle" {:fn equilateral-triangle :params [:side] :desc "Equilateral triangle"}
   "isosceles-triangle" {:fn isosceles-triangle :params [:base :leg] :desc "Isosceles triangle"}
   "circle" {:fn circle :params [:radius] :desc "Circle"}
   "semicircle" {:fn semicircle :params [:radius] :desc "Semicircle"}
   "sector" {:fn circular-sector :params [:radius :angle] :desc "Circular sector"}
   "segment" {:fn circular-segment :params [:radius :height] :desc "Circular segment"}
   "ring" {:fn ring :params [:r1 :r2] :desc "Ring (annulus)"}
   "half-ring" {:fn half-ring :params [:r1 :r2] :desc "Half ring"}
   "ellipse" {:fn ellipse :params [:a :b] :desc "Ellipse"}
   "pentagon" {:fn regular-pentagon :params [:side] :desc "Regular pentagon"}
   "hexagon" {:fn regular-hexagon :params [:side] :desc "Regular hexagon"}
   "octagon" {:fn regular-octagon :params [:side] :desc "Regular octagon"}
   "capsule" {:fn capsule :params [:radius :length] :desc "Capsule"}})

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   First positional argument is shape name.
   Rest are named shape parameters.
   Returns map {:shape \"circle\" :radius \"10\" ...}"
  (loop [remaining args
         result {}
         shape-set? false
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
                   shape-set?
                   key))
          
          ;; This is a value for previous flag
          current-key
          (recur (rest remaining)
                 (assoc result current-key current)
                 shape-set?
                 nil)
          
          ;; First positional argument is shape name
          (not shape-set?)
          (recur (rest remaining)
                 (assoc result :shape current)
                 true
                 nil)
          
          ;; Unexpected positional argument
          :else
          (recur (rest remaining)
                 (update result :extra-positional conj current)
                 shape-set?
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
  (println "Usage: node target/main.js <shape> [parameters]")
  (println)
  (println "Available shapes:")
  (doseq [[k v] (sort shapes)]
    (println (str "  " k " - " (:desc v) " (" (clojure.string/join ", " (map name (:params v))) ")")))
  (println)
  (println "Examples:")
  (println "  node target/main.js square --side 5")
  (println "  node target/main.js circle --radius 10")
  (println "  node target/main.js triangle --a 3 --b 4 --c 5")
  (println "  node target/main.js rectangle --width 10 --height 5"))

(defn fmt [n]
  (.toFixed n 4))

(defn fmt6 [n]
  (.toFixed n 6))

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
        shape-key (:shape parsed)
        extra-positional (:extra-positional parsed)]
    
    (cond
      ;; Shape not specified
      (nil? shape-key)
      (do
        (println "Error: Shape name is required as the first argument.")
        (println)
        (print-usage)
        (exit 1))

      ;; Extra positional arguments
      (seq extra-positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " extra-positional)))
        (println "       Use named parameters like --side, --radius, etc.")
        (println)
        (print-usage)
        (exit 1))

      ;; Unknown shape
      (not (contains? shapes shape-key))
      (do
        (println (str "Error: Unknown shape '" shape-key "'"))
        (println)
        (print-usage)
        (exit 1))

      :else
      (let [shape-info (get shapes shape-key)
            required-params (:params shape-info)
            shape-fn (:fn shape-info)
            
            ;; Collect parameter values
            param-results (mapv (fn [p]
                                  (let [val (get parsed p)
                                        parsed-val (parse-positive-number val)]
                                    (if (:ok parsed-val)
                                      parsed-val
                                      {:ok false 
                                       :raw val 
                                       :reason (if (nil? val)
                                                 (str "missing parameter --" (name p))
                                                 (:reason parsed-val))})))
                                required-params)
            
            errors (filterv #(not (:ok %)) param-results)]
        
        (cond
          ;; Has errors
          (seq errors)
          (do
            (println "Error: Invalid or missing parameters:")
            (doseq [e errors]
              (let [param-name (first (filter #(= (:raw e) (get parsed %)) required-params))]
                (println (str "  • --" (name (or param-name (first required-params))) 
                             " " (or (:raw e) "MISSING") " — " (:reason e)))))
            (println)
            (print-usage)
            (exit 1))

          ;; All OK — calculate
          :else
          (let [values (mapv :value param-results)
                result (apply shape-fn values)]
            
            (println "=== Geometry Calculator ===")
            (println)
            (println "--- Shape ---")
            (println (str "  " (:desc shape-info) " (" shape-key ")"))
            (println)
            
            (println "--- Parameters ---")
            (doseq [[k v] (:params result)]
              (when (number? v)
                (println (str (pad-left (name k) 20) " " (fmt6 v)))))
            (println)
            
            (println "--- Results ---")
            (if (:area result)
              (println (str (pad-left "Area:" 20) " " (fmt (:area result))))
              (println (str (pad-left "Area:" 20) " N/A (" (:note result) ")")))
            
            (if (:perimeter result)
              (println (str (pad-left "Perimeter:" 20) " " (fmt (:perimeter result))))
              (println (str (pad-left "Perimeter:" 20) " N/A (" (:note result) ")")))
            
            (exit 0)))))))
