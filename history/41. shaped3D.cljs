(ns cli.core)

;; --- Constants ---

(def ^:private PI js/Math.PI)

;; --- 3D Shape Formulas ---

(defn cube [side]
  "Cube: side a"
  {:volume (* side side side)
   :surface-area (* 6 side side)
   :params {:side side}})

(defn rectangular-prism [width height depth]
  "Rectangular Prism: width w, height h, depth d"
  {:volume (* width height depth)
   :surface-area (* 2 (+ (* width height) (* width depth) (* height depth)))
   :params {:width width :height height :depth depth}})

(defn prism [base-area base-perimeter height]
  "Prism: base area, base perimeter, height"
  {:volume (* base-area height)
   :surface-area (+ (* 2 base-area) (* base-perimeter height))
   :params {:base-area base-area :base-perimeter base-perimeter :height height}})

(defn pyramid [base-area base-perimeter height slant-height]
  "Pyramid: base area, base perimeter, height, slant height"
  {:volume (* (/ 1 3) base-area height)
   :surface-area (+ base-area (* 0.5 base-perimeter slant-height))
   :params {:base-area base-area :base-perimeter base-perimeter :height height :slant-height slant-height}})

(defn cylinder [radius height]
  "Cylinder: radius r, height h"
  {:volume (* PI radius radius height)
   :surface-area (* 2 PI radius (+ radius height))
   :params {:radius radius :height height}})

(defn cone [radius height]
  "Cone: radius r, height h"
  (let [slant (js/Math.sqrt (+ (* radius radius) (* height height)))]
    {:volume (* (/ 1 3) PI radius radius height)
     :surface-area (* PI radius (+ radius slant))
     :params {:radius radius :height height :slant-height slant}}))

(defn sphere [radius]
  "Sphere: radius r"
  {:volume (* (/ 4 3) PI radius radius radius)
   :surface-area (* 4 PI radius radius)
   :params {:radius radius}})

(defn torus [R r]
  "Torus: major radius R, minor radius r"
  {:volume (* 2 PI PI R r r)
   :surface-area (* 4 PI PI R r)
   :params {:major-radius R :minor-radius r}})

(defn truncated-cone [R r h]
  "Truncated Cone: radius1 R, radius2 r, height h"
  (let [slant (js/Math.sqrt (+ (* (- R r) (- R r)) (* h h)))]
    {:volume (* (/ 1 3) PI h (+ (* R R) (* R r) (* r r)))
     :surface-area (+ (* PI (+ (* R R) (* r r))) (* PI (+ R r) slant))
     :params {:radius1 R :radius2 r :height h :slant-height slant}}))

(defn truncated-pyramid [A1 A2 h P1 P2]
  "Truncated Pyramid: base1 area A1, base2 area A2, height h, perimeter1 P1, perimeter2 P2"
  (let [slant (js/Math.sqrt (+ (* h h) (* (/ (- P1 P2) 8) (/ (- P1 P2) 8))))]
    {:volume (* (/ 1 3) h (+ A1 A2 (js/Math.sqrt (* A1 A2))))
     :surface-area (+ A1 A2 (* 0.5 (+ P1 P2) slant))
     :params {:base1-area A1 :base2-area A2 :height h :perimeter1 P1 :perimeter2 P2 :slant-height slant}}))

(defn ellipsoid [a b c]
  "Ellipsoid: semi-axes a, b, c"
  (let [p 1.6075
        surface (* 4 PI (js/Math.pow (/ (+ (js/Math.pow a p) (js/Math.pow b p) (js/Math.pow c p)) 3) (/ 1 p)))]
    {:volume (* (/ 4 3) PI a b c)
     :surface-area surface
     :params {:semi-axis-a a :semi-axis-b b :semi-axis-c c}}))

(defn regular-octahedron [edge]
  "Regular Octahedron: edge a"
  (let [volume (/ (* (js/Math.sqrt 2) edge edge edge) 3)
        surface (* 2 (js/Math.sqrt 3) edge edge)]
    {:volume volume
     :surface-area surface
     :params {:edge edge}}))

;; --- Shapes Registry ---

(def ^:private shapes
  {"cube" {:fn cube :params [:side] :desc "Cube"}
   "rectangular-prism" {:fn rectangular-prism :params [:width :height :depth] :desc "Rectangular Prism"}
   "prism" {:fn prism :params [:base-area :base-perimeter :height] :desc "Prism"}
   "pyramid" {:fn pyramid :params [:base-area :base-perimeter :height :slant-height] :desc "Pyramid"}
   "cylinder" {:fn cylinder :params [:radius :height] :desc "Cylinder"}
   "cone" {:fn cone :params [:radius :height] :desc "Cone"}
   "sphere" {:fn sphere :params [:radius] :desc "Sphere"}
   "torus" {:fn torus :params [:R :r] :desc "Torus"}
   "truncated-cone" {:fn truncated-cone :params [:R :r :h] :desc "Truncated Cone"}
   "truncated-pyramid" {:fn truncated-pyramid :params [:A1 :A2 :h :P1 :P2] :desc "Truncated Pyramid"}
   "ellipsoid" {:fn ellipsoid :params [:a :b :c] :desc "Ellipsoid"}
   "octahedron" {:fn regular-octahedron :params [:edge] :desc "Regular Octahedron"}})

;; --- Argument Parsing ---

(defn parse-args [args]
  "Parses command line arguments.
   First positional argument is shape name.
   Rest are named shape parameters.
   Returns map {:shape \"cube\" :side \"5\" ...}"
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
  (println "  node target/main.js cube --side 5")
  (println "  node target/main.js sphere --radius 10")
  (println "  node target/main.js cylinder --radius 5 --height 10")
  (println "  node target/main.js cone --radius 3 --height 7"))

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
            
            (println "=== 3D Geometry Calculator ===")
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
            (println (str (pad-left "Volume:" 20) " " (fmt (:volume result))))
            (println (str (pad-left "Surface Area:" 20) " " (fmt (:surface-area result))))
            
            (exit 0)))))))
