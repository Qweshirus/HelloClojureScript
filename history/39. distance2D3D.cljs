(ns cli.core)

;; --- Вычисление ---

(defn distance-2d [x1 y1 x2 y2]
  "Вычисляет расстояние между двумя точками в 2D пространстве.
   Формула: d = √((x2-x1)² + (y2-y1)²)"
  (js/Math.sqrt (+ (js/Math.pow (- x2 x1) 2)
                   (js/Math.pow (- y2 y1) 2))))

(defn distance-3d [x1 y1 z1 x2 y2 z2]
  "Вычисляет расстояние между двумя точками в 3D пространстве.
   Формула: d = √((x2-x1)² + (y2-y1)² + (z2-z1)²)"
  (js/Math.sqrt (+ (js/Math.pow (- x2 x1) 2)
                   (js/Math.pow (- y2 y1) 2)
                   (js/Math.pow (- z2 z1) 2))))

;; --- Парсинг аргументов ---

(def ^:private valid-flags #{:x1 :y1 :z1 :x2 :y2 :z2})

(defn parse-args [args]
  "Парсит аргументы командной строки."
  (loop [remaining args
         result {:x1 nil :y1 nil :z1 nil :x2 nil :y2 nil :z2 nil
                 :unknown-flags []
                 :positional []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (.startsWith current "--")
          (let [flag-name (keyword (subs current 2))]
            (if (contains? valid-flags flag-name)
              (recur (rest remaining) result flag-name)
              (recur (rest remaining)
                     (update result :unknown-flags conj current)
                     nil)))

          :else
          (if current-flag
            (if (contains? valid-flags current-flag)
              (recur (rest remaining)
                     (assoc result current-flag current)
                     nil)
              (recur (rest remaining) result nil))
            (recur (rest remaining)
                   (update result :positional conj current)
                   nil)))))))

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-number [s]
  "Пытается преобразовать строку в число."
  (let [n (js/Number s)]
    (if (js/isNaN n)
      {:ok false :raw s :reason "must be a valid number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage:")
  (println "  node target/main.js --x1 <v> --y1 <v> --x2 <v> --y2 <v>           # 2D distance")
  (println "  node target/main.js --x1 <v> --y1 <v> --z1 <v> --x2 <v> --y2 <v> --z2 <v>  # 3D distance")
  (println)
  (println "Calculates Euclidean distance between two points.")
  (println)
  (println "Options:")
  (println "  --x1 <value>    X coordinate of first point")
  (println "  --y1 <value>    Y coordinate of first point")
  (println "  --z1 <value>    Z coordinate of first point (for 3D)")
  (println "  --x2 <value>    X coordinate of second point")
  (println "  --y2 <value>    Y coordinate of second point")
  (println "  --z2 <value>    Z coordinate of second point (for 3D)")
  (println)
  (println "Note: Either provide all 4 coordinates (2D) or all 6 coordinates (3D).")
  (println)
  (println "Examples:")
  (println "  node target/main.js --x1 0 --y1 0 --x2 3 --y2 4          # 2D: d = 5")
  (println "  node target/main.js --x1 0 --y1 0 --z1 0 --x2 1 --y2 2 --z2 2  # 3D: d = 3"))

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
  "Гарантированно завершает процесс Node.js."
  (js/process.exit code))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        x1-str (:x1 parsed)
        y1-str (:y1 parsed)
        z1-str (:z1 parsed)
        x2-str (:x2 parsed)
        y2-str (:y2 parsed)
        z2-str (:z2 parsed)
        unknown-flags (:unknown-flags parsed)
        positional (:positional parsed)]
    
    (cond
      ;; Есть неизвестные флаги
      (seq unknown-flags)
      (do
        (println (str "Error: Unknown option(s): " (clojure.string/join ", " unknown-flags)))
        (println)
        (print-usage)
        (exit 1))

      ;; Есть неименованные параметры
      (seq positional)
      (do
        (println (str "Error: Unexpected positional argument(s): " (clojure.string/join ", " positional)))
        (println "       This program only accepts named parameters: --x1, --y1, --z1, --x2, --y2, --z2")
        (println)
        (print-usage)
        (exit 1))

      ;; Обязательные координаты не указаны (x1, y1, x2, y2)
      (or (nil? x1-str) (nil? y1-str) (nil? x2-str) (nil? y2-str))
      (do
        (println "Error: Minimum required coordinates: --x1, --y1, --x2, --y2")
        (println)
        (print-usage)
        (exit 1))

      ;; Несоответствие Z-координат (должны быть либо обе, либо ни одной)
      (not= (some? z1-str) (some? z2-str))
      (do
        (println "Error: Z coordinates must be both provided or both omitted for 3D calculation.")
        (println)
        (print-usage)
        (exit 1))

      :else
      ;; Валидация чисел
      (let [coords-2d [x1-str y1-str x2-str y2-str]
            coords-3d (when z1-str [z1-str z2-str])
            all-coords (concat coords-2d coords-3d)
            
            parsed-coords (mapv parse-number all-coords)
            errors (filterv #(not (:ok %)) parsed-coords)]
        
        (cond
          ;; Есть ошибки валидации
          (seq errors)
          (do
            (println "Error: Invalid coordinate value(s):")
            (doseq [e errors]
              (println (str "  • " (:raw e) " — " (:reason e))))
            (println)
            (print-usage)
            (exit 1))

          ;; Всё ок — вычисляем
          :else
          (let [values (mapv :value parsed-coords)
                [x1 y1 x2 y2] (take 4 values)
                z1 (when z1-str (nth values 4))
                z2 (when z2-str (nth values 5))
                is-3d? (some? z1-str)
                distance (if is-3d?
                           (distance-3d x1 y1 z1 x2 y2 z2)
                           (distance-2d x1 y1 x2 y2))]
            
            (println "=== Distance Calculator ===")
            (println)
            (println "--- Mode ---")
            (println (str "  " (if is-3d? "3D Space" "2D Plane")))
            (println)
            
            (println "--- Point 1 ---")
            (println (str (pad-left "X:" 15) " " (fmt6 x1)))
            (println (str (pad-left "Y:" 15) " " (fmt6 y1)))
            (when is-3d?
              (println (str (pad-left "Z:" 15) " " (fmt6 z1))))
            (println)
            
            (println "--- Point 2 ---")
            (println (str (pad-left "X:" 15) " " (fmt6 x2)))
            (println (str (pad-left "Y:" 15) " " (fmt6 y2)))
            (when is-3d?
              (println (str (pad-left "Z:" 15) " " (fmt6 z2))))
            (println)
            
            (println "--- Result ---")
            (println (str (pad-left "Distance:" 15) " " (fmt6 distance)))
            (println)
            
            (println "--- Formula ---")
            (if is-3d?
              (do
                (println "  d = √((x2-x1)² + (y2-y1)² + (z2-z1)²)")
                (println (str "  d = √((" (fmt x2) "-" (fmt x1) ")² + (" 
                             (fmt y2) "-" (fmt y1) ")² + (" 
                             (fmt z2) "-" (fmt z1) ")²)"))
                (println (str "  d = √(" (fmt (js/Math.pow (- x2 x1) 2)) " + " 
                             (fmt (js/Math.pow (- y2 y1) 2)) " + " 
                             (fmt (js/Math.pow (- z2 z1) 2)) ")")))
              (do
                (println "  d = √((x2-x1)² + (y2-y1)²)")
                (println (str "  d = √((" (fmt x2) "-" (fmt x1) ")² + (" 
                             (fmt y2) "-" (fmt y1) ")²)"))
                (println (str "  d = √(" (fmt (js/Math.pow (- x2 x1) 2)) " + " 
                             (fmt (js/Math.pow (- y2 y1) 2)) ")"))))
            
            (exit 0)))))))
