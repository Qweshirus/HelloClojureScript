(ns cli.core)

;; --- Вычисление PPI ---

(defn calculate-ppi [width height diagonal]
  "Вычисляет плотность пикселей (PPI) по разрешению и диагонали экрана.
   PPI = sqrt(width² + height²) / diagonal"
  (let [diagonal-pixels (js/Math.sqrt (+ (* width width) (* height height)))]
    (/ diagonal-pixels diagonal)))

;; --- Парсинг размера экрана ---

(defn parse-size [s]
  "Парсит строку размера экрана формата WIDTHxHEIGHT (например, 1920x1080).
   Возвращает {:ok true :width w :height h} или {:ok false :raw s :reason ...}."
  (if (or (nil? s) (empty? s))
    {:ok false :raw s :reason "size is empty"}
    (let [matches (re-matches #"^(\d+)\s*[xX]\s*(\d+)$" s)]
      (if (nil? matches)
        {:ok false :raw s :reason "expected format WIDTHxHEIGHT (e.g. 1920x1080)"}
        (let [width (js/parseInt (nth matches 1) 10)
              height (js/parseInt (nth matches 2) 10)]
          (cond
            (or (js/isNaN width) (js/isNaN height))
            {:ok false :raw s :reason "width and height must be integers"}

            (or (<= width 0) (<= height 0))
            {:ok false :raw s :reason "dimensions must be positive"}

            :else
            {:ok true :width width :height height}))))))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после флагов --size и --diagonal."
  (loop [remaining args
         result {:size nil :diagonal nil}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--size")
          (recur (rest remaining) result :size)

          (= current "--diagonal")
          (recur (rest remaining) result :diagonal)

          :else
          (if current-flag
            (recur (rest remaining)
                   (assoc result current-flag current)
                   nil)
            (recur (rest remaining) result nil)))))))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-positive-number [s]
  "Пытается преобразовать строку в положительное число."
  (let [n (js/Number s)]
    (if (or (js/isNaN n) (<= n 0))
      {:ok false :raw s :reason "must be a positive number"}
      {:ok true :value n})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --size <WIDTHxHEIGHT> --diagonal <inches>")
  (println)
  (println "Calculates PPI (Pixels Per Inch) of a screen.")
  (println)
  (println "Options:")
  (println "  --size <WIDTHxHEIGHT>   Screen resolution in pixels (e.g. 1920x1080)")
  (println "  --diagonal <inches>     Screen diagonal in inches (positive number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --size 1920x1080 --diagonal 24")
  (println "  node target/main.js --size 3840x2160 --diagonal 27")))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        size-str (:size parsed)
        diagonal-str (:diagonal parsed)]
    
    (cond
      ;; Не все параметры указаны
      (or (nil? size-str) (nil? diagonal-str))
      (do
        (println "Error: Both parameters are required: --size and --diagonal.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [size-parsed (parse-size size-str)
            diagonal-parsed (parse-positive-number diagonal-str)]
        
        (cond
          ;; Ошибка в размере
          (not (:ok size-parsed))
          (do
            (println (str "Error: Invalid size '" (:raw size-parsed) "' — " (:reason size-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Ошибка в диагонали
          (not (:ok diagonal-parsed))
          (do
            (println (str "Error: Invalid diagonal '" (:raw diagonal-parsed) "' — " (:reason diagonal-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем PPI
          :else
          (let [width (:width size-parsed)
                height (:height size-parsed)
                diagonal (:value diagonal-parsed)
                ppi (calculate-ppi width height diagonal)]
            (println (str "Resolution:  " width " × " height " px"))
            (println (str "Diagonal:    " diagonal " inches"))
            (println (str "PPI:         " (round2 ppi)))
            (js/process.exit 0)))))))
