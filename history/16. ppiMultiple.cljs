;; npx shadow-cljs watch cli
;; node target/main.js
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
   Возвращает {:ok true :width w :height h :raw s} или {:ok false :raw s :reason ...}."
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
            {:ok true :width width :height height :raw s}))))))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает все значения после --size и --diagonal в векторы."
  (loop [remaining args
         result {:size [] :diagonal []}
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
                   (update result current-flag conj current)
                   current-flag)
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
      {:ok true :value n :raw s})))

;; --- CLI ---

(defn print-usage []
  (println "Usage: node target/main.js --size <WxH> [<WxH> ...] --diagonal <in> [<in> ...]")
  (println)
  (println "Calculates PPI (Pixels Per Inch) for every combination of size and diagonal.")
  (println)
  (println "Options:")
  (println "  --size <WxH> [<WxH> ...]       One or more screen resolutions (e.g. 1920x1080)")
  (println "  --diagonal <in> [<in> ...]     One or more screen diagonals in inches")
  (println)
  (println "Examples:")
  (println "  node target/main.js --size 1920x1080 --diagonal 24")
  (println "  node target/main.js --size 1920x1080 3840x2160 --diagonal 24 27")
  (println "  node target/main.js --size 2560x1440 3840x2160 --diagonal 27 32"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        size-strs (:size parsed)
        diagonal-strs (:diagonal parsed)]
    
    (cond
      ;; Нет параметров
      (and (empty? size-strs) (empty? diagonal-strs))
      (do
        (println "Error: Both --size and --diagonal are required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Нет размеров
      (empty? size-strs)
      (do
        (println "Error: At least one --size is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Нет диагоналей
      (empty? diagonal-strs)
      (do
        (println "Error: At least one --diagonal is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [;; Парсим все размеры
            sizes-parsed (mapv parse-size size-strs)
            ;; Парсим все диагонали
            diagonals-parsed (mapv parse-positive-number diagonal-strs)
            
            ;; Собираем ошибки
            size-errors (filterv #(not (:ok %)) sizes-parsed)
            diagonal-errors (filterv #(not (:ok %)) diagonals-parsed)]
        
        (cond
          ;; Есть ошибки в размерах
          (seq size-errors)
          (do
            (println "Errors in --size values:")
            (doseq [e size-errors]
              (println (str "  '" (:raw e) "' — " (:reason e))))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Есть ошибки в диагоналях
          (seq diagonal-errors)
          (do
            (println "Errors in --diagonal values:")
            (doseq [e diagonal-errors]
              (println (str "  '" (:raw e) "' — " (:reason e))))
            (println)
            (print-usage)
            (js/process.exit 1))

          ;; Всё ок — вычисляем PPI для каждой пары
          :else
          (let [valid-sizes (filter :ok sizes-parsed)
                valid-diagonals (filter :ok diagonals-parsed)
                ;; Декартово произведение: все пары (size, diagonal)
                pairs (for [s valid-sizes
                            d valid-diagonals]
                        [s d])]
            
            (println (str "Combinations: " (count valid-sizes) " size(s) × " 
                         (count valid-diagonals) " diagonal(s) = " 
                         (count pairs) " result(s)"))
            (println)
            
            (doseq [[s d] pairs]
              (let [width (:width s)
                    height (:height s)
                    diagonal (:value d)
                    ppi (calculate-ppi width height diagonal)]
                (println (str "  " width " × " height " @ " diagonal "\"  →  PPI: " (round2 ppi)))))
            
            (js/process.exit 0)))))))
