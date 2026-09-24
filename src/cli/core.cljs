;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Функции конвертации ---

(defn rgb->yuv [r g b]
  "Конвертирует RGB в YUV (по стандарту BT.601).
   Возвращает карту {:y :u :v}."
  (let [y (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))
        u (+ (* -0.14713 r) (* -0.28886 g) (* 0.436 b))
        v (+ (* 0.615 r) (* -0.51499 g) (* -0.10001 b))]
    {:y y :u u :v v}))

(defn yuv->rgb [y u v]
  "Конвертирует YUV в RGB (по стандарту BT.601).
   Возвращает карту {:r :g :b}, значения ограничены диапазоном [0, 255]."
  (let [clamp (fn [n] (-> n (max 0) (min 255)))
        r (+ y (* 1.13983 v))
        g (+ y (* -0.39465 u) (* -0.58060 v))
        b (+ y (* 2.03211 u))]
    {:r (clamp r) :g (clamp g) :b (clamp b)}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки.
   Собирает значения после --rgb или --yuv в векторы."
  (loop [remaining args
         result {:rgb [] :yuv []}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--rgb")
          (recur (rest remaining) result :rgb)

          (= current "--yuv")
          (recur (rest remaining) result :yuv)

          :else
          (if current-flag
            (recur (rest remaining)
                   (update result current-flag conj current)
                   current-flag)
            (recur (rest remaining) result current-flag)))))))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

;; --- Валидация ---

(defn parse-numbers [strs]
  "Пытается преобразовать список строк в числа.
   Возвращает {:ok true :values [...]} или {:ok false :errors [...]}."
  (let [parsed (mapv (fn [s]
                       (let [n (js/Number s)]
                         (if (js/isNaN n)
                           {:ok false :raw s}
                           {:ok true :value n})))
                     strs)
        errors (filterv #(not (:ok %)) parsed)
        values (mapv :value (filter :ok parsed))]
    (if (seq errors)
      {:ok false :errors errors :values values}
      {:ok true :values values})))

;; --- CLI ---

(defn print-usage []
  (println "Usage:")
  (println "  node target/main.js --rgb <r> <g> <b>")
  (println "  node target/main.js --yuv <y> <u> <v>")
  (println)
  (println "Options:")
  (println "  --rgb <r> <g> <b>    Convert RGB (0-255) to YUV")
  (println "  --yuv <y> <u> <v>    Convert YUV to RGB (0-255)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --rgb 255 128 0")
  (println "  node target/main.js --yuv 179 73 -97"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        rgb-vals (:rgb parsed)
        yuv-vals (:yuv parsed)]
    
    (cond
      ;; Оба флага указаны
      (and (seq rgb-vals) (seq yuv-vals))
      (do
        (println "Error: Specify only one of --rgb or --yuv.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Ни один флаг не указан
      (and (empty? rgb-vals) (empty? yuv-vals))
      (do
        (println "Error: No conversion option specified.")
        (println)
        (print-usage)
        (js/process.exit 1))

      ;; Конвертация RGB → YUV
      (seq rgb-vals)
      (let [parsed-nums (parse-numbers rgb-vals)]
        (if (not (:ok parsed-nums))
          (do
            (doseq [e (:errors parsed-nums)]
              (println (str "Error: '" (:raw e) "' is not a valid number.")))
            (println)
            (print-usage)
            (js/process.exit 1))
          (let [values (:values parsed-nums)]
            (if (not= (count values) 3)
              (do
                (println (str "Error: RGB requires exactly 3 values, got " (count values) "."))
                (println)
                (print-usage)
                (js/process.exit 1))
              (let [[r g b] values
                    yuv (rgb->yuv r g b)]
                (println (str "RGB: (" r ", " g ", " b ")"))
                (println (str "YUV: (" (round2 (:y yuv))
                             ", " (round2 (:u yuv))
                             ", " (round2 (:v yuv)) ")"))
                (js/process.exit 0))))))

      ;; Конвертация YUV → RGB
      (seq yuv-vals)
      (let [parsed-nums (parse-numbers yuv-vals)]
        (if (not (:ok parsed-nums))
          (do
            (doseq [e (:errors parsed-nums)]
              (println (str "Error: '" (:raw e) "' is not a valid number.")))
            (println)
            (print-usage)
            (js/process.exit 1))
          (let [values (:values parsed-nums)]
            (if (not= (count values) 3)
              (do
                (println (str "Error: YUV requires exactly 3 values, got " (count values) "."))
                (println)
                (print-usage)
                (js/process.exit 1))
              (let [[y u v] values
                    rgb (yuv->rgb y u v)]
                (println (str "YUV: (" y ", " u ", " v ")"))
                (println (str "RGB: (" (round2 (:r rgb))
                             ", " (round2 (:g rgb))
                             ", " (round2 (:b rgb)) ")"))
                (js/process.exit 0)))))))))