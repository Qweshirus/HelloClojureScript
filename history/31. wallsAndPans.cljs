(ns cli.core)

;; --- Вычисление ---

(defn calculate-paint [length width height rate]
  "Вычисляет площадь стен и количество банок краски.
   Площадь стен = 2 × (длина + ширина) × высота.
   Количество банок округляется вверх.
   Возвращает карту с результатами."
  (let [perimeter (* 2 (+ length width))
        wall-area (* perimeter height)
        exact-banks (* wall-area rate)
        banks (js/Math.ceil exact-banks)]
    {:perimeter perimeter
     :wall-area wall-area
     :exact-banks exact-banks
     :banks banks}))

;; --- Парсинг аргументов ---

(defn parse-args [args]
  "Парсит аргументы командной строки."
  (loop [remaining args
         result {:length nil :width nil :height nil :rate nil}
         current-flag nil]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (cond
          (= current "--length")
          (recur (rest remaining) result :length)

          (= current "--width")
          (recur (rest remaining) result :width)

          (= current "--height")
          (recur (rest remaining) result :height)

          (= current "--rate")
          (recur (rest remaining) result :rate)

          :else
          (if current-flag
            (recur (rest remaining)
                   (assoc result current-flag current)
                   nil)
            (recur (rest remaining) result nil)))))))

(defn get-cli-args [args]
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
  (println "Usage: node target/main.js --length <m> --width <m> --height <m> --rate <banks/m²>")
  (println)
  (println "Calculates wall area and number of paint cans needed.")
  (println)
  (println "Options:")
  (println "  --length <m>        Room length in meters (positive number)")
  (println "  --width <m>         Room width in meters (positive number)")
  (println "  --height <m>        Room height in meters (positive number)")
  (println "  --rate <banks/m²>   Paint consumption in cans per square meter (positive number)")
  (println)
  (println "Examples:")
  (println "  node target/main.js --length 5 --width 4 --height 2.7 --rate 0.1")
  (println "  node target/main.js --length 6.5 --width 3.2 --height 2.5 --rate 0.15"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

(defn fmt [n]
  "Форматирует число с 2 знаками и разделителями тысяч."
  (let [s (.toFixed n 2)
        parts (.split s ".")
        int-part (first parts)
        dec-part (second parts)
        with-spaces (-> int-part
                        (.replace #"\B(?=(\d{3})+(?!\d))" " "))]
    (str with-spaces "." dec-part)))

(defn pad-left [s width]
  (let [s (str s)]
    (if (>= (count s) width)
      s
      (str (apply str (repeat (- width (count s)) " ")) s))))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        parsed (parse-args js-args)
        length-str (:length parsed)
        width-str (:width parsed)
        height-str (:height parsed)
        rate-str (:rate parsed)]
    
    (cond
      ;; Не все параметры указаны
      (or (nil? length-str) (nil? width-str) (nil? height-str) (nil? rate-str))
      (do
        (println "Error: All parameters are required: --length, --width, --height, --rate.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (let [length-parsed (parse-positive-number length-str)
            width-parsed (parse-positive-number width-str)
            height-parsed (parse-positive-number height-str)
            rate-parsed (parse-positive-number rate-str)]
        
        (cond
          (not (:ok length-parsed))
          (do
            (println (str "Error: Invalid length '" (:raw length-parsed) "' — " (:reason length-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok width-parsed))
          (do
            (println (str "Error: Invalid width '" (:raw width-parsed) "' — " (:reason width-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok height-parsed))
          (do
            (println (str "Error: Invalid height '" (:raw height-parsed) "' — " (:reason height-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          (not (:ok rate-parsed))
          (do
            (println (str "Error: Invalid rate '" (:raw rate-parsed) "' — " (:reason rate-parsed)))
            (println)
            (print-usage)
            (js/process.exit 1))

          :else
          (let [length (:value length-parsed)
                width (:value width-parsed)
                height (:value height-parsed)
                rate (:value rate-parsed)
                result (calculate-paint length width height rate)]
            
            (println "=== Paint Calculator ===")
            (println)
            (println "--- Room dimensions ---")
            (println (str (pad-left "Length:" 20) " " (fmt length) " m"))
            (println (str (pad-left "Width:" 20) " " (fmt width) " m"))
            (println (str (pad-left "Height:" 20) " " (fmt height) " m"))
            (println)
            (println "--- Calculations ---")
            (println (str (pad-left "Perimeter:" 20) " " (fmt (:perimeter result)) " m"))
            (println (str (pad-left "Wall area:" 20) " " (fmt (:wall-area result)) " m²"))
            (println (str (pad-left "Paint rate:" 20) " " rate " cans/m²"))
            (println (str (pad-left "Exact cans needed:" 20) " " (fmt (:exact-banks result))))
            (println)
            (println "--- Result ---")
            (println (str (pad-left "Cans to buy:" 20) " " (:banks result)))
            (when (> (:banks result) (:exact-banks result))
              (println (str (pad-left "Leftover paint:" 20) " "
                           (fmt (- (:banks result) (:exact-banks result)))
                           " cans worth")))
            
            (js/process.exit 0)))))))
