(ns cli.core)

(defn double-it
  "Удваивает переданное число."
  [x]
  (* 2 x))

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js <number>")
  (println "Arguments:")
  (println "  <number>    A number to double"))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn main [& args]
  "Точка входа приложения. Принимает число, удваивает его и выводит результат."
  (let [js-args (get-cli-args args)
        arg-str (first js-args)
        ;; Пытаемся преобразовать строку в число
        num-val (when arg-str (js/Number arg-str))]
    (cond
      ;; 1. Аргумент не передан или это пустая строка
      (or (nil? arg-str) (empty? arg-str))
      (do
        (print-usage)
        (js/process.exit 1))

      ;; 2. Строка не является валидным числом (результат NaN)
      (js/isNaN num-val)
      (do
        (println (str "Error: '" arg-str "' is not a valid number."))
        (print-usage)
        (js/process.exit 1))

      ;; 3. Успешный случай
      :else
      (do
        (println (str "Double " num-val " = " (double-it num-val)))
        (js/process.exit 0)))))
