(ns cli.core)

(defn parse-args [args]
  "Парсит аргументы командной строки и возвращает карту с найденными флагами.
   Ищет флаг --name и возвращает значение после него."
  (loop [remaining args
         result {}]
    (if (empty? remaining)
      result
      (let [current (first remaining)
            next-val (second remaining)]
        (if (= current "--name")
          (recur (drop 2 remaining) (assoc result :name next-val))
          (recur (rest remaining) result))))))

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js --name <your-name>")
  (println "Options:")
  (println "  --name    Your name to greet"))

(defn main [& args]
  "Точка входа приложения. Принимает аргументы командной строки,
   парсит их и выводит приветствие или ошибку."
  (let [js-args (or args (-> js/process .-argv (drop 2)))
        parsed (parse-args js-args)
        name (:name parsed)]
    (if name
      (println (str "Hello, " name "!"))
      (do
        (println "Error: Name is required.")
        (println)
        (print-usage)
        (js/process.exit 1)))))
