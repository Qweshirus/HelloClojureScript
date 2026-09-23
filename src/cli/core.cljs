;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки.
   Если args переданы явно, использует их. Иначе читает из js/process.argv,
   отбрасывая первые два элемента (путь к node и путь к скрипту)."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js <your-name>")
  (println "Arguments:")
  (println "  <your-name>    Your name to greet"))

(defn main [& args]
  "Точка входа приложения. Принимает первый аргумент командной строки
   и выводит приветствие или ошибку."
    (let [js-args (get-cli-args args)
       name (first js-args)]
    (if (and nil? name (not (empty? name)))
      (do
	    (println (str "Hello, " name "!"))
		(js/process.exit 0))
      (do
        (print-usage)
        (js/process.exit 1)))))
