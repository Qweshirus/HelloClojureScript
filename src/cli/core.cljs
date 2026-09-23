;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js <your-name>")
  (println "Arguments:")
  (println "  <your-name>    Your name to greet"))

(defn main [& args]
  "Точка входа приложения. Принимает первый аргумент командной строки
   и выводит приветствие или ошибку."
  (let [js-args (or args 
                  (when (>= (count (.-argv js/process)) 2)
                    (vec (drop 2 (.-argv js/process)))))
      name (first js-args)]
    (if (and nil? name (not (empty? name)))
      (do
	    (println (str "Hello, " name "!"))
		(js/process.exit 0))
      (do
        (print-usage)
        (js/process.exit 1)))))
