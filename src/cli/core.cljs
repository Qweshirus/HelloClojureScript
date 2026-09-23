;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

(defn print-usage []
  "Выводит подсказку по использованию приложения."
  (println "Usage: node target/main.js <text>")
  (println "Arguments:")
  (println "  <text>    The text to count words and characters in"))

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn count-words [text]
  "Подсчитывает количество слов в тексте.
   Находит все последовательности непробельных символов с помощью регулярного выражения."
  (count (re-seq #"\S+" text)))

(defn main [& args]
  "Точка входа приложения. Принимает текст и выводит статистику."
  (let [js-args (get-cli-args args)
        text (first js-args)]
    
    (if (nil? text)
      ;; Если аргумент не передан
      (do
        (println "Error: Text argument is required.")
        (print-usage)
        (js/process.exit 1))
      
      ;; Если аргумент передан
      (let [char-count (count text)
            word-count (count-words text)]
        (println (str "Text: \"" text "\""))
        (println (str "Characters: " char-count))
        (println (str "Words: " word-count))
        (js/process.exit 0)))))
