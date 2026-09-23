;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Работа с файлами ---

(defn read-file [path]
  "Читает текстовый файл и возвращает его содержимое как строку.
   Бросает исключение, если файл не найден."
  (let [fs (js/require "fs")]
    (.readFileSync fs path "utf8")))

;; --- Анализ текста ---

(defn get-words [text]
  "Извлекает все слова из текста.
   Словом считается последовательность непробельных символов."
  (re-seq #"\S+" text))

(defn analyze-text [text]
  "Анализирует текст и возвращает карту со статистикой:
   {:char-count :word-count :words :shortest :longest :avg-length}"
  (let [char-count (count text)
        words (get-words text)
        word-count (count words)]
    (if (zero? word-count)
      {:char-count char-count
       :word-count 0
       :words []
       :shortest nil
       :longest nil
       :avg-length 0}
      (let [lengths (map count words)
            total-length (reduce + lengths)
            shortest (apply min-key count words)
            longest (apply max-key count words)
            avg-length (/ total-length word-count)]
        {:char-count char-count
         :word-count word-count
         :words words
         :shortest shortest
         :longest longest
         :avg-length avg-length}))))

;; --- CLI ---

(defn get-cli-args [args]
  "Возвращает список аргументов командной строки."
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  (println "Usage: node target/main.js <file-path>")
  (println)
  (println "Arguments:")
  (println "  <file-path>    Path to the text file to analyze"))

(defn round2 [n]
  (js/parseFloat (.toFixed n 2)))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        file-path (first js-args)]
    
    (cond
      ;; Файл не указан
      (nil? file-path)
      (do
        (println "Error: File path is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (try
        (let [text (read-file file-path)
              stats (analyze-text text)]
          (println (str "File: " file-path))
          (println (str "Characters:     " (:char-count stats)))
          (println (str "Words:          " (:word-count stats)))
          
          (if (zero? (:word-count stats))
            (println "No words found in the file.")
            (do
              (println (str "Shortest word:  \"" (:shortest stats) 
                           "\" (" (count (:shortest stats)) " chars)"))
              (println (str "Longest word:   \"" (:longest stats) 
                           "\" (" (count (:longest stats)) " chars)"))
              (println (str "Average length: " (round2 (:avg-length stats)) " chars"))))
          
          (js/process.exit 0))
        
        (catch :default e
          (println (str "Error: Cannot read file '" file-path "'"))
          (println (str "Details: " (.-message e)))
          (js/process.exit 1))))))
