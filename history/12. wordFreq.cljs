(ns cli.core)

;; --- Работа с файлами ---

(defn read-file [path]
  "Читает текстовый файл и возвращает его содержимое как строку."
  (let [fs (js/require "fs")]
    (.readFileSync fs path "utf8")))

;; --- Анализ текста ---

(defn get-words [text]
  "Извлекает все слова из текста и приводит их к нижнему регистру.
   Словом считается последовательность непробельных символов."
  (map #(.toLowerCase %) (re-seq #"\S+" text)))

(defn analyze-text [text]
  "Анализирует текст и возвращает карту со статистикой:
   {:total-words :unique-words :word-frequencies}"
  (let [words (get-words text)
        total-words (count words)
        freqs (frequencies words)
        unique-words (count freqs)]
    {:total-words total-words
     :unique-words unique-words
     :word-frequencies freqs}))

(defn sorted-frequencies [freqs]
  "Сортирует карту частот по убыванию частоты, затем по алфавиту."
  (sort-by (fn [[word count]] [(- count) word]) freqs))

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

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        file-path (first js-args)]
    
    (cond
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
          (println (str "Total words:  " (:total-words stats)))
          (println (str "Unique words: " (:unique-words stats)))
          (println)
          
          (if (zero? (:total-words stats))
            (println "No words found in the file.")
            (do
              (println "Word frequencies:")
              (doseq [[word count] (sorted-frequencies (:word-frequencies stats))]
                (println (str "  " word " : " count)))))
          
          (js/process.exit 0))
        
        (catch :default e
          (println (str "Error: Cannot read file '" file-path "'"))
          (println (str "Details: " (.-message e)))
          (js/process.exit 1))))))
