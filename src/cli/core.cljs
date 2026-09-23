;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

;; --- Работа с файловой системой ---

(defn get-all-files [dir]
  "Рекурсивно собирает все файлы в директории."
  (let [fs (js/require "fs")
        path (js/require "path")
        entries (fs.readdirSync dir)]
    (reduce (fn [acc entry]
              (let [full-path (.join path dir entry)
                    stat (fs.statSync full-path)]
                (if (.isDirectory stat)
                  (into acc (get-all-files full-path))
                  (conj acc full-path))))
            []
            entries)))

(defn read-selected-lines [file-path]
  "Читает из файла строки 1, 4, 7, 10, 13 (1-based).
   Возвращает вектор прочитанных строк."
  (let [fs (js/require "fs")
        content (.readFileSync fs file-path "utf8")
        lines (.split content "\n")
        indices [0 3 6 9 12]]
    (->> indices
         (filter #(< % (count lines)))
         (mapv #(nth lines %)))))

(defn read-lines-safe [file-path]
  "Безопасно читает выбранные строки из файла.
   Возвращает {:ok true :lines [...]} или {:ok false :error ...}."
  (try
    {:ok true :lines (read-selected-lines file-path)}
    (catch :default e
      {:ok false :error (.-message e)})))

;; --- Анализ текста ---

(defn get-words [text]
  "Извлекает все слова из текста и приводит к нижнему регистру."
  (map #(.toLowerCase %) (re-seq #"\S+" text)))

(defn analyze-words [words]
  "Анализирует список слов и возвращает статистику."
  (let [total (count words)
        freqs (frequencies words)
        unique (count freqs)]
    {:total-words total
     :unique-words unique
     :word-frequencies freqs}))

(defn sorted-frequencies [freqs]
  "Сортирует карту частот по убыванию частоты, затем по алфавиту."
  (sort-by (fn [[word cnt]] [(- cnt) word]) freqs))

;; --- CLI ---

(defn get-cli-args [args]
  (or args
      (let [argv (.-argv js/process)]
        (when (>= (count argv) 2)
          (vec (drop 2 argv))))))

(defn print-usage []
  (println "Usage: node target/main.js <directory>")
  (println)
  (println "Recursively reads lines 1, 4, 7, 10, 13 from all files")
  (println "in the given directory and prints word statistics.")
  (println)
  (println "Arguments:")
  (println "  <directory>    Path to the directory to scan"))

;; --- Точка входа ---

(defn main [& args]
  (let [js-args (get-cli-args args)
        dir-path (first js-args)]
    
    (cond
      (nil? dir-path)
      (do
        (println "Error: Directory path is required.")
        (println)
        (print-usage)
        (js/process.exit 1))

      :else
      (try
        (let [fs (js/require "fs")
              stat (fs.statSync dir-path)]
          
          (if-not (.isDirectory stat)
            (do
              (println (str "Error: '" dir-path "' is not a directory."))
              (js/process.exit 1))
            
            (let [files (get-all-files dir-path)]
              
              (if (empty? files)
                (do
                  (println (str "No files found in '" dir-path "'."))
                  (js/process.exit 0))
                
                (let [results (mapv (fn [f]
                                      (let [res (read-lines-safe f)]
                                        (assoc res :file f)))
                                    files)
                      
                      ok-results (filter :ok results)
                      err-results (filter #(not (:ok %)) results)
                      
                      all-lines (mapcat :lines ok-results)
                      all-text (clojure.string/join "\n" all-lines)
                      words (get-words all-text)
                      stats (analyze-words words)]
                  
                  ;; Вывод обработанных файлов
                  (println (str "Directory: " dir-path))
                  (println (str "Files found: " (count files)))
                  (println (str "Files read:  " (count ok-results)))
                  (println (str "Lines read per file: 1, 4, 7, 10, 13"))
                  (println)
                  
                  ;; Ошибки чтения
                  (when (seq err-results)
                    (println "Files with errors:")
                    (doseq [r err-results]
                      (println (str "  " (:file r) " : " (:error r))))
                    (println))
                  
                  ;; Список прочитанных файлов
                  (println "Processed files:")
                  (doseq [r ok-results]
                    (println (str "  " (:file r) 
                                 " (" (count (:lines r)) " lines read)")))
                  (println)
                  
                  ;; Статистика
                  (println (str "Total words:  " (:total-words stats)))
                  (println (str "Unique words: " (:unique-words stats)))
                  (println)
                  
                  (if (zero? (:total-words stats))
                    (println "No words found.")
                    (do
                      (println "Word frequencies:")
                      (doseq [[word cnt] (sorted-frequencies 
                                           (:word-frequencies stats))]
                        (println (str "  " word " : " cnt)))))
                  
                  (js/process.exit 0))))))
        
        (catch :default e
          (println (str "Error: Cannot access '" dir-path "'"))
          (println (str "Details: " (.-message e)))
          (js/process.exit 1))))))
