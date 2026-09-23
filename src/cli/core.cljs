;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

(defn main []
  (do 
    (println "Hello Clojure")
	(js/process.exit 0)
	))
