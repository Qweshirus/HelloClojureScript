(ns cli.core)

(defn main []
  (do 
    (println "Hello Clojure")
	(js/process.exit 0)
	))
