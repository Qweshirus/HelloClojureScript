;; npx shadow-cljs watch cli
;; node target/main.js
(ns cli.core)

(defn main []
  (let [readline (js/require "readline")
        rl (.createInterface readline
                             #js {:input js/process.stdin
                                  :output js/process.stdout})]
    (.question rl "Plese enter your name: "
               (fn [name]
                 (println (str "Hello, " name))
                 (.close rl)
                 (js/process.exit 0)))))
