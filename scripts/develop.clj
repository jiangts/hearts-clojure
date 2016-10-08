(require '[figwheel-sidecar.repl-api :as ra]
         '[cljs.repl.node :as node]
         '[cljs.repl.browser :as browser])

(defn start []
  (ra/start-figwheel!
    {:figwheel-options { :server-port 1337 }
     :build-ids ["dev"]
     :all-builds
     [{:id "dev"
       :figwheel {:devcards true
                  :open-urls ["https://localhost:1337/cards.html"]}
       :source-paths ["src"]
       :compiler {:main       "hearts.view"
                  :asset-path "js/compiled/devcards_out"
                  :output-to  "resources/public/js/compiled/hearts_devcards.js"
                  :output-dir "resources/public/js/compiled/devcards_out"
                  :source-map-timestamp true }
       }]}))

;; Please note that when you stop the Figwheel Server http-kit throws
;; a java.util.concurrent.RejectedExecutionException, this is expected

(defn stop []
  (ra/stop-figwheel!))

(defn repl []
  (ra/cljs-repl))

(defn run []
  (start)
  (repl))
