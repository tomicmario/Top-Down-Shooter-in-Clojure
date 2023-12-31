(defproject game "0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.clojure-goes-fast/clj-async-profiler "1.0.4"]
                 [org.clojure/clojurescript "1.9.521"]
                 [jakarta.xml.bind/jakarta.xml.bind-api "2.3.2"]
                 [org.glassfish.jaxb/jaxb-runtime "2.3.2"]] 
  
  :plugins [[lein-cljsbuild "1.1.8"]] 

  :repl-options {:init-ns game.core}
  :main game.core
  :jvm-opts ["-XX:+UseSerialGC"
             ;"-verbose:gc" 
             "-Djdk.attach.allowAttachSelf=true"
             "-Xms128m" "-Xmx128m"]
  :profiles {:uberjar {:aot :all}}
  :aot [game.core]

  :cljsbuild {:builds [{:id "test"
                         :source-paths ["src"]
                         :compiler {:output-to "test.js" 
                                    :output-dir "out"
                                    :optimizations :none
                                    :source-map true}}]}
  )


