(defproject zetgen "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [hiccup "1.0.5"]
                 [instaparse "1.4.10"]
                 [me.raynes/fs "1.4.6"]
                 [clj-jgit "1.0.1"]
                 [clojure.java-time "0.3.3"]]
  :main zetgen.core
  :repl-options {:init-ns zetgen.core}
  :profiles {:uberjar {:aot [zetgen.core]}})
