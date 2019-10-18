(set-env!
 :source-paths   #{"src"}
 :resource-paths #{"resources"}
 :dependencies   '[[org.clojure/clojure "1.10.1"]
                   [org.clojure/math.numeric-tower "0.0.4"]
                   [org.clojure/math.combinatorics "0.1.5"]
                   [org.clojure/data.int-map "0.2.4"]
                   [org.clojure/data.csv "0.1.4"]

                   [com.taoensso/tufte "2.0.1"]

                   [org.bitbucket.mstrobel/procyon-core "0.5.35"]
                   [org.bitbucket.mstrobel/procyon-compilertools "0.5.35"]
                   [com.clojure-goes-fast/clj-java-decompiler "0.2.1"
                    :exclusions [org.bitbucket.mstrobel/procyon-core
                                 org.bitbucket.mstrobel/procyon-compilertools]]])
