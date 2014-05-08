(defproject mese-test-client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 
                 [seesaw "1.4.4"]
                 [clj-time "0.5.1"]
                 [http-kit "2.1.17"]
                 [fontselector "1.0.0"]]
  :aot [mese-client.core]
  :main mese-client.core)
