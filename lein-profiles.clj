{
 :repl {:plugins [[cider/cider-nrepl "0.21.1"]]}
 :user {:dependencies [[pjstadig/humane-test-output "0.9.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]
                  [jonase/eastwood "0.3.5"]
                  [lein-cloverage "1.1.1"]
                  [lein-kibit "0.1.6" :exclusions [org.clojure/clojure]]]
        :signing {:gpg-key "843BBC8AF7927761DF290561E7FC3852D9E58138"}
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :quiet true
                       :changes-only true}}}
