{
 :repl {:plugins [[cider/cider-nrepl "0.16.0"]]}
 :user {:dependencies [[pjstadig/humane-test-output "0.8.3"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[com.jakemccrary/lein-test-refresh "0.22.0"]
                  [jonase/eastwood "0.2.5"]
                  [lein-kibit "0.1.5" :exclusions [org.clojure/clojure]]]
        :signing {:gpg-key "843BBC8AF7927761DF290561E7FC3852D9E58138"}
        :test-refresh {:notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                       :quiet true
                       :changes-only true}}}
