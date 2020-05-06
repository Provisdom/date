(def project 'provisdom/date)
(def version "1.0-alpha1")

(set-env! :resource-paths #{"src"}
          :source-paths #{"test"}
          :dependencies '[[provisdom/boot-tasks "0.7.0" :scope "test"]
                          [provisdom/test "0.2.2-SNAPSHOT" :scope "test"]
                          [adzerk/boot-test "1.1.2" :scope "test"]
                          [org.clojure/test.check "0.9.0" :scope "test"]
                          [criterium "0.4.4" :scope "test"]

                          [org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
                          [provisdom/utility-belt "0.1.1"]
                          [provisdom/math "0.3.0-SNAPSHOT"]
                          [clj-time "0.12.0"]
                          [com.taoensso/truss "1.3.6"]])

(require '[adzerk.boot-test :refer [test]]
         '[provisdom.boot-tasks.core :refer [build push-jar]])

(task-options!
  pom {:project     project
       :version     version
       :description "FIXME: write description"
       :url         "http://example/FIXME"
       :scm         {:url "https://github.com/yourname/date"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}}
  jar {:main 'provisdom.date.tick
       :file (str "date-" version "-standalone.jar")})
