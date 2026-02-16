(asdf:defsystem "yano-oracle"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on ()
  :serial t
  :components ((:module "../src/oracle"
                :components
                ((:file "package")
                 (:file "server"))))
  :description "")
