(asdf:defsystem "yano-proxy"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on (:cffi)
  :serial t
  :components ((:module "../src/proxy"
                :components
                ((:file "package")
                 (:file "crypto")
                 (:file "socks")
                 (:file "handshake")
                 (:file "handle")
                 (:file "server"))))
  :description "")
