(asdf:defsystem "yano"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on (:hunchentoot
               :easy-routes
               :djula
               :drakma
               :babel
               :jonathan
               :cffi)
  :serial t
  :components ((:module "../src"
                :components
                ((:module "frontend"
                  :components
                  ((:file "package")
                   (:file "models")
                   (:file "routes")
                   (:file "server")))
                 (:module "proxy"
                  :components
                  ((:file "package")
                   (:file "crypto")
                   (:file "socks")
                   (:file "handshake")
                   (:file "handle")
                   (:file "server")))
                 (:module "backend"
                  :components
                  ((:file "package")
                   (:file "models")
                   (:file "routes")
                   (:file "server")))
                 (:module "oracle"
                  :components
                  ((:file "package")
                   (:file "handle")
                   (:file "server"))))))
  :description "")
