(asdf:defsystem "yano-backend"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on (:hunchentoot
               :easy-routes
               :jonathan)
  :serial t
  :components ((:module "src/backend"
                :components
                ((:file "package")
                 (:file "models")
                 (:file "routes")
                 (:file "server"))))
  :description "")
