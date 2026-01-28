(asdf:defsystem "yano-frontend"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on (:hunchentoot
               :easy-routes
               :djula
               :drakma
               :babel
               :jonathan)
  :serial t
  :components ((:module "src/frontend"
                :components
                ((:file "package")
                 (:file "models")
                 (:file "routes")
                 (:file "server"))))
  :description "")
