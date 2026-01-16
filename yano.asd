(asdf:defsystem "yano"
  :version "0.0.1"
  :author "spike spiegel"
  :license "GPL-3.0 license"
  :depends-on (:hunchentoot
               :easy-routes
               :djula)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "")
