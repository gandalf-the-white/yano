(defpackage :yano
  (:use :cl)
  (:import-from :djula #:render-template*)
  (:import-from :easy-routes #:easy-routes-acceptor #:defroute)
  (:export
   :start-server
   :stop-server))

(in-package :yano)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899
  "Application port.")
