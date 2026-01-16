(defpackage :yano
  (:use :cl :djula)
  (:import-from :easy-routes #:easy-routes-acceptor)
  (:export
   :start-server
   :stop-server))

(in-package :yano)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899
  "Application port.")
