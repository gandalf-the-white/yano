(in-package :yano)

;;===========================================
;; S E R V E R
;;===========================================

(defun start-server (&key (port *port*))
  "Start the server"
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes-acceptor
                                :port port))
  (tbnl:start *server*))

(defun stop-server ()
  "Stop the server"
  (when *server*
    (progn
      (tbnl:stop *server*)
      (format t "~&Stopping the web server~&")
      (setf *server* nil))))
