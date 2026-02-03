(in-package :yano/oracle)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 10000
  "Application port.")

(defun start-server (&key (port *port*))
  "Start the server"
  (format t "~&Starting the web server on port ~a~&" port)
  (setf *server* (make-instance 'easy-routes-acceptor
                                :document-root (merge-pathnames #p"static/" (uiop:getcwd))
                                :port port
                                :address "127.0.0.1"))
  (tbnl:start *server*))


(defun stop-server ()
  "Stop the server"
  (when *server*
    (progn
      (tbnl:stop *server*)
      (format t "~&Stopping the web server~&")
      (setf *server* nil))))
