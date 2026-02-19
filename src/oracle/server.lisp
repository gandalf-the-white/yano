(in-package :yano/oracle)

(defvar *global-server-socket* nil
  "Socket TCP du server global" )

(defvar *global-server-running* nil
  "Indique si le server global est en cours d'execution.")

(defparameter *port* 10000
  "Application port.")


(defun global-accept-loop (server)
  (unwind-protect
       (loop while *global-server-running* do
         (handler-case
             (multiple-value-bind (client-socket client-addr client-port)
                 (sb-bsd-sockets:socket-accept server)
               (declare (ignore client-addr client-port))
               (sb-thread:make-thread
                (lambda ()
                  (handle-global-client client-socket))
                :name "global-auth-client"))
           ;; socket fermé => sortie propre
           (sb-bsd-sockets:socket-error () (return))
           (error (e)
             (format t "[GLOBAL] accept error: ~A~%" e))))
    (ignore-errors (close server))))

(defun start-server (&key (host "0.0.0.0") (port *port*))
  "Démarre le serveur global (auth/validation) sur host:port."
  (when *global-server-running*
    (error "Global server already running"))

  (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)

    (sb-bsd-sockets:socket-bind server
                                (sb-bsd-sockets:make-inet-address host)
                                port)
    (sb-bsd-sockets:socket-listen server 128)

    ;; Store global state
    (setf *global-server-socket* server
          *global-server-running* t)

    (sb-thread:make-thread
     (lambda ()
       (global-accept-loop server))
     :name "global-auth-accept")

    (format t "[GLOBAL] listening on ~A:~A~%" host port)
    t))

(defun stop-server ()
  "Arrête le serveur global."
  (when *global-server-running*
    (setf *global-server-running* nil)
    ;; Ferme le socket serveur → débloque socket-accept
    (when *global-server-socket*
      (ignore-errors (close *global-server-socket*))
      (setf *global-server-socket* nil))
    (format t "[GLOBAL] stopped~%")
    t))
