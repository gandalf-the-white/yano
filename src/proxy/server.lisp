(in-package :yano/proxy)


(defvar *server-socket* nil
  "Socket TCP du serveur proxy.")

(defvar *server-running* nil
  "Indique si le serveur proxy est en cours d'exécution.")

(defparameter *backend-address* "192.188.200.55")

(defparameter *backend-port* 80)


;; (defun start-server (listen-port target-host target-port
;;                      &key (listen-host "0.0.0.0"))
;;   (let ((server (make-instance 'inet-socket
;;                                :type :stream
;;                                :protocol :tcp)))
;;     (socket-bind server
;;                  (host-ent-address
;;                   (get-host-by-name listen-host))
;;                  listen-port)
;;     (socket-listen server 128)

;;     (format t "Proxy listening on ~A:~A -> ~A:~A~%"
;;             listen-host listen-port target-host target-port)

;;     (loop
;;       (multiple-value-bind (client-socket addr port)
;;           (socket-accept server)
;;         (declare (ignore addr port))
;;         (make-thread
;;          (lambda ()
;;            (handle-client client-socket
;;                           target-host
;;                           target-port)))))))

(defun accept-loop (server target-host target-port)
  (setf *server-running* t)
  (unwind-protect
       (loop while *server-running* do
         (handler-case
             (multiple-value-bind (client-socket client-addr client-port)
                 (socket-accept server)
               ;; (declare (ignore addr port))

               ;; LOG connexion entrante
               (format t "[~A] CONNECT  ~A:~A -> ~A:~A~%"
                       (now)
                       client-addr client-port
                       target-host target-port)
               
               (make-thread
                (lambda ()
                  (handle-client client-socket
                                 client-addr
                                 client-port
                                 target-host
                                 target-port))))
           ;; Erreur levée quand le socket est fermé → sortie propre
           (sb-bsd-sockets:socket-error ()
             (return))))
    (ignore-errors (close server))))

(defun start-server (listen-port target-host target-port
                     &key (listen-host "0.0.0.0"))
  "Démarre le proxy TCP forward."
  (when *server-running*
    (error "Server already running"))

  (let ((server (make-instance 'inet-socket
                               :type :stream
                               :protocol :tcp)))
    (socket-bind server
                 (host-ent-address
                  (get-host-by-name listen-host))
                 listen-port)
    (socket-listen server 128)

    (setf *server-socket* server
          *server-running* t)

    (make-thread
     (lambda ()
       (accept-loop server target-host target-port)))

    (format t "TCP proxy running on ~A:~A → ~A:~A~%"
            listen-host listen-port target-host target-port)))

(defun stop-server ()
  "Arrête proprement le proxy TCP."
  (when *server-running*
    (setf *server-running* nil)

    ;; Ferme le socket serveur → débloque socket-accept
    (when *server-socket*
      (ignore-errors
       (close *server-socket*))
      (setf *server-socket* nil))

    (format t "TCP proxy stopped~%")))
