(in-package :yano/proxy)


(defvar *server-socket* nil
  "Socket TCP du serveur proxy.")

(defvar *server-running* nil
  "Indique si le serveur proxy est en cours d'exécution.")

(defvar *role* nil
  "Indique l emplacement du proxy.")

(defparameter *handshake-size* 10
  "Taille du secret.")

(defparameter *global-host* nil
  "Define Oracle IP address")

(defparameter *global-port* nil
  "Define Oracle port")

(defun handshake-mode (role)
  "Retourne deux valeurs: SEND? EXPECT?"
  (cond
    ((or (eq role :alone) (string= role "alone")) (values nil nil))
    ((or (eq role :p1)    (string= role "p1"))    (values t nil))
    ((or (eq role :p2)    (string= role "p2"))    (values nil t))
    (t (values nil nil))))

(defun accept-loop (server target-host target-port)
  "Boucle d'acceptation : accepte les connexions et lance un thread par client.
Sort proprement quand *server-running* passe à NIL ou quand SERVER est fermé."
  (unwind-protect
       (loop while *server-running* do
         (handler-case
             (multiple-value-bind (client-socket client-addr client-port)
                 (sb-bsd-sockets:socket-accept server)
               (format t "[~A] CONNECT  ~A:~A -> ~A:~A (role=~A)~%"
                       (now)
                       client-addr client-port
                       target-host target-port
                       *role*)
               ;; Un thread par client
               (sb-thread:make-thread
                (lambda ()
                  (handler-case
                      (handle-client client-socket
                                     client-addr
                                     client-port
                                     target-host
                                     target-port)
                    (error (e)
                      (format t "[~A] CLIENT-ERROR ~A:~A ~A~%"
                              (now)
                              client-addr
                              client-port
                              e))))
                :name (format nil "proxy-client-~A:~A" client-addr client-port)))
           
           ;; Socket fermé => sortie propre
           (sb-bsd-sockets:socket-error () (return))
           (error (e)
             (format t "[~A] ACCEPT-ERROR ~A~%" (now) e))))
    (%safe-close server)))

;; lsof -i :<listen-port>
;; kill -9 <PID>
;;
;;./build/yano-frontend-bin
;; ./build/yano-proxy-bin 45000 "127.0.0.1" 45001 "0.0.0.0" "client"
;; ./build/yano-proxy-bin 45001 "192.188.200.55" 80 "0.0.0.0" "server"
;; ./build/yano-backend-bin 9000 "http://192.188.200.55"

(defun start-server (listen-port target-host target-port global-host global-port &key (listen-host "0.0.0.0") (role :alone))
  "Démarre le proxy TCP forward."
  (when *server-running*
    (error "Server already running"))

  (setf *global-host* global-host
        *global-port* global-port)
  
  (setf *role*
        (etypecase role
          (keyword role)
          (string (intern (string-upcase role) :keyword))))
  
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))

    ;; Réutilisation d’adresse
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)

    ;; bind robuste sur IP littérale
    (sb-bsd-sockets:socket-bind
     server
     (sb-bsd-sockets:make-inet-address listen-host)
     listen-port)

    ;; Listen
    (sb-bsd-sockets:socket-listen server 128)

    ;; Store global state
    (setf *server-socket* server
          *server-running* t)

    ;; Thread d'accept
    (sb-thread:make-thread
     (lambda ()
       (accept-loop server target-host target-port))
     :name (format nil "proxy-~A" listen-port))

    (format t "[~A] TCP proxy running on ~A:~A → ~A:~A~%"
            *role* listen-host listen-port target-host target-port)))

(defun stop-server ()
  "Arrête proprement le proxy TCP."
  (when *server-running*
    (setf *server-running* nil)
    ;; Ferme le socket serveur → débloque socket-accept
    (when *server-socket*
      (%safe-close *server-socket*)
      (setf *server-socket* nil))
    (format t "TCP proxy stopped~%")))
