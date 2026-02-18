(in-package :yano/oracle)

(defvar *global-server-socket* nil)
(defvar *global-server-running* nil)

;; (defvar *server* nil
;;   "Server instance (Hunchentoot acceptor).")

(defparameter *port* 10000
  "Application port.")


(defun write-ascii-line (stream line)
  (loop for ch across line do
    (write-byte (char-code ch) stream))
  (write-byte 10 stream)
  (finish-output stream))

(defun read-ascii-line (stream &key (max 512))
  "Lit jusqu'à \\n, retourne une string (sans \\n). EOF => string partielle (ou vide)."
  (let ((chars '())
        (n 0))
    (loop
      (when (>= n max)
        (return (coerce (nreverse chars) 'string)))
      (let ((b (read-byte stream nil :eof)))
        (when (eq b :eof)
          (return (coerce (nreverse chars) 'string)))
        (cond
          ((= b 10) ; \n
           (return (coerce (nreverse chars) 'string)))
          ((= b 13) ; \r (on ignore)
           nil)
          (t
           (push (code-char b) chars)
           (incf n)))))))

(defun authorize-line-p (line)
  "Accepte: hello <role> <conn-id> <peer...> <peer-port>"
  (let* ((parts (remove "" (uiop:split-string line :separator " ")
                        :test #'string=))
         (n (length parts)))
    (and (>= n 5)
         (string= (first parts) "hello")
         (member (second parts) '("p1" "p2" "alone") :test #'string=)
         (> (length (third parts)) 0)
         ;; dernier champ = port
         (ignore-errors
          (let ((p (parse-integer (nth (1- n) parts) :junk-allowed nil)))
            (and (<= 1 p) (<= p 65535)))))))

(defun handle-global-client (client-socket)
  (let ((s nil))
    (unwind-protect
         (progn
           (setf s (sb-bsd-sockets:socket-make-stream client-socket
                                                      :input t :output t
                                                      :element-type '(unsigned-byte 8)
                                                      :timeout nil
                                                      :buffering :none))
           (let ((line (read-ascii-line s)))
             ;; log côté serveur global
             (format t "[GLOBAL] recv: ~S~%" line)

             (if (authorize-line-p line)
                 (write-ascii-line s "ok not")
                 ;; (write-ascii-line s "ok xor 42")
                 (write-ascii-line s "deny"))))
      (ignore-errors (close s))
      (ignore-errors (close client-socket)))))

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
    (when *global-server-socket*
      (ignore-errors (close *global-server-socket*))
      (setf *global-server-socket* nil))
    (format t "[GLOBAL] stopped~%")
    t))
