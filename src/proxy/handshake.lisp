(in-package :yano/proxy)

(defun write-ascii-line (stream line)
  (loop for ch across line do (write-byte (char-code ch) stream))
  (write-byte 10 stream) ; \n
  (finish-output stream))

(defun read-ascii-line (stream &key (max 128))
  (let ((chars '())
        (n 0))
    (loop
      (when (>= n max)
        (return (coerce (nreverse chars) 'string)))
      (let ((b (read-byte stream nil :eof)))
        (when (eq b :eof)
          (return (coerce (nreverse chars) 'string)))
        (cond
          ((= b 10)
           (return (coerce (nreverse chars) 'string))) ; \n
          (t
           (push (code-char b) chars)
           (incf n)))))))

(defun do-client-handshake (target-stream)
  (write-ascii-line target-stream "bonjour")
  (let ((reply (read-ascii-line target-stream)))
    (unless (string= reply "ok")
      (error "Handshake failed, expected \"ok\", got ~S" reply))))

(defun do-server-handshake (client-stream)
  (let ((msg (read-ascii-line client-stream)))
    (unless (string= msg "bonjour")
      (error "Unexpected handshake, got ~S" msg))
    (write-ascii-line client-stream "ok")))


;; (defmacro with-timeout ((seconds &optional (on-timeout nil)) &body body)
;;   `(sb-ext:with-timeout (,seconds ,on-timeout)
;;      ,@body))

(defun make-conn-id ()
  (format nil "~D-~D"
          (get-universal-time)
          (random most-positive-fixnum)))

(defun p1-handshake-with-p2 (target-stream conn-id)
  (write-ascii-line target-stream (format nil "bonjour ~A" conn-id))
  (let ((line (sb-ext:with-timeout 3  ; 3s
                (read-ascii-line target-stream))))
    (unless (string= line (format nil "ok ~A" conn-id))
      (error "Bad handshake from p2: ~S" line))))

(defun p2-handshake-with-p1 (client-stream)
  (let* ((line (sb-ext:with-timeout 3 
                 (read-ascii-line client-stream)))
         ;; attend "bonjour <conn-id>"
         (parts (uiop:split-string line :separator " ")))
    (unless (and (= (length parts) 2)
                 (string= (first parts) "bonjour"))
      (error "Unexpected handshake from p1: ~S" line))
    (let ((conn-id (second parts)))
      (write-ascii-line client-stream (format nil "ok ~A" conn-id))
      conn-id)))

(defun global-handshake (global-host global-port role conn-id peer-ip peer-port)
  "Contacte le serveur global. Lève une erreur si deny/timeout."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (s nil))
    (unwind-protect
         (progn
           (sb-ext:with-timeout 3 
             (sb-bsd-sockets:socket-connect
              sock
              (sb-bsd-sockets:host-ent-address
               (sb-bsd-sockets:get-host-by-name global-host))
              global-port))

           (setf s (sb-bsd-sockets:socket-make-stream sock
                                                      :input t :output t
                                                      :element-type '(unsigned-byte 8)
                                                      :timeout nil
                                                      :buffering :none))
           (write-ascii-line s
                             (format nil "hello ~A ~A ~A ~A"
                                     role conn-id peer-ip peer-port))

           (let ((reply (sb-ext:with-timeout 3 
                          (read-ascii-line s))))
             (cond
               ((string= reply "ok") t)
               (t (error "Global denied: ~S" reply)))))
      (ignore-errors (close s))
      (ignore-errors (close sock)))))
