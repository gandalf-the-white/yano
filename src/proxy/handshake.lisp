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

;; run by p1
(defun p1-handshake-with-p2 (target-stream conn-id host port)
  (write-ascii-line target-stream (format nil "bonjour ~A ~A ~D" conn-id host port))
  (let ((line (sb-ext:with-timeout 3  ; 3s
                (read-ascii-line target-stream))))
    (cond
      ((string= line (format nil "ok ~A" conn-id)) t)
      ((string= line (format nil "fail ~A" conn-id)) nil)
      (t (error "Bad handshake from p2: ~S" line)))))

;; run by p2
(defun p2-handshake-with-p1 (client-stream)
  (let* ((line (sb-ext:with-timeout 3 
                 (read-ascii-line client-stream))))
    ;; attend "bonjour <conn-id>"
    (let ((parts (uiop:split-string line :separator " ")))
      (unless (and (= (length parts) 4)
                   (string= (first parts) "bonjour"))
        (error "Unexpected handshake from p1: ~S" line))
      (values (second parts)                 ;; conn-id
              (third parts)                  ;; host
              (parse-integer (fourth parts)) ;; port
              ))))

(defun parse-global-reply-fctn (line)
  "LINE: \"ok not\" ou \"ok xor 42\".
Retourne 2 valeurs: encrypt-fn decrypt-fn."
  (let* ((parts (remove "" (uiop:split-string line :separator " ") :test #'string=)))
    (unless (and (>= (length parts) 2) (string= (first parts) "ok"))
      (error "Global denied: ~S" line))
    (let ((algo (second parts)))
      (cond
        ((string= algo "idt")
         ;; (format t "Apply encryption function: ~S~%" algo)
         (values #'identity #'identity))
        ((string= algo "not")
         ;; (format t "Apply encryption function: ~S~%" algo)
         (values #'byte-not #'byte-not))
        ((string= algo "xor")
         (unless (= (length parts) 3)
           (error "Bad global reply (xor needs key): ~S" line))
         (let* ((k (parse-integer (third parts) :junk-allowed nil))
                (fn (make-byte-xor k)))
           ;; (format t "Apply encryption function: ~S~%" algo)
           (values fn fn)))
        (t
         (error "Unknown algo from global: ~S" algo))))))

(defun global-handshake (global-host global-port role conn-id peer-ip peer-port)
  "Contacte le serveur global. Lève une erreur si deny/timeout."
  (let ((global-socket (make-instance 'sb-bsd-sockets:inet-socket
                                      :type :stream
                                      :protocol :tcp))
        (global-stream nil))
    (unwind-protect
         (progn
           (sb-ext:with-timeout 3 
             (sb-bsd-sockets:socket-connect
              global-socket
              (sb-bsd-sockets:host-ent-address
               (sb-bsd-sockets:get-host-by-name global-host))
              global-port))

           (setf global-stream (sb-bsd-sockets:socket-make-stream global-socket
                                                                  :input t :output t
                                                                  :element-type '(unsigned-byte 8)
                                                                  :timeout nil
                                                                  :buffering :none))
           (write-ascii-line global-stream
                             (format nil "hello ~A ~A ~A ~A"
                                     role conn-id peer-ip peer-port))

           (let ((reply (sb-ext:with-timeout 3 
                          (read-ascii-line global-stream))))
             ;; (cond
             ;; ((string= reply "ok") t)
             ;; (t (error "Global denied: ~S" reply)))

             (multiple-value-bind (enc dec)
                 (parse-global-reply-fctn reply)
               (values enc dec))))
      (ignore-errors (close global-stream))
      (ignore-errors (close global-socket)))))
