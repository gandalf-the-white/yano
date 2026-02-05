(in-package :yano/proxy)

(defun now ()
  "Retourne un timestamp lisible."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

;; (defun forward-stream (in out)
;;   "Copie les octets de IN vers OUT jusqu'à EOF."
;;   (handler-case
;;       (loop
;;         (let ((byte (read-byte in nil nil)))
;;           (unless byte
;;             (finish-output out)
;;             (return))
;;           (write-byte byte out)
;;           (finish-output out)))
;;     (error () nil)))

;; ex: (forward-stream *standard-input* *standard-output*
;; :transform-fn (lambda (byte) (logxor byte #xff)))

(defun forward-stream (in out &key (transform-fn #'identity))
  "Copie les octets de IN vers OUT jusqu'à EOF."
  (handler-case
      (loop
        (let ((byte (read-byte in nil nil)))
          (unless byte
            (finish-output out)
            (return))
          (let ((transformed-byte (funcall transform-fn byte)))
            (when transformed-byte
              (write-byte transformed-byte out)
              (finish-output out)))))
    (error () nil)))

;; (defun send-secret target-stream &key (message #(0 1 2 3 4 5))
;;   (write-sequence message target-stream)
;;   (force-output)
;;   (format t "Send Message (16 bytes) : ~a~%"
;;           (map 'list #'identity message)))

(defun random-bytes (n)
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (loop for i below n
          do (setf (aref buf i) (random 256)))
    buf))

(defun secret-handshake (in out &key (role :alone))
  (let ((local (random-bytes *handshake-size*))
        (remote (make-array *handshake-size*
                            :element-type '(unsigned-byte 8)))
        (remoteside (make-array *handshake-size*
                                :element-type '(unsigned-byte 8))))
    (cond
      ((eq role :alone)
       (format t "Alone")
       (force-output)
       )
      ((eq role :client)
       (format t "Client: -> ")
       (force-output)
       (write-sequence local out)
       (read-sequence remote out)
       (finish-output out)
       (loop for byte across remote
             do (format t "~2,'0x " byte))
       (format t "~%")
       (force-output))
      ;; ((eq role :middle)
      ;;  (format t "Middle: -> ")
      ;;  (force-output)
      ;;  (read-sequence remote in)
      ;;  (write-sequence local in)
      ;;  (write-sequence local out)
      ;;  (read-sequence remoteside out)
      ;;  (loop for byte across remote
      ;;        do (format t "~2,'0x " byte))
      ;;  (loop for byte across remoteside
      ;;        do (format t "~2,'0x " byte))
      ;;  (format t "~%")
      ;;  (force-output)
      ;;  (finish-output out))
      ((eq role :server)
       (format t "Server: -> ")
       (force-output)
       (read-sequence remote in)
       (write-sequence local in)
       (finish-output in)
       (loop for byte across remote
             do (format t "~2,'0x " byte))
       (format t "~%")
       (force-output))
      (t
       (format t "Alone")
       (force-output)))))

(defun singularity  (target-host target-port)
  (let* ((target-socket (make-instance 'inet-socket
                                       :type :stream
                                       :protocol :tcp)))
    (socket-connect target-socket
                    (host-en-address
                     (get-host-by-name target-host))
                    target-port)
    (let ((target-stream (socket-make-stream target-socket
                                             :input t :output t
                                             :element-type '(unsigned-byte 8)
                                             :timeout nil
                                             :buffering :none)))
      ))
  )

;; (start-server 45000 "127.0.0.1" 45001 :role :client)
;; (start-server 45001 "192.188.200.55" 80 :role :server)


(defun handle-client (client-socket client-addr client-port
                      target-host target-port)
  (let ((start-time (get-universal-time)))
    (unwind-protect
         (let* ((client-stream (socket-make-stream client-socket
                                                   :input t :output t
                                                   :element-type '(unsigned-byte 8)
                                                   :timeout nil
                                                   :buffering :none))
                ;; Socket to target 
                (target-socket (make-instance 'inet-socket
                                              :type :stream
                                              :protocol :tcp)))
           ;; send connect to target
           (socket-connect target-socket
                           (host-ent-address
                            (get-host-by-name target-host))
                           target-port)

           (let ((target-stream (socket-make-stream target-socket
                                                    :input t :output t
                                                    :element-type '(unsigned-byte 8)
                                                    :timeout nil
                                                    :buffering :none)))
             
             ;; (secret-handshake client-stream target-stream :role *role*)
             
             (let ((t1 (make-thread
                        (lambda ()
                          (forward-stream client-stream target-stream))))
                   (t2 (make-thread
                        (lambda ()
                          (forward-stream target-stream client-stream)))))
               (join-thread t1)
               (join-thread t2))))
      ;; LOG déconnexion
      (let ((duration (- (get-universal-time) start-time)))
        (format t "[~A] DISCONNECT ~A:~A (duration ~As)~%"
                (now)
                client-addr client-port
                duration))

      (ignore-errors (close client-socket)))))

