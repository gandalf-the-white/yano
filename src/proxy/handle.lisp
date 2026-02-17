(in-package :yano/proxy)

(defun now ()
  "Retourne un timestamp lisible."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun forward-stream (in out &key (transform-fn #'identity) on-finish)
  (unwind-protect
       (handler-case
           (loop
             (let ((byte (read-byte in nil :eof)))
               (when (eq byte :eof) (return))
               (let ((b (funcall transform-fn byte)))
                 (when b
                   (write-byte b out)))))
         (end-of-file () nil)
         (stream-error () nil)
         (sb-bsd-sockets:socket-error () nil)
         (error () nil))
    (ignore-errors (finish-output out))
    (when on-finish (ignore-errors (funcall on-finish)))))


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


;; (start-server 45000 "127.0.0.1" 45001 :role :client)
;; (start-server 45001 "192.188.200.55" 80 :role :server)


(defun %safe-close (x)
  (when x
    (ignore-errors (close x))))

(defun handle-client (client-socket client-addr client-port target-host target-port)
  (multiple-value-bind (send-handshake expect-handshake)
      (handshake-mode *role*)
    (let ((start-time (get-universal-time))
          (client-stream nil)
          (target-socket nil)
          (target-stream nil)
          (t1 nil)
          (t2 nil))
      (unwind-protect
           (progn
             (setf client-stream
                   (sb-bsd-sockets:socket-make-stream client-socket
                                                      :input t :output t
                                                      :element-type '(unsigned-byte 8)
                                                      :timeout nil
                                                      :buffering :none))

             ;; p2 handshake before forward anything
             (when expect-handshake
               (do-server-handshake client-stream))
             
             (setf target-socket
                   (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream
                                  :protocol :tcp))

             (sb-bsd-sockets:socket-connect target-socket
                                            (sb-bsd-sockets:host-ent-address
                                             (sb-bsd-sockets:get-host-by-name target-host))
                                            target-port)

             (setf target-stream
                   (sb-bsd-sockets:socket-make-stream target-socket
                                                      :input t :output t
                                                      :element-type '(unsigned-byte 8)
                                                      :timeout nil
                                                      :buffering :none))

             ;; p1 send handshake to p2 proxy before
             (when send-handshake
               (do-client-handshake target-stream))
             
             ;; Ensuite seulement: forwarding
             (let ((t1 (make-thread (lambda () (forward-stream client-stream target-stream))))
                   (t2 (make-thread (lambda () (forward-stream target-stream client-stream)))))
               (join-thread t1)
               (join-thread t2)))


        
        ;; LOG déconnexion + cleanup
        (let ((duration (- (get-universal-time) start-time)))
          (format t "[~A] DISCONNECT ~A:~A (duration ~As)~%"
                  (now) client-addr client-port duration))

        ;; fermer streams d'abord, puis sockets
        (%safe-close client-stream)
        (%safe-close target-stream)
        (%safe-close client-socket)
        (%safe-close target-socket)))))
