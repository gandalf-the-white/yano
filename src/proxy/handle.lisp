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

;; (defun random-bytes (n)
;;   (let ((buf (make-array n :element-type '(unsigned-byte 8))))
;;     (loop for i below n
;;           do (setf (aref buf i) (random 256)))
;;     buf))


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

             (cond
               ((eq *role* :p2)
                ;; 1) handshake p1->p2 (consomme "bonjour ...")
                (let ((conn-id (p2-handshake-with-p1 client-stream)))
                  ;; 2) handshake avec serveur global AVANT de toucher au backend
                  (multiple-value-bind (enc-fn dec-fn)
                      (global-handshake *global-host* *global-port* "p2"
                                        conn-id client-addr client-port))
                  ;; 3) maintenant seulement : connexion backend
                  (setf target-socket (make-instance 'sb-bsd-sockets:inet-socket
                                                     :type :stream :protocol :tcp))
                  (sb-bsd-sockets:socket-connect target-socket
                                                 (sb-bsd-sockets:host-ent-address
                                                  (sb-bsd-sockets:get-host-by-name target-host))
                                                 target-port)
                  (setf target-stream (sb-bsd-sockets:socket-make-stream target-socket
                                                                         :input t :output t
                                                                         :element-type '(unsigned-byte 8)
                                                                         :timeout nil
                                                                         :buffering :none))))
               (t
                ;; connect au target (p2 ou backend)
                (setf target-socket (make-instance 'sb-bsd-sockets:inet-socket
                                                   :type :stream :protocol :tcp))
                (sb-bsd-sockets:socket-connect target-socket
                                               (sb-bsd-sockets:host-ent-address
                                                (sb-bsd-sockets:get-host-by-name target-host))
                                               target-port)
                (setf target-stream (sb-bsd-sockets:socket-make-stream target-socket
                                                                       :input t :output t
                                                                       :element-type '(unsigned-byte 8)
                                                                       :timeout nil
                                                                       :buffering :none))

                (when (eq *role* :p1)
                  ;; 1) handshake p1->p2
                  (let ((conn-id (make-conn-id)))
                    (p1-handshake-with-p2 target-stream conn-id)
                    ;; 2) handshake global avant forward
                    (multiple-value-bind (enc-fn dec-fn)
                        (global-handshake *global-host* *global-port* "p1"
                                          conn-id client-addr client-port))))))
             
             (labels
                 ((shutdown-target-output ()
                    (when target-socket
                      (ignore-errors
                       (sb-bsd-sockets:socket-shutdown target-socket :direction :output))))
                  (shutdown-client-output ()
                    (when client-socket
                      (ignore-errors
                       (sb-bsd-sockets:socket-shutdown client-socket :direction :output)))))

               (cond
                 ;; P1: enc vers p2, dec vers client
                 ((eq *role* :p1)
                  (let ((t1 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream client-stream target-stream
                                               :transform-fn enc-fn
                                               :on-finish #'shutdown-target-output))))
                        (t2 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream target-stream client-stream
                                               :transform-fn dec-fn
                                               :on-finish #'shutdown-client-output)))))
                    (sb-thread:join-thread t1)
                    (sb-thread:join-thread t2)))

                 ;; P2: dec vers backend, enc vers p1
                 ((eq *role* :p2)
                  (let ((t1 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream client-stream target-stream
                                               :transform-fn dec-fn
                                               :on-finish #'shutdown-target-output))))
                        (t2 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream target-stream client-stream
                                               :transform-fn enc-fn
                                               :on-finish #'shutdown-client-output)))))
                    (sb-thread:join-thread t1)
                    (sb-thread:join-thread t2)))

                 ;; ALONE: identity dans les deux sens
                 (t
                  (let ((t1 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream client-stream target-stream
                                               :transform-fn #'identity
                                               :on-finish #'shutdown-target-output))))
                        (t2 (sb-thread:make-thread
                             (lambda ()
                               (forward-stream target-stream client-stream
                                               :transform-fn #'identity
                                               :on-finish #'shutdown-client-output)))))
                    (sb-thread:join-thread t1)
                    (sb-thread:join-thread t2))))))
        
        ;; LOG déconnexion + cleanup
        (let ((duration (- (get-universal-time) start-time)))
          (format t "[~A] DISCONNECT ~A:~A (duration ~As)~%"
                  (now) client-addr client-port duration))

        ;; fermer streams d'abord, puis sockets
        (%safe-close client-stream)
        (%safe-close target-stream)
        (%safe-close client-socket)
        (%safe-close target-socket)))))
