(in-package :yano/proxy)

(defun now ()
  "Retourne un timestamp lisible."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun make-tap (real-transform-fn label)
  "Retourne une fonction (byte -> byte) qui applique REAL-TRANSFORM-FN
   et logge les 16 premiers octets après transformation."
  (let ((count 0))
    (lambda (b)
      (let ((out (funcall real-transform-fn b)))
        (when (< count 16)
          (format t "~A TAP ~2,'0X~%" label out)
          (incf count))
        out))))

(defun make-tap-before-after (real-transform-fn label)
  "Retourne une fonction (byte -> byte) qui:
   - applique REAL-TRANSFORM-FN
   - logge AVANT et APRES sur les 16 premiers octets"
  (let ((count 0))
    (lambda (b)
      (let ((before b)
            (after (funcall real-transform-fn b)))
        (when (< count 16)
          (format t "~A TAP ~2,'0X -> ~2,'0X~%" label before after)
          (incf count))
        after))))

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

(defun safe-close (x)
  (when x
    (ignore-errors (close x))))

(defun handle-client (client-socket client-addr client-port target-host target-port)
  ;; (multiple-value-bind (send-handshake expect-handshake)
  ;; (handshake-mode *role*)
  (declare (ignore target-host target-port))
  (let ((start-time (get-universal-time))
        (client-stream nil)
        (target-socket nil)
        (target-stream nil)
        ;; Transformation function (default to identity)
        (enc-fn #'identity)
        (dec-fn #'identity))
    (unwind-protect
         (progn
           (setf client-stream
                 (sb-bsd-sockets:socket-make-stream client-socket
                                                    :input t :output t
                                                    :element-type '(unsigned-byte 8)
                                                    :timeout nil
                                                    :buffering :none))


           ;;============================================

           (cond 
             ((eq *role* :alone)
              (multiple-value-bind (cd dst-port dst-host dst-ip4 userid)
                  (parse-socks4-request client-stream)
                (declare (ignore userid))
                (cond
                  ((= cd #x01) ;; CONNECT
                   (handler-case
                       (progn
                         (setf target-socket (make-instance 'sb-bsd-sockets:inet-socket
                                                            :type :stream
                                                            :protocol :tcp))
                         (sb-bsd-sockets:socket-connect target-socket
                                                        (sb-bsd-sockets:host-ent-address
                                                         (sb-bsd-sockets:get-host-by-name dst-host))
                                                        dst-port)

                         (setf target-stream (sb-bsd-sockets:socket-make-stream target-socket
                                                                                :input t :output t
                                                                                :element-type '(unsigned-byte 8)
                                                                                :timeout nil
                                                                                :buffering :none))

                         ;; reply GRANTED
                         (write-socks4-reply client-stream #x5A dst-port dst-ip4)
                         )
                     (error (e)
                       ;; reply REJECTED and stop
                       (declare (ignore e))
                       (write-socks4-reply client-stream #x5B dst-port dst-ip4)
                       (return-from handle-client nil))))
                  (t
                   ;; VBIND no managed
                   (write-socks4-reply client-stream #x5B dst-port dst-ip4)
                   (return-from handle-client nil)))))

             
             ((eq *role* :p1)
              (multiple-value-bind (cd dst-port dst-host dst-ip4 userid)
                  (parse-socks4-request client-stream)
                (declare (ignore userid))
                (unless (= cd #x01)
                  (write-socks4-reply client-stream #x5B dst-port dst-ip4)
                  (return-from handle-client nil))

                ;; 1) se connecter à P2 (target-host/port fournis au start-server)
                (handler-case
                    (progn
                      (setf target-socket (make-instance 'sb-bsd-sockets:inet-socket
                                                         :type :stream
                                                         :protocol :tcp))
                      (sb-bsd-sockets:socket-connect target-socket
                                                     (sb-bsd-sockets:host-ent-address
                                                      (sb-bsd-sockets:get-host-by-name dst-host))
                                                     dst-port)

                      (format t "[~A] P1 connected-to ~A:~A~%"
                              (now) dst-host dst-port)

                      (setf target-stream (sb-bsd-sockets:socket-make-stream target-socket
                                                                             :input t :output t
                                                                             :element-type '(unsigned-byte 8)
                                                                             :timeout nil
                                                                             :buffering :none))

                      ;; reply GRANTED
                      (write-socks4-reply client-stream #x5A dst-port dst-ip4)
                      )
                  (error (e)
                    ;; reply REJECTED and stop
                    (declare (ignore e))
                    (write-socks4-reply client-stream #x5B dst-port dst-ip4)
                    (return-from handle-client nil)))

                ;; 2) handshake p1->p2 incluant host/port demandé
                (let ((conn-id (make-conn-id)))
                  (let ((ok (p1-handshake-with-p2 target-stream conn-id dst-host dst-port)))
                    (unless ok
                      (write-socks4-reply client-stream #x5B dst-port dst-ip4)
                      (return-from handle-client nil)))

                  ;; 3) global-handshake côté p1 (met à jour enc/dec)
                  (multiple-value-bind (enc dec)
                      (global-handshake *global-host* *global-port* "p1"
                                        conn-id client-addr client-port)
                    (setf enc-fn enc
                          dec-fn dec))                 

                  ;; 4) seulement maintenant : OK au client SOCKS4
                  (write-socks4-reply client-stream #x5A dst-port dst-ip4)))) 


             
             ((eq *role* :p2)
              (multiple-value-bind (conn-id dst-host dst-port)
                  (p2-handshake-with-p1 client-stream)
                (multiple-value-bind (enc dec)
                    (global-handshake *global-host* *global-port* "p2"
                                      conn-id client-addr client-port)
                  (setf enc-fn enc
                        dec-fn dec))
                (handler-case
                    (progn
                      ;; 3) maintenant seulement : connexion backend
                      (setf target-socket (make-instance 'sb-bsd-sockets:inet-socket
                                                         :type :stream :protocol :tcp))
                      (sb-bsd-sockets:socket-connect target-socket
                                                     (sb-bsd-sockets:host-ent-address
                                                      (sb-bsd-sockets:get-host-by-name dst-host))
                                                     dst-port)
                      (setf target-stream (sb-bsd-sockets:socket-make-stream target-socket
                                                                             :input t :output t
                                                                             :element-type '(unsigned-byte 8)
                                                                             :timeout nil
                                                                             :buffering :none))
                      (write-ascii-line client-stream (format nil "ok ~A" conn-id))
                      ))
                (error ()
                       (write-ascii-line client-stream (format nil "fail ~A" conn-id))
                       (return-from handle-client nil))))
             )
           
           ;;============================================ 

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
                                             :transform-fn enc-fn ;; #'identity
                                             :on-finish #'shutdown-target-output))))
                      (t2 (sb-thread:make-thread
                           (lambda ()
                             (forward-stream target-stream client-stream
                                             :transform-fn dec-fn ;; #'identity
                                             :on-finish #'shutdown-client-output)))))
                  (sb-thread:join-thread t1)
                  (sb-thread:join-thread t2))))))
      
      ;; LOG déconnexion + cleanup
      (let ((duration (- (get-universal-time) start-time)))
        (format t "[~A] DISCONNECT ~A:~A (duration ~As)~%"
                (now) client-addr client-port duration))

      ;; fermer streams d'abord, puis sockets
      (safe-close client-stream)
      (safe-close target-stream)
      (safe-close client-socket)
      (safe-close target-socket))))
;; )
