(in-package :yano/proxy)

(defun forward-stream (in out)
  "Copie les octets de IN vers OUT jusqu'à EOF."
  (handler-case
      (loop
        (let ((byte (read-byte in nil nil)))
          (unless byte
            (finish-output out)
            (return))
          (write-byte byte out)
          (finish-output out)))
    (error () nil)))

(defun handle-client (client-socket client-addr client-port
                      target-host target-port)
  (let ((start-time (get-universal-time)))
    (unwind-protect
         (let* ((client-stream (socket-make-stream client-socket
                                                   :input t :output t
                                                   :element-type '(unsigned-byte 8)
                                                   :timeout nil
                                                   :buffering :none))
                (target-socket (make-instance 'inet-socket
                                              :type :stream
                                              :protocol :tcp)))
           (socket-connect target-socket
                           (host-ent-address
                            (get-host-by-name target-host))
                           target-port)

           (let ((target-stream (socket-make-stream target-socket
                                                    :input t :output t
                                                    :element-type '(unsigned-byte 8)
                                                    :timeout nil
                                                    :buffering :none)))
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

(defun now ()
  "Retourne un timestamp lisible."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))
