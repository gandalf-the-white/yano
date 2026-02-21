(in-package :yano/oracle)

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
                 (write-ascii-line s "ok idt")
                 ;; (write-ascii-line s "ok not")
                 ;; (write-ascii-line s "ok xor 42")
                 ;; (write-ascii-line s "deny")
                 )))
      (ignore-errors (close s))
      (ignore-errors (close client-socket)))))

