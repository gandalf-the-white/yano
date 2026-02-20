(in-package :yano/proxy)

(defun read-u8 (s) (read-byte s))
(defun read-u16be (s)
  (let ((hi (read-u8 s))
        (lo (read-u8 s)))
    (+ (ash hi 8) lo)))

(defun read-cstring (s &key (max 512))
  (let ((chars '())
        (n 0))
    (loop
      (when (>= n max) (error "CString too long"))
      (let ((b (read-byte s nil :eof)))
        (when (eq b :eof) (error "EOF in cstring"))
        (when (= b 0) (return (coerce (nreverse chars) 'string)))
        (push (code-char b) chars)
        (incf n)))))

(defun ip4->string (ip4)
  (format nil "~D.~D.~D.~D" (aref ip4 0) (aref ip4 1) (aref ip4 2) (aref ip4 3)))

(defun write-socks4-reply (s status &optional (port 0) (ip4 #(0 0 0 0)))
  ;; 8 bytes: VN=0, CD=status, PORT, IP
  (write-byte 0 s)
  (write-byte status s)
  (write-byte (ldb (byte 8 8) port) s)
  (write-byte (ldb (byte 8 0) port) s)
  (dotimes (i 4) (write-byte (aref ip4 i) s))
  (finish-output s))

(defun parse-socks4-request (s)
  "Retourne: cd port host-string ip4-vector userid"
  (let* ((vn (read-u8 s))
         (cd (read-u8 s))
         (port (read-u16be s))
         (ip4 (vector (read-u8 s) (read-u8 s) (read-u8 s) (read-u8 s)))
         (userid (read-cstring s)))
    (unless (= vn #x04)
      (error "Not SOCKS4 (VN=~X)" vn))
    ;; SOCKS4a: IP = 0.0.0.x (x != 0) => domain after userid
    (let* ((is-4a (and (= (aref ip4 0) 0)
                       (= (aref ip4 1) 0)
                       (= (aref ip4 2) 0)
                       (/= (aref ip4 3) 0)))
           (host (if is-4a
                     (read-cstring s)
                     (ip4->string ip4))))
      (values cd port host ip4 userid))))
