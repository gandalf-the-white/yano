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

;; (defconstant +sol-ip+ 0)
;; (defconstant +so-original-dst+ 80) ; Linux SO_ORIGINAL_DST

;; (cffi:defcstruct sockaddr-in
;;   (sin-family :unsigned-short)
;;   (sin-port   :unsigned-short)
;;   (sin-addr   :unsigned-int)
;;   (sin-zero   (:array :unsigned-char 8)))

;; (cffi:defcfun ("getsockopt" c-getsockopt) :int
;;   (fd :int) (level :int) (optname :int)
;;   (optval :pointer) (optlen :pointer))

;; (defun ntohs (x)
;;   ;; x is 16-bit in network order
;;   (logior (ash (logand x #xFF) 8)
;;           (ash (logand x #xFF00) -8)))

;; (defun ip-from-u32 (u32)
;;   ;; u32 is network-order for sin_addr (on little endian you often need ntohl,
;;   ;; but for printing we can extract bytes consistently)
;;   (let ((b1 (ldb (byte 8 24) u32))
;;         (b2 (ldb (byte 8 16) u32))
;;         (b3 (ldb (byte 8 8)  u32))
;;         (b4 (ldb (byte 8 0)  u32)))
;;     (format nil "~D.~D.~D.~D" b1 b2 b3 b4)))

;; (defun original-dst (socket)
;;   "Retourne (values host port) de la destination originale (iptables REDIRECT)."
;;   (let* ((fd (sb-bsd-sockets:socket-file-descriptor socket))
;;          (len (cffi:foreign-alloc :unsigned-int :initial-element (cffi:foreign-type-size '(:struct sockaddr-in)))))
;;     (unwind-protect
;;          (cffi:with-foreign-object (addr '(:struct sockaddr-in))
;;            (let ((rc (c-getsockopt fd +sol-ip+ +so-original-dst+
;;                                    addr len)))
;;              (when (/= rc 0)
;;                (error "getsockopt(SO_ORIGINAL_DST) failed"))
;;              (let ((port (ntohs (cffi:foreign-slot-value addr '(:struct sockaddr-in) 'sin-port)))
;;                    (ip   (cffi:foreign-slot-value addr '(:struct sockaddr-in) 'sin-addr)))
;;                (values (ip-from-u32 ip) port))))
;;       (cffi:foreign-free len))))
