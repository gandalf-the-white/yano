#!/usr/bin/env sbcl --script

;; the death of human empathy is one of the earliest and
;; most telling signs of a culture about to fall into barbarism 
;; Hannah Arendt

(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;; Load system and dependencies
;; (ql:quickload '(:hunchentoot :easy-routes :djula :drakma :babel :jonathan))
(push (merge-pathnames "asd/" (uiop:getcwd)) asdf:*central-registry*)
;; (push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "yano-proxy")

;; Create the directory if not
(defun ensure-build-dir ()
  (let ((build-dir (merge-pathnames "build/" (uiop:getcwd))))
    (unless (uiop:directory-exists-p build-dir)
      (ensure-directories-exist build-dir)
      (format t "'build' Directory created.~%"))))

(defun parse-port (s)
  (let ((n (parse-integer s :junk-allowed nil)))
    (unless (<= 1 n 65535)
      (error "Invalid port: ~a (must be 1..65535)" n))
    n))

(defun nil-string->nil (s)
  (if (and s (string-equal s "nil")) nil s))

(defun parse-maybe-port (s)
  (let ((s (nil-string->nil s)))
    (when s
      (parse-port s))))

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (and (>= (length args) 5) (<= (length args) 8))
      (format *error-output*
              "Usage: yano-proxy-bin <listen-port> <target-host> <target-port> <global-host> <global-port> [listen-host] [role] [mode]~%  mode: socks4|transparent (default socks4)~%")
      (sb-ext:exit :code 1))
    (let* ((listen-port (parse-integer (first args)))
           (target-host (nil-string->nil (second args)))
           (target-port (parse-maybe-port (third args)))
           ;; (target-host (second args))
           ;; (target-port (parse-integer (third args)))
           (global-host (fourth args))
           (global-port (parse-integer (fifth args)))
           (listen-host (if (>= (length args) 6)
                            "0.0.0.0"
                            (sixth args)))
           (role-str (if (= (length args) 7)
                         (string-downcase (seventh args))
                         "alone"))
           (role (cond ((string= role-str "alone") :alone)
                       ((string= role-str "p1") :p1)
                       ((string= role-str "p2") :p2)
                       (t
                        (format *error-output*
                                "Invalid role: ~A (expected: alone, p1, p2)~%"
                                role-str)
                        (sb-ext:exit :code 1))))
           (mode-str (if (>= (length args) 8)
                         (string-downcase (eighth args))
                         "socks4"))
           (mode (cond ((string= mode-str "socks4") :socks4)
                       ((string= mode-str "transparent") :transparent)
                       (t
                        (format *error-output* "Invalid mode: ~A (expected: socks4, transparent)~%"
                                mode-str)
                        (sb-ext:exit :code 1)))))
      (values listen-port target-host target-port global-host global-port listen-host role mode))))

(defun main ()
  (multiple-value-bind (listen-port target-host target-port global-host global-port listen-host role mode)
      (parse-args)
    (let ((*package* (find-package :yano/proxy)))
      (setf yano/proxy::*frontend-mode* mode)
      (yano/proxy::start-server listen-port target-host target-port global-host global-port
                                :listen-host listen-host
                                :role role)
      (sleep most-positive-fixnum))))

(ensure-build-dir)

(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-proxy-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)

;; example
;; To access the storage "192.188.200.55" 
;; yano-proxy-bin 45000 "192.188.200.55" 80 "0.0.0.0" "alone"
