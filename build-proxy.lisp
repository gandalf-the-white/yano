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
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "yano-proxy")

;; Create the directory if not
(defun ensure-build-dir ()
  (let ((build-dir (merge-pathnames "build/" (uiop:getcwd))))
    (unless (uiop:directory-exists-p build-dir)
      (ensure-directories-exist build-dir)
      (format t "'build' Directory created.~%"))))

;; ./yano-proxy-bin 8080 example.com 80 127.0.0.1
(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (and (>= (length args) 3) (<= (length args) 4))
      (format *error-output*
              "Usage: yano-proxy-bin <listen-port> <target-host> <target-port> [listen-host]~%")
      (sb-ext:exit :code 1))
    (let ((listen-port (parse-integer (first args)))
          (target-host (second args))
          (target-port (parse-integer (third args)))
          (listen-host (if (<= (length args) 3)
                           "0.0.0.0"
                           (fourth args))))
      (values listen-port target-host target-port listen-host))))

(defun main ()
  (multiple-value-bind (listen-port target-host target-port listen-host)
      (parse-args)
    (let ((*package* (find-package :yano/proxy)))
      (yano/proxy::start-server listen-port target-host target-port
                                :listen-host listen-host)
      (sleep most-positive-fixnum))))

(ensure-build-dir)

(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-proxy-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)
