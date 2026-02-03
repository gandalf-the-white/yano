#!/usr/bin/env sbcl --script

(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;; Load system and dependencies
;; (ql:quickload '(:hunchentoot :easy-routes :djula :drakma :babel :jonathan))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "yano-backend")

;; Create the directory if not
(defun ensure-build-dir ()
  (let ((build-dir (merge-pathnames "build/" (uiop:getcwd))))
    (unless (uiop:directory-exists-p build-dir)
      (ensure-directories-exist build-dir)
      (format t "'build' Directory created.~%"))))

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (and (>= (length args) 0) (<= (length args) 1))
      (format *error-output*
              "Usage: yano-backend-bin [port]~%")
      (sb-ext:exit :code 1))
    (let ((port (if (<= (length args) 0)
                    9000
                    (first args))))
      (values port))))

(defun main ()
  (multiple-value-bind (port)
      (parse-args)
    (let ((*package* (find-package :yano/backend)))
      (yano/backend::start-server :port port)
      (sleep most-positive-fixnum))))

(ensure-build-dir)

;; Save binary
(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-backend-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)
