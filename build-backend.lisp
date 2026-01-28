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

;; Backend Main function
(defun main ()
  (let ((*package* (find-package :yano/backend)))
    (yano/backend::start-server)
    (sleep most-positive-fixnum)))

(ensure-build-dir)

;; Save binary
(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-backend-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)
