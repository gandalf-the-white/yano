#!/usr/bin/env sbcl --script

(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;; Load system and dependencies
;; (ql:quickload '(:hunchentoot :easy-routes :djula :drakma :babel :jonathan))
;; (push (uiop:getcwd) asdf:*central-registry*)
(push (merge-pathnames "asd/" (uiop:getcwd)) asdf:*central-registry*)
(ql:quickload "yano-frontend")

;; Create the directory if not
(defun ensure-build-dir ()
  (let ((build-dir (merge-pathnames "build/" (uiop:getcwd))))
    (unless (uiop:directory-exists-p build-dir)
      (ensure-directories-exist build-dir)
      (format t "'build' Directory created.~%"))))

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (= (length args) 2)
      (format *error-output*
              "Usage: yano-frontend-bin [port] [backend-address]~%")
      (sb-ext:exit :code 1))
    (let ((port (parse-integer (first args)))
          (backend-address (second args)))
      (values port backend-address))))

(defun main ()
  (multiple-value-bind (port backend-address)
      (parse-args)
    (let ((*package* (find-package :yano/frontend)))
      (yano/frontend::start-server
       :port port
       :backend-address backend-address)
      (sleep most-positive-fixnum))))


(ensure-build-dir)

;; Save binary
(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-frontend-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)

;; example
;; To access the backend "192.188.200.57" 
;; yano-frontend-bin 8000 "http://192.188.200.57:9000"
