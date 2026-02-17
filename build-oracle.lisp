#!/usr/bin/env sbcl --script

(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;; Load system and dependencies
;; (ql:quickload '(:hunchentoot :easy-routes :djula :drakma :babel :jonathan))
(push (merge-pathnames "asd/" (uiop:getcwd)) asdf:*central-registry*)
;; (push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "yano-oracle")

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

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (= (length args) 1)
      (format *error-output*
              "Usage: yano-oracle-bin [listen-port]~%")
      (sb-ext:exit :code 1))
    (let ((listen-port (first args)))
      (values (parse-port listen-port)))))

(defun main ()
  (multiple-value-bind (listen-port)
      (parse-args)
    (let ((*package* (find-package :yano/oracle)))
      (yano/oracle::start-server :port listen-port)
      (sleep most-positive-fixnum))))

(ensure-build-dir)

;; Save binary
(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-oracle-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)

;; example
;; To access the storage "192.188.200.55" 
;; yano-backend-bin 9000 "http://192.188.200.55"
