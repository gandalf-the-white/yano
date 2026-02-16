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

(defun parse-port (s)
  (let ((n (parse-integer s :junk-allowed nil)))
    (unless (<= 1 n 65535)
      (error "Invalid port: ~a (must be 1..65535)" n))
    n))

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (= (length args) 2)
      (format *error-output*
              "Usage: yano-backend-bin [listen-port] [storage-IP]~%")
      (sb-ext:exit :code 1))
    (let ((listen-port (first args))
          (storage (second args)))
      (values (parse-port listen-port) storage))))

(defun main ()
  (multiple-value-bind (listen-port storage)
      (parse-args)
    (let ((*package* (find-package :yano/backend)))
      (yano/backend::start-server :port listen-port
                                  :storage storage)
      (sleep most-positive-fixnum))))

(ensure-build-dir)

;; Save binary
(sb-ext:save-lisp-and-die
 (merge-pathnames "build/yano-backend-bin" (uiop:getcwd))
 :toplevel #'main
 :executable t
 :compression nil)

;; example
;; To access the storage "192.188.200.55" 
;; yano-backend-bin 9000 "http://192.188.200.55"
