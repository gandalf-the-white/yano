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

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (unless (and (>= (length args) 3) (<= (length args) 5))
      (format *error-output*
              "Usage: yano-proxy-bin <listen-port> <target-host> <target-port> [listen-host] [role]~%")
      (sb-ext:exit :code 1))
    (let* ((listen-port (parse-integer (first args)))
           (target-host (second args))
           (target-port (parse-integer (third args)))
           (listen-host (if (<= (length args) 3)
                            "0.0.0.0"
                            (fourth args)))
           (role-str (if (= (length args) 5)
                         (string-downcase (fifth args))
                         "alone"))
           (role (cond ((string= role-str "alone") :alone)
                       ((string= role-str "p1") :p1)
                       ((string= role-str "p2") :p2)
                       (t
                        (format *error-output*
                                "Invalid role: ~A (expected: alone, p1, p2)\n"
                                role-str)))))
      (values listen-port target-host target-port listen-host role))))

(defun main ()
  (multiple-value-bind (listen-port target-host target-port listen-host role)
      (parse-args)
    (let ((*package* (find-package :yano/proxy)))
      (yano/proxy::start-server listen-port target-host target-port
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
