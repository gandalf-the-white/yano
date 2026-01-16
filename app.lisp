#!/usr/bin/env sbcl --script

;;; 1. Load quicklisp
(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;;; 2. Load dependencies
(ql:quickload '(:hunchentoot :easy-routes :djula))
(push (uiop:getcwd) asdf:*central-registry*)

;;; 3. Load the system
(ql:quickload "yano")

;;; 4. Start the server
(let ((port (or (and (uiop:command-line-arguments)
                     (parse-integer (first (uiop:command-line-arguments)) :junk-allowed t))
                8899)))
  (handler-case
      (progn
        (yano:start-server :port port)
        ;; Wait signal to stop (Ctrl+C)
        (loop (sleep 1)))
    (error (e)
      (format *error-output* "~&Error: ~a~&" e)
      (sb-ext:exit :code 1))))
