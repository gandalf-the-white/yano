#!/usr/bin/env sbcl --script

(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;; Charger les dépendances et le système
(ql:quickload '(:hunchentoot :easy-routes :djula))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "yano")

;; Fonction principale de l'application
(defun main ()
  (let ((*package* (find-package :yano)))
    (yano::start-server)
    (sleep most-positive-fixnum)))

;; Sauvegarder le binaire
(sb-ext:save-lisp-and-die
 "yano-bin"
 :toplevel #'main
 :executable t
 :compression t)
