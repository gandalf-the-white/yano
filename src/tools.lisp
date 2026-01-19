
(in-package :yano)

;;===========================================
;; R E N D E R
;;===========================================

(defun render (template &rest args)
  (apply
   #'render-template*
   (asdf:system-relative-pathname "yano" template)
   nil
   args))

;;===========================================
;; T O O L
;;===========================================

(defun get-videos (&optional (n 5))
  (loop for i from 0 below n
        collect (list i (format nil "Video nb ~a" i)
                      "title")))

(defun get-video (n)
  (list n (format nil "Video nb ~a" n) "title"))
