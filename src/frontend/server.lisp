(in-package :yano/frontend)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8000
  "Application port.")

;; backend
(defparameter *video-api-base*
  "http://127.0.0.1:9000")

;; to list all videos
(defparameter *video-api-list*
  "/api/videos")

(defparameter +index.html+ nil)

(defparameter +player.html+ nil)

(defparameter *proxy-address* "127.0.0.1")

(defparameter *proxy-port* 45000)

(defun init-djula ()
  (djula:add-template-directory (asdf:system-relative-pathname :yano "templates/"))
  (setf +index.html+ (djula:compile-template* "index.html"))
  (setf +player.html+ (djula:compile-template* "player.html")))


(defun start-server (&key (port *port*)(video-api *video-api-base*))
  "Start the server"
  (format t "~&Starting the web server on port ~a~&" port)
  (init-djula)
  (setf *server* (make-instance 'easy-routes-acceptor
                                :document-root (merge-pathnames #p"static/" (uiop:getcwd))
                                :port port
                                :address "0.0.0.0")
        *video-api-base* video-api)
  (tbnl:start *server*))


(defun stop-server ()
  "Stop the server"
  (when *server*
    (progn
      (tbnl:stop *server*)
      (format t "~&Stopping the web server~&")
      (setf *server* nil))))
