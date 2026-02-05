(in-package :yano/backend)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 9000
  "Application port.")

(defvar *video-storage-base-url* "http://192.188.200.55")

(defparameter *videos*
  ;; List of movies
  (list (make-instance 'video
                       :id "movie1"
                       :title "Tears of Steel"
                       :duration 3600
                       :hls-path "/hls/tears/playlist.m3u8")

        (make-instance 'video
                       :id "movie2"
                       :title "Demoreel"
                       :duration 1800
                       :hls-path "/hls/real/playlist.m3u8")))

(defun start-server (&key (port *port*) (storage *video-storage-base-url*))
  ;; (defun start-server (&key (port *port*))
  "Start the server"
  (format t "~&Starting the web server on port ~a~&" port)
  (setf *server* (make-instance 'easy-routes-acceptor
                                :document-root (merge-pathnames #p"static/" (uiop:getcwd))
                                :port port
                                :address "0.0.0.0")
        *video-storage-base-url* storage)
  
  (tbnl:start *server*))


(defun stop-server ()
  "Stop the server"
  (when *server*
    (progn
      (tbnl:stop *server*)
      (format t "~&Stopping the web server~&")
      (setf *server* nil))))
