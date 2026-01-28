(in-package :yano/frontend)

(defclass video ()
  ((id
    :initarg :id
    :reader video-id)
   (title
    :initarg :title
    :reader video-title)
   (duration
    :initarg :duration
    :reader video-duration)
   (hls-url
    :initarg :hls-url
    :reader video-hls-url)))
