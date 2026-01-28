(in-package :yano/backend)

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
   (hls-path
    :initarg :hls-path
    :reader video-hls-path)))
