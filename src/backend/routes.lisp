(in-package :yano/backend)

(defun video->json (video)
  `(:id ,(video-id video)
    :title ,(video-title video)
    :duration ,(video-duration video)
    :hls-url ,(concatenate 'string
                           *video-storage-base-url*
                           (video-hls-path video))))

(defun find-video (id)
  (find id *videos*
        :key #'video-id
        :test #'string=))

(defroute list-videos ("/api/videos" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (to-json
   (mapcar #'video->json *videos*)))

(defroute get-video ("/api/videos/:id" :method :get) (&path (id 'string))
  (let ((video (find-video id)))
    (if video
        (progn
          (setf (hunchentoot:content-type*) "application/json")
          (jonathan:to-json (video->json video)))
        (progn
          (setf (hunchentoot:return-code*) 404)
          (setf (hunchentoot:content-type*) "application/json")
          "{\"error\":\"video not found\"}"))))

