(in-package :yano/frontend)

(defun fetch-video-list ()
  (multiple-value-bind (body status header)
      (drakma:http-request
       (concatenate 'string
                    *video-api-base*
                    *video-api-list*)
       :method :get
       :accept "application/json"
       :force-binary nil)
    (declare (ignore status))
    ;; (format t "~&[frontend] status=~A~%" status)
    (cond
      ((= status 200) 
       (let ((string (babel:octets-to-string body :encoding :utf-8)))
         (jonathan:parse string)))
      ((= status 404)
       nil)
      (t
       (error "Backend returned HTTP ~A")))))

(defun fetch-video (id)
  (multiple-value-bind (body status header)
      (drakma:http-request
       (concatenate 'string
                    *video-api-base*
                    *video-api-list*
                    "/" id)
       :method :get
       :accept "application/json"
       :force-binary nil)
    (declare (ignore header))
    (cond
      ((= status 200)
       (let ((string (babel:octets-to-string body :encoding :utf-8)))
         (jonathan:parse string)))
      ((= status 404)
       nil)
      (t
       (error "Backend returned HTTP ~A while fetching video ~A" status id)))))
;; (format t "~&[frontend] status=~A~%" status)
;; (let ((string (babel:octets-to-string body :encoding :utf-8)))
;; (jonathan:parse string))))

(defun make-video (json)
  ;; json based on plist :id :title
  (make-instance 'video
                 :id (getf json :id)
                 :duration (getf json :duration)
                 :title (getf json :title)
                 :hls-url (getf json :hls-url)))

;; =============================================
;; A D D R E S S
;; =============================================

(defun extract-server-ip (url)
  (let* ((start (search "://" url))
         (rest (subseq url (+ start 3)))
         (colon-pos (position #\: rest))
         (slash-pos (position #\/ rest)))
    (cond
      ((and colon-pos slash-pos)
       (if (< colon-pos slash-pos)
           (subseq rest 0 colon-pos)
           (subseq rest 0 slash-pos)))
      (colon-pos (subseq rest 0 colon-pos))
      (slash-pos (subseq rest 0 slash-pos))
      (t rest))))

(defun replace-server-in-url (url new-ip &optional new-port)
  (let* ((start (search "://" url))
         (rest (subseq url (+ start 3)))
         (colon-pos (position #\: rest))
         (slash-pos (position #\/ rest))
         (old-server (cond
                       ((and colon-pos slash-pos)
                        (if (< colon-pos slash-pos)
                            (subseq rest 0 slash-pos)
                            (subseq rest 0 slash-pos)))
                       (colon-pos (subseq rest 0 colon-pos))
                       (slash-pos (subseq rest 0 slash-pos))
                       (t rest)))
         (new-server (if new-port
                         (format nil "~a:~a" new-ip new-port)
                         new-ip)))
    (concatenate 'string
                 (subseq url 0 (+ start 3))
                 new-server
                 (subseq url (+ start 3 (length old-server))))))

;; =============================================
;; R E N D E R
;; =============================================

(defun render-index ()
  (let* ((raw (fetch-video-list))
         (videos (mapcar #'make-video raw)))
    (djula:render-template* +index.html+
                            nil
                            :videos videos)))

(defun render-player (id)
  (let ((video (fetch-video id)))
    ;; (format t "~a~%" video)
    (if video
        (progn
          (setf (getf video :HLS-URL)
                (replace-server-in-url
                 (getf video :HLS-URL) *proxy-address* *proxy-port*))
          (format t "[~a]~%" video)
          (render-template* +player.html+
                            nil
                            :video video))
        (progn
          (setf (tbnl:return-code*) 404)
          "Video not found"))))

;; =============================================
;; R O U T E
;; =============================================

(defroute index-route ("/" :method :get) ()
  (render-index))

(defroute video-route ("/video/:id" :method :get) (&path (id 'string))
  (render-player id))

