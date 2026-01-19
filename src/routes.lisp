
(in-package :yano)

;;===========================================
;; R O U T E
;;===========================================

(defroute root ("/" :method :get) ()
  ;;; Juts the welcome page
  (render "templates/index.html"))

(defroute videos-root ("/videos" :method :get) ()
  (render "templates/videos.html" :videos (get-videos)))

(defroute video-root ("/video/:n" :method :get) ()
  (render "templates/video.html" :video (get-video n)))
