;;;================================================================
(require 'transient)

(defclass ffmpeg-option (transient-option) ()
  "Class used for command-line argument that can take a value.")

(cl-defmethod transient-infix-value ((obj ffmpeg-option))
  (when-let ((value (oref obj value)))
    (concat (oref obj argument) " " value)))

(cl-defmethod transient-format-value ((obj ffmpeg-option))
    (let ((value (oref obj value)))
      (propertize (concat (oref obj argument) (when value " ") value)
                  'face (if value
                            'transient-value
                          'transient-inactive-value))))

;;;================================================================
;; Infix arguments to ffmpeg
;;;================================================================

;; ffmpeg -i out.mp4 -vf "fps=10,scale=600:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 embark-prefix-arg.gif
;; ffmpeg -ss 30 -t 3 -i out.mp4 -vf "fps=10,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 embark-shell-command-on-buffer.gif
;; ffmpeg -i /tmp/outiwGcU7JJXZ.avi  -vcodec libx265 -crf 26 formula2b.mp4
;; ffmpeg -i impulse_response_unstable_omega_no_axes.avi -vcodec libx264 -acodec libfaac impulse_response_unstable_omega_no_axes.mp4

;; ffmpeg -vaapi_device /dev/dri/renderD128 \
;;            -f alsa -i default -c:a flac \
;;            -f x11grab \
;;            -framerate 30 \
;;            -video_size 1920x1200 \
;;            -i $DISPLAY+0,0 \
;;            -vf 'hwupload,scale_vaapi=format=nv12' -c:v h264_vaapi -qp 24 \
;;            "$HOME/screencast-$(date '+%y%m%d-%H%M-%S').mp4" 
;; ffcast -s % ffmpeg -y -f x11grab -show_region 1 -framerate 15 \
;;     -video_size %s -i %D+%c -codec:v huffyuv                  \
;;     -vf crop="iw-mod(iw\\,2):ih-mod(ih\\,2)" $TMP_AVI
;; ffmpeg -i $TMP_AVI  -vcodec libx265 -crf 26 out.mp4
;; && convert -set delay 10 -layers Optimize $TMP_AVI out.gif

;; ffcast -w % ffmpeg -y -f x11grab -show_region 1 -framerate 30 \
;;     -video_size %s -i %D+%c -pix_fmt yuv420p -movflags +faststart \
;;     -codec:v libx264 -crf 25 \
;;     -vf crop="iw-mod(iw\\,2):ih-mod(ih\\,2)" $TMP_AVI
;; mv $TMP_AVI out.mp4

;;----------------------------------------------------------------
;; Input file arguments
;;----------------------------------------------------------------
(transient-define-argument ffmpeg-dispatch:-i ()
  :description "Input file"
  :class 'ffmpeg-option
  :key "-i"
  :argument "-i"
  :prompt "Input file: "
  :reader 'ffmpeg-read-file-name
  )

(defun ffmpeg-read-file-name (prompt initial-input history)
  (read-file-name prompt nil nil t initial-input))

(transient-define-argument ffmpeg-dispatch:-f ()
  :description "Input source format"
  :class 'ffmpeg-option
  :key "-f"
  :argument "-f"
  :prompt "Input stream"
  :reader 'ffmpeg-read-video-format
  )

(defun ffmpeg-read-video-format (prompt initial-input history)
  (completing-read prompt '(x11grab )))

(transient-define-argument ffmpeg-dispatch:-s ()
  :description "Size WxH pixels"
  :class 'ffmpeg-option
  :key "-s"
  :argument "-video_size"
  :prompt "Size WxH: "
  )

(transient-define-argument ffmpeg-dispatch:-vf ()
  :description "Filtergraph"
  :class 'ffmpeg-option
  :key "-vf"
  :argument "-filter:v"
  :prompt "Video filter: "
  )
;;----------------------------------------------------------------
;;; Output file options
;;----------------------------------------------------------------

(transient-define-argument ffmpeg-dispatch:-af ()
  :description "Audio source"
  :class 'ffmpeg-option
  :key "-af"
  :argument "-f"
  :prompt "Input stream"
  :reader 'ffmpeg-read-audio-format
  )

(defun ffmpeg-read-audio-format (prompt initial-input history)
  (completing-read prompt '(alsa oss)))

(transient-define-argument ffmpeg-dispatch:-r ()
  :description "Output frame rate"
  :class 'ffmpeg-option
  :key "-r"
  :argument "-framerate"
  :reader 'transient-read-number-N+
  )

(transient-define-argument ffmpeg-dispatch:-c:v ()
  :description "Encoder"
  :class 'ffmpeg-option
  :key "-c:v"
  :argument "-codec:v"
  :reader 'ffmpeg-read-video-codec
  )

(defun ffmpeg-read-video-codec (prompt initial-input history)
  (completing-read prompt '("h264_vaapi"
                            "libx264"
                            "libx265")
                   nil t))

(defun ffmpeg-codec:v-p ()
  (member "-codec:v" (delq nil current-transient-suffixes))
  )

(transient-define-argument ffmpeg-dispatch:-c:a ()
  :description "Encoder"
  :class 'ffmpeg-option
  :key "-c:a"
  :argument "-codec:a"
  :reader 'ffmpeg-read-audio-codec
  )

(defun ffmpeg-read-audio-codec (prompt initial-input history)
  (completing-read prompt '("alsa" "oss") nil t))

(defun ffmpeg-read-profile (prompt initial-input history)
  "docstring"
  (interactive)
  (completing-read prompt '("profile1" "profile2" "profile3") nil t initial-input)
  )

(transient-define-argument ffmpeg-dispatch:-pix_fmt ()
  :description "Pixel format"
  :class 'ffmpeg-option
  :key "-pf"
  :argument "-pix_fmt"
  :reader 'ffmpeg-read-pixfmt
  )

(defun ffmpeg-read-pixfmt (prompt initial-input history)
  (interactive)
  (completing-read prompt '("yuv420p" "rgb24" "huffyuv") nil t initial-input)
  )

(transient-define-argument ffmpeg-dispatch:-crf ()
  ;; :if (ffmpeg-crf-needed-p)
  :description "Constant Rate Factor"
  :class 'ffmpeg-option
  :key "-crf"
  :argument "-crf"
  :reader 'transient-read-number-N+
  )

(defun ffmpeg-crf-needed-p ()
  (seq-some (lambda (value) (string-match-p "libx265" value))
            (transient-args transient-current-command))
  ;; (member (transient-infix-value transient-current-suffixes)
  ;;         '("libx265"
  ;;           "libx264"
  ;;           "h264_vaapi"))
  )



;;;----------------------------------------------------------------
;; Transient commands for ffmpeg
;;;----------------------------------------------------------------
(transient-define-prefix ffmpeg-dispatch ()
  "Start a new FFMPEG job"
  :man-page "ffmpeg"
  ["Input options"
   (ffmpeg-dispatch:-i)
   (ffmpeg-dispatch:-f)
   ("-y" "Overwrite output" "-y")
   ]
  [["Output audio"
    (ffmpeg-dispatch:-af)
    (ffmpeg-dispatch:-c:a)
    ]
   ["Output video"
    (ffmpeg-dispatch:-r)
    (ffmpeg-dispatch:-c:v)
    ("-crf" "Constant Rate Fac" ffmpeg-dispatch:-crf :if ffmpeg-crf-needed-p)
    (ffmpeg-dispatch:-s)
    (ffmpeg-dispatch:-pix_fmt)
    (5 ffmpeg-dispatch:-vf)
    ]
   [:if ffmpeg-codec:v-p
    "Encoder details"]
   ]
  [["Run"
    ("f" "ffmpeg" ffmpeg-echo-arguments)
    ]
   ["Show"
    ("w" "As shell command" ffmpeg-echo-arguments)
    ]
   ["Presets"
    ("L" "load preset" ffmpeg-preset-load)
    ("S" "Save preset" ffmpeg-preset-save)
    ]
   ]
  )

(transient-define-suffix ffmpeg-preset-load ()
  "Load a preset argument list"
  :transient 'transient--do-stay
  (interactive)
  (let* ((preset-name (completing-read "Load preset: "
                                       (mapcar 'car ffmpeg-dispatch-presets)))
         (saved-values (alist-get preset-name ffmpeg-dispatch-presets nil nil #'string=)))
    (with-demoted-errors "Error: %S"
      (let* ((obj transient--prefix))
        (oset obj value saved-values)
        (mapc #'transient-init-value transient--suffixes)))))

(defvar ffmpeg-dispatch-presets nil
  "Alist of presets for ffmpeg-dispatch")

(transient-define-suffix ffmpeg-preset-save ()
  "Save an argument list as a preset"
  :transient 'transient--do-stay
  (interactive)
   (let ((preset-name (completing-read "Save to preset: "
                                       (mapcar 'car ffmpeg-dispatch-presets)))
         (transient-current-prefix transient--prefix)
         (transient-current-suffixes transient--suffixes))
    (setf (alist-get preset-name ffmpeg-dispatch-presets nil nil #'string=)
          (transient-get-value))))

(defun ffmpeg-echo-arguments (&optional args)
  (interactive
   (list (transient-args 'ffmpeg-dispatch)))
  (message (concat "ffmpeg " (string-join args " ")))
  (prin1 (transient-args transient-current-command)))

(global-set-key (kbd "C-c t") 'ffmpeg-dispatch)

  ;; (setq transient-current-prefix   transient--prefix
  ;;       transient-current-suffixes transient--suffixes)
  ;; (ffmpeg-dispatch-preset-save-func)
  ;; (setq ffmpeg-dispatch-saved (transient-get-value))
  
  ;; (prin1 (mapcar (lambda (obj) (list (oref obj command)
  ;;                               (oref obj key)
  ;;                               (if (slot-exists-p obj 'value)
  ;;                                   (oref obj value))))
  ;;                transient--suffixes))

  ;; (oset transient--prefix value '("-video_size 1920x1080"))
  ;; (mapc #'transient-init-value transient--suffixes)

;; (ffmpeg-dispatch)

;; (transient-define-suffix magit-log-an-argument-preset ()
;;   :transient 'transient--do-stay
;;   (interactive)
;;   (oset transient--prefix value '("--patch" "--stat"))
;;   (mapc #'transient-init-value transient--suffixes))

;; (transient-append-suffix 'magit-log "h"
;;   '("X" "use some argument preset" magit-log-an-argument-preset))
