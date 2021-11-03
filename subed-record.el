;;; subed-record.el --- Record audio in segments and compile it into a file  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Version: 0.1
;; Package-Requires: ((hydra) (mpv)
;; 
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; subed-record makes it easier to prepare text for subtitles, record a
;; voiceover, and then compile a video that includes the visuals, audio,
;; and subtitles specified by the text file.
;; 
;; For more information, see README.org.

;;; Code:

(defgroup subed-record nil
  "Tools for recording and compiling presentations."
  :group 'subed
  :prefix "subed-record")

(defcustom subed-record-ffmpeg-executable "ffmpeg"
  "FFMPEG command."
  :group 'subed-record
  :type 'string)

(defcustom subed-record-extension ".wav"
  "Extension for recording voiceovers."
  :group 'subed-record
  :type 'string)

(defcustom subed-record-spacing 100
  "Milliseconds to skip before and after the accept command.
This should account for the sound of the keystroke."
  :type 'integer
  :group 'subed-record)

(defcustom subed-record-backend 'ffmpeg
  "Recording backend to use."
  :type '(choice (:const 'sox)
                 (:const 'obs)
                 (:const 'ffmpeg)
                 (:const nil))
  :group 'subed-record)

(defcustom subed-record-sox-executable "rec"
  "Rec command used if `subed-record-backend' is set to 'sox."
  :group 'subed-record
  :type 'string)

(defcustom subed-record-sox-args (list "-r" "48000" "-c" "1")
  "Extra arguments for sox recording process."
  :type '(repeat string)
  :group 'subed-record)

(defcustom subed-record-ffmpeg-args '("-f" "alsa" "-i" "hw:0" "-y")
  "Arguments to pass to ffmpeg for recording.
Use 'arecord -l' at the command line to find out what device to use."
  :type '(repeat string)
  :group 'subed-record)

(defvar subed-record-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up] #'subed-backward-subtitle-text)
    (define-key map [down] #'subed-forward-subtitle-text)
    (define-key map [left] #'subed-record-retry)
    (define-key map [right] #'subed-record-accept-segment)
    (define-key map [q] #'subed-record-minor-mode)
    (define-key map (kbd "RET") #'subed-record-minor-mode)
    map))

(defvar subed-record-filename nil)
(defvar subed-record-start-time nil "Emacs timestamp from when the recording was started.")
(defvar subed-record-process nil "Process for recording.")
(defvar subed-record-compile-ffmpeg-conversion-process nil "Process for compiling.")

;;; Recording

;;;###autoload
(defun subed-record ()
  "Turn on subed-record-minor-mode."
  (interactive)
  (subed-record-minor-mode 1))

;;;###autoload
(define-minor-mode subed-record-minor-mode
  "Minor mode for recording segments using the subtitles in the buffer."
  :lighter " REC"
  (if subed-record-minor-mode
      (progn
        (subed-record-start-recording (concat (file-name-sans-extension (buffer-file-name)) subed-record-extension))
        (message "Recording..."))
    (subed-record-stop-recording)
    (message "Stopped.")))

(defun subed-record-start-recording (filename)
  "Start recording. Save results to FILENAME."
  (interactive)
  (setq subed-record-filename filename)
  (setq subed-record-start-time (current-time))
  (if (process-live-p subed-record-process)
      (quit-process subed-record-process))
  (cond
   ((eq subed-record-backend 'obs)
    (when (not (websocket-openp obs-websocket))
      (obs-websocket-connect))
    (obs-websocket-send "SetRecordingFolder" :rec-folder (expand-file-name (file-name-directory filename)))
    (obs-websocket-send "SetFilenameFormatting" :filename-formatting (file-name-nondirectory filename))
    (obs-websocket-send "StartRecording"))
   ((eq subed-record-backend 'ffmpeg)
    (subed-record-ffmpeg-start filename))
   ((eq subed-record-backend 'sox)
    (subed-record-sox-start filename))))

(defun subed-record-offset-ms (&optional time)
  "Milliseconds since the start of recording."
  (* (float-time (time-subtract (or time (current-time)) subed-record-start-time)) 1000.0)) 

(defun subed-record-stop-recording (&optional time)
  "Finish recording."
  (interactive)
  (if (eq (subed-subtitle-msecs-stop) 0)
      (subed-record-accept-segment))
  (when (process-live-p subed-record-process) (quit-process subed-record-process))
  (when (eq subed-record-backend 'obs)
    (obs-websocket-send "StopRecording")))

;;; Converting from Org

(defun subed-record-org-to-vtt ()
  "Create a WebVTT file based on the current Org subtree.
Captions and file links are included. HTTP links are removed.
Captions are split at lines ending with a \".\" or at new
paragraphs. The subtitles are written to a file based on the name
of the current file, but ending with \".vtt\". NOTE: Existing
files are overwritten."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-end-of-meta-data)
    (let ((data
           (replace-regexp-in-string
            "\\[\\[http.*?\\]\\[\\(\\([^]]+\\|\n\\)*?\\)\\]\\]"
            "\\2"
            (replace-regexp-in-string "\n[ \t\n]*\n" "\n"
                                      (buffer-substring-no-properties
                                       (point) 
                                       (org-end-of-subtree)))))
          filename)
      (with-temp-file filename
        (insert "WEBVTT\n\n"
                (mapconcat
                 (lambda (s)
                   (concat "00:00:00.000 --> 00:00:00.000\n" (string-trim s)))
                 (split-string
                  (replace-regexp-in-string
                   "\n[ \t]+"
                   "\n"
                   (replace-regexp-in-string "\\.[ \t]*\n\n*" ".\n\n" data))
                  "[ \t]*\n\\([ \t]*\n\\)+")
                 "\n\n")))
      (find-file filename))))

;;; Using ffmpeg to record

(defun subed-record-ffmpeg-start (filename)
  "Start recording into FILENAME."
  (with-current-buffer (get-buffer-create "*ffmpeg*")
    (erase-buffer)
    (setq subed-record-process
          (apply 'start-process "ffmpeg"
		             (current-buffer)
		             subed-record-ffmpeg-executable
		             (append subed-record-ffmpeg-args (list filename) nil)))))

;;; Using sox to record


(defun subed-record-sox-start (filename)
  "Start recording into FILENAME."
  (interactive)
  (if (process-live-p subed-record-process)
      (quit-process subed-record-process))
  (setq subed-record-process
        (apply
         'start-process
         "sox"
         nil
         subed-record-sox-executable
         filename
         subed-record-sox-args)))

;;; Working with segments

(defun subed-record-accept-segment ()
  "Save the current timestamps for this segment and move to the next one."
  (interactive)
  (let ((end-time (subed-record-offset-ms)))
    (subed-set-subtitle-time-stop end-time)
    (when (subed-forward-subtitle-text)
      (subed-set-subtitle-time-start end-time)
      (subed-set-subtitle-time-stop 0)
      (recenter-top-bottom))))

(defun subed-record-retry ()
  "Try recording this segment again."
  (interactive)
  (subed-set-subtitle-time-start (subed-record-offset-ms))
  (subed-set-subtitle-time-stop 0))

;;; Compiling

(defcustom subed-record-compile-description-height 50 "Number of pixels for top description in video.
If nil, omit the description."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-caption-height 75 "Number of pixels to leave at the bottom for captions in video.
If nil, do not leave space for captions."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-output-video-width 1280 "Video will be the specified number of pixels wide."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-output-video-height 720 "Video will be the specified number of pixels tall."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-output-filename "output.webm" "Output filename."
  :type 'file :group 'subed-record)
(defcustom subed-record-compile-description-drawtext-filter-params "fontcolor=white:x=5:y=5:fontsize=40"
  "Additional filter arguments for drawing the visual description."
  :type 'string :group 'subed-record)

(defcustom subed-record-compile-info-functions '(subed-record-compile-get-subtitle-info
                                                 subed-record-compile-get-visuals-from-org
                                                 subed-record-compile-get-audio-info)
  "Functions to run to get information for the current segment.
Functions should accept one argument, the plist, and return the
modified plist."
  :type '(list function)
  :group 'subed-record)

(defun subed-record-compile-get-subtitle-info (info)
  "Get information from the current subtitle."
  (setf info (plist-put info :caption (subed-subtitle-text)))
  (setf info (plist-put info :start-ms (subed-subtitle-msecs-start)))
  (setf info (plist-put info :stop-ms (subed-subtitle-msecs-stop)))
  info)

(defun subed-record-compile-get-visuals-from-org (info)
  "Get visual information from Org syntax and store it in INFO."
  (let ((caption (plist-get info :caption)))
    (when (string-match "#\\+CAPTION: \\(.+?\\)\n" caption)
      (setq info (plist-put info :visual-description (match-string 1 caption)))
      (setq caption (replace-match "" nil t caption)))
    (when (string-match "\\[\\[file:\\([^]]+?\\)\\].+\n" caption)
      (setq info (plist-put info :visual-file (match-string 1 caption)))
      (setq caption (replace-match "" nil t caption)))
    (setq info (plist-put info :caption caption))
    info))

(defun subed-record-compile-get-audio-info (info)
  "Get the audio file or current media file."
  (let ((caption (plist-get info :caption)))
    (if (string-match "#\\+AUDIO: \\(.+?\\)\n" caption)
        (progn
          (setq audio-file (match-string 1 caption))
          (setq caption (replace-match "" nil t caption)))
      (setq audio-file subed-mpv-video-file))
    (setq info (plist-put info :caption caption))
    (setq info (plist-put info :audio-file audio-file))))

(defun subed-record-compile-get-selection-for-region (beg end)
  (interactive "r")
  (goto-char beg)
  (let (result)
    (subed-for-each-subtitle beg end nil
      (setq result
            (cons
             (seq-reduce
              (lambda (val f) (funcall f val))
              subed-record-compile-info-functions
              nil)
             result)))
    (reverse result)))

(defun subed-record-compile-get-duration-ms (filename)
  (* 1000
     (string-to-number
      (shell-command-to-string
       (concat "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "
               (shell-quote-argument (expand-file-name filename)))))))

(defun subed-record-compile-get-frames (filename)
  "Return the number of frames for an animated GIF at FILENAME."
  (string-to-number
   (shell-command-to-string
    (concat "ffprobe -v error -select_streams v:0 -count_packets -show_entries stream=nb_read_packets -of csv=p=0 "
            (shell-quote-argument (expand-file-name filename))))))

(defun subed-record-compile-scale-filter (o)
  "Return the complex filter for scaling O."
  (seq-let (start-ms end-ms caption description) o
    (format "scale=%d:%d:force_original_aspect_ratio=decrease,setsar=sar=1,pad=%d:%d:(ow-iw)/2:%d+(oh-%d-%d-ih)/2"
            subed-record-compile-output-video-width
            (- subed-record-compile-output-video-height (or subed-record-compile-caption-height 0) (or subed-record-compile-description-height 0))
            subed-record-compile-output-video-width
            subed-record-compile-output-video-height
            subed-record-compile-description-height
            subed-record-compile-description-height
            subed-record-compile-caption-height)))

(defun subed-record-compile-ffmpeg-make-description-filter (description)
  "Return the FFMPEG filter needed to add DESCRIPTION as text."
  (if description
      (concat ",drawtext=" subed-record-compile-description-drawtext-filter-params ":text='"
              description ; TODO quote this properly
              "'")
    ""))

(defun subed-record-compile-ffmpeg-prepare-video (visual-file duration scale description-filter i)
  "Return ffmpeg arguments for videos."
  (list
   :input
   (list "-i" visual-file)
   :filter
   (let ((video-duration (subed-record-compile-get-duration-ms visual-file)))
     (format
      "[%d:v]setpts=PTS*%.3f,%s%s[r%d];"
      i
      (/ duration video-duration)
      scale
      description-filter
      i))))

(defun subed-record-compile-ffmpeg-prepare-animated-gif (visual-file duration scale description-filter i)
  (let ((gif-frames (subed-record-compile-get-frames visual-file)))
    (list
     :input
     (list "-r" (format "%.3f" (/ gif-frames (/ duration 1000.0))) "-i" visual-file)
     :filter
     ;; (format "-i %s" filename)
     (format "[%d:v]%s%s[r%d];" i scale description-filter i))))

(defun subed-record-compile-ffmpeg-prepare-static-image (visual-file duration scale description-filter i)
  (list
   :input
   (list "-loop" "1" "-t" (format "%.3f" (/ duration 1000.0)) "-i" visual-file)
   :filter
   (format "[%d:v]%s%s[r%d];" i scale description-filter i)))

(defun subed-record-compile-selection-visuals (selection)
  "Return selection segments containing visuals with adjusted durations."
  (let (current result)
    (while selection
      (when (plist-get (car selection) :visual-file)
        (setq current (car selection))
        (setf current (plist-put current :visual-duration 0))
        (setq result (cons current result)))
      (setf current (plist-put current :visual-duration (+ (plist-get current :visual-duration)
                                                           (- (plist-get (car selection) :stop-ms)
                                                              (plist-get (car selection) :start-ms)))))
      (setq selection (cdr selection)))
    (nreverse result)))

(defun subed-record-compile-format-selection-as-visuals (selection)
  (let* (info filter input visuals)
    (setq visuals (subed-record-compile-selection-visuals selection))
    (setq info
          (seq-map-indexed
           (lambda (o i)
             (let* ((visual-description (plist-get o :visual-description))
                    (visual-file (plist-get o :visual-file))
                    (visual-duration (plist-get o :visual-duration))
                    (description-filter (subed-record-compile-ffmpeg-make-description-filter visual-description))
                    (scale (subed-record-compile-segment-scale-filter o)))
               (funcall 
                (cond
                 ((string-match "mp4" visual-file) 'subed-record-compile-ffmpeg-prepare-video)
                 ((string-match "gif$" visual-file) 'subed-record-compile-ffmpeg-prepare-animated-gif)
                 (t 'subed-record-compile-ffmpeg-prepare-static-image))
                visual-file visual-duration scale description-filter i)))
           visuals))
    (setq filter (list
                  (mapconcat (lambda (o) (plist-get o :filter)) info "")
                  (mapconcat
                   (lambda (o) (format "[r%d]" o))
                   (number-sequence 0 (1- (length visuals)))
                   "")
                  (format "concat=n=%d:v=1:a=0[v]" (length visuals))))
    (setq input (apply 'seq-concatenate 'list (mapcar (lambda (o) (plist-get o :input)) info)))
    (list :input input :filter (string-join filter "")
          :output (list "-map" "[v]:v" "-c:v" "vp8" "-vsync" "2" "-b:v" "800k")
          :input-count (length visuals))))

(defun subed-record-compile-format-selection-as-audio (list &rest args)
  "LIST is a plist of (:start-ms ... :end-ms ... :audio-file)
ARGS: :start-input should have the numerical index for the starting input file."
  (let ((temp list) groups current previous)
    (while temp
      (setq current
            (seq-take-while
             (lambda (o)
               (prog1 (or (null previous)
                          (and 
                           (string= (plist-get o :audio-file) (plist-get previous :audio-file))
                           (>= (plist-get o :start-ms) (plist-get previous :stop-ms))))
                 (setq previous o)))
             temp))
      (setq groups (cons
                    (list
                     :input (list "-i" (plist-get (car current) :audio-file))
                     :filter (format "aselect='%s',asetpts='N/SR/TB'"
                                     (mapconcat
                                      (lambda (o)
                                        (format "between(t,%.3f,%.3f)"
                                                (/ (plist-get o :start-ms) 1000.0)
                                                (/ (plist-get o :stop-ms) 1000.0)))
                                      current
                                      "+")
                                     ))
                    groups))
      (setq temp (seq-drop temp (length current)))
      (setq previous nil))
    (setq groups (reverse groups))
    (list
     :input
     (seq-mapcat (lambda (o) (plist-get o :input)) groups)
     :filter
     (concat
      (string-join (seq-map-indexed
                    (lambda (o i)
                      (format "[%d]%s[%s]"
                              (+ (plist-get args :start-input) i)
                              (plist-get o :filter)
                              (if (> (length groups) 1)
                                  (format "a%d" i)
                                "a")))
                    groups)
                   ";")
      (if (> (length groups) 1)
          (concat
           ";"
           (mapconcat (lambda (sink) (format "[a%d]" sink)) (number-sequence 0 (1- (length groups))) "")
           (format "concat=n=%d:v=0:a=1[a]" (length groups)))
        ""))
     :input-count (length groups)
     :output
     (list "-map:a" "[a]" "-acodec" "libvorbis"))))

(defun subed-record-compile-audio (&optional beg end &rest args)
  "Compile just the audio."
  (interactive)
  (apply 'subed-record-compile-video (append (list beg end '(audio subtitles)) args)))

(defun subed-record-compile-try-flow (&optional beg end)
  "Try a segment to see if the audio flows well."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))))
  (save-excursion
    (subed-record-compile-audio
     beg end
     :sentinel
     (lambda (process event)
       (when (string-match "finished" event)
         (mpv-play subed-record-compile-output-filename))))))

;;https://emacs.stackexchange.com/questions/48256/how-to-have-a-buffer-interpret-c-m-as-an-actual-carriage-return
(defun subed-record-compile-process-filter-function (proc input-string)
  "Handle ^M for progress reporting."
  (let ((proc-buf (process-buffer proc)))
    (when (buffer-live-p proc-buf)
      (with-current-buffer proc-buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (if (not (string= "\r" (substring input-string 0 1)))
                (insert input-string)
              (delete-region (line-beginning-position) (line-end-position))
              (insert (substring input-string 1)))))))))

(defun subed-record-compile-video (&optional beg end include &rest args)
  "Create output file with video, audio, and subtitles.
INCLUDE should be a list of the form '(video audio subtitles)."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))
                     '(video audio subtitles)))
  (if (string= (expand-file-name
                (concat (file-name-sans-extension (buffer-file-name))
                        subed-record-extension))
               (expand-file-name subed-record-compile-output-filename))
      (error "Compiling to %s would overwrite the source recording. Please change `subed-record-compile-output-filename'."
             sube-record-compile-extension))
  (setq include (or include '(video audio subtitles)))
  (let* ((selection (subed-record-compile-get-selection-for-region (or beg (point-min)) (or end (point-max))))
         (subtitle-file
          (when (member 'subtitles include)
            (make-temp-file
             (file-name-sans-extension (buffer-file-name)) nil ".vtt"
             (insert (subed-record-compile-format-as-vtt selection)))))
         (visual-args
          (when (member 'video include)
            (subed-record-compile-format-selection-as-visuals selection)))
         (audio-args
          (when (member 'audio include)
            (subed-record-compile-format-selection-as-audio selection :start-input (or (plist-get visual-args :input-count) 0))))
         (ffmpeg-args
          (append
           (plist-get visual-args :input)
           (plist-get audio-args :input)
           (list "-filter_complex" (string-join
                                    (delq nil
                                          (list
                                           (plist-get visual-args :filter)
                                           (plist-get audio-args :filter)))
                                    ";"))
           (when (member 'subtitles include)
             (list "-i" subtitle-file "-map"
                   (format "%d:s"
                           (apply '+
                                  (delq nil
                                        (list (plist-get visual-args :input-count)
                                              (plist-get audio-args :input-count)
                                              0))))))
           (plist-get visual-args :output)
           (plist-get audio-args :output)
           (list "-y" subed-record-compile-output-filename)
           nil)))
    (with-current-buffer (get-buffer-create "*ffmpeg*")
      (when (process-live-p subed-record-compile-ffmpeg-conversion-process)
        (quit-process subed-record-compile-ffmpeg-conversion-process))
      (erase-buffer)
      (kill-new (concat "ffmpeg " (mapconcat 'shell-quote-argument ffmpeg-args " ")))
      (insert "ffmpeg " (mapconcat 'shell-quote-argument ffmpeg-args " ") "\n")
      (setq subed-record-compile-ffmpeg-conversion-process
            (apply 'start-process "ffmpeg" (current-buffer) subed-record-ffmpeg-executable ffmpeg-args))
      (set-process-coding-system subed-record-compile-ffmpeg-conversion-process 'utf-8-dos 'utf-8-dos)
      (set-process-filter subed-record-compile-ffmpeg-conversion-process 'subed-record-compile-process-filter-function)
      (set-process-sentinel subed-record-compile-ffmpeg-conversion-process
                            (lambda (process event)
                              (when (save-match-data (string-match "finished" event))
                                (when subtitle-file (delete-file subtitle-file)))
                              (when (plist-get args :sentinel)
                                (funcall (plist-get args :sentinel) process event))))
      (display-buffer (current-buffer)))))

(defun subed-record-compile-test-visuals (&optional limit)
  (interactive "p")
  (let* ((visuals
          (seq-filter (lambda (o) (plist-get o :visual-file))
                      (subed-record-compile-get-selection-for-region (point-min) (point-max))))
         (formatted-visuals
          (subed-record-compile-format-selection-as-visuals
           (seq-map
            (lambda (o)
              (setf o (plist-put o :stop-ms (+ 1000 (plist-get o :start-ms))))
              o)
            (if (and (numberp limit) (> limit 1)) (seq-take visuals limit) visuals))))
         (args
          (append
           (plist-get formatted-visuals :input)
           (list "-filter_complex" (plist-get formatted-visuals :filter))
           (plist-get formatted-visuals :output)
           (list "-y" "visuals.webm"))))
    (with-current-buffer (get-buffer-create "*ffmpeg*")
      (erase-buffer)
      (insert "ffmpeg " (mapconcat 'shell-quote-argument args " ") "\n")
      (apply 'call-process subed-record-ffmpeg-executable nil t nil args)
      (display-buffer (current-buffer)))))

(defun subed-record-compile-format-as-audacity-labels (list)
  "LIST is a list of (start-ms end-ms text)."
  (mapconcat
   (lambda (o)
     (format "%0.3f\t%0.3f\t%s\n"
             (/ (plist-get o :start-ms) 1000.0)
             (/ (plist-get o :stop-ms) 1000.0)
             (string-trim (replace-regexp-in-string "[\t\n]+" " " (plist-get o :caption)))))
   list
   ""))

(defun subed-record-compile-format-as-vtt (list)
  (let ((ms 0))
    (concat "WEBVTT\n\n"
            (mapconcat
             (lambda (o)
               (prog1
                   (format "%s --> %s\n%s\n\n"
                           (my-msecs-to-timestamp ms)
                           (my-msecs-to-timestamp (+ ms (- (plist-get o :stop-ms)
                                                           (plist-get o :start-ms))))
                           (replace-regexp-in-string "#\\+.*?\n\\|\\[[file.*?]]\n" ""
                                                     (plist-get o :caption)))
                 (setq ms (+ ms (- (plist-get o :stop-ms)
                                   (plist-get o :start-ms))))))
             list
             ""))))

(defun subed-record-export-audacity-labels (&optional beg end)
  (interactive "r")
  (with-temp-file (concat
                   (file-name-base (buffer-file-name))
                   ".txt")
    (insert (my-record-format-as-audacity-labels
             (with-current-buffer (current-buffer)
               (unless (region-active-p)
                 (setq beg (point-min) end (point-max)))
               (my-record-get-selection-for-region beg end)))))) 


(provide 'subed-record)

;;; subed-waveform.el ends here
