;;; subed-record.el --- Record audio in segments and compile it into a file  -*- lexical-binding: t; -*-

;; Author: Sacha Chua <sacha@sachachua.com>
;; Version: 0.2
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

(require 'compile-media)

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
  :type '(choice (const sox)
                 (const obs)
                 (const obs-old)
                 (const ffmpeg)
                 (const nil))
  :group 'subed-record)

(defcustom subed-record-sox-executable "rec"
  "Rec command used if `subed-record-backend' is set to 'sox."
  :group 'subed-record
  :type 'string)

(defcustom subed-record-sox-args (list "-d" "-r" "48000" "-c" "1" "--type" "wav")
  "Extra arguments for sox recording process."
  :type '(repeat string)
  :group 'subed-record)

(defcustom subed-record-ffmpeg-args '("-f" "alsa" "-i" "default" "-y")
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
	(setq-local subed-enforce-time-boundaries nil)
  (subed-record-minor-mode 1))

;;;###autoload
(define-minor-mode subed-record-minor-mode
  "Minor mode for recording segments using the subtitles in the buffer."
  :lighter " REC"
  (if subed-record-minor-mode
      (progn
        (subed-record-start-recording
				 (concat (file-name-sans-extension (buffer-file-name))
								 "-"
								 (format-time-string "%Y-%m-%d-%H%M%S")
								 subed-record-extension))
        (message "Recording..."))
    (subed-record-stop-recording)
    (message "Stopped.")))

(defun subed-record-start-recording (filename)
  "Start recording. Save results to FILENAME."
  (interactive (list (if (or current-prefix-arg (not (subed-media-file)))
                         (read-file-name "File: ")
                       (subed-media-file))))
  (setq subed-record-filename filename)
  (setq subed-record-start-time (current-time))
  (if (process-live-p subed-record-process)
      (quit-process subed-record-process))
  (pcase subed-record-backend
   ('obs-old
    (when (not (websocket-openp obs-websocket))
      (obs-websocket-connect))
    (obs-websocket-send "SetRecordingFolder" :rec-folder (expand-file-name (file-name-directory filename)))
    (obs-websocket-send "SetFilenameFormatting" :filename-formatting (file-name-nondirectory filename))
    (obs-websocket-send "StartRecording"))
   ('obs
    (when (not (websocket-openp obs-websocket))
      (obs-websocket-connect))
		;; filename is ignored. StopRecord will set filename
    (obs-websocket-send "GetRecordDirectory")
    (obs-websocket-send "StartRecord"))
   ('ffmpeg
    (subed-record-ffmpeg-start filename))
   ('sox
    (subed-record-sox-start filename))))

(defun subed-record-offset-ms (&optional time)
  "Milliseconds since the start of recording."
  (* (float-time (time-subtract (or time (current-time)) subed-record-start-time)) 1000.0))

(defun subed-record-get-filename-from-obs-stop-record-callback (_frame payload)
  (when-let (d (plist-get payload :d))
    (when-let (requestStatus (plist-get d :requestStatus))
      (when-let (result (plist-get requestStatus :result))
	(when-let (responseData (plist-get d :responseData))
	  (when-let (fn (plist-get responseData :outputPath))
	    (unless fn
	      (error "Failed to get output filename on StopRecord request")
	      (setq subed-record-filename fn))))))))

(defun subed-record-stop-recording (&optional time)
  "Finish recording."
  (interactive)
	(when (derived-mode-p 'subed-mode)
		(when (eq (subed-subtitle-msecs-stop) 0)
			(subed-record-accept-segment)))
  (when (process-live-p subed-record-process) (quit-process subed-record-process))
	(pcase subed-record-backend
		('obs-old (obs-websocket-send "StopRecording"))
		('obs (obs-websocket-send "StopRecord" #'subed-record-get-filename-from-obs-stop-record-callback))))

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
  (interactive (list (read-file-name "File: ")))
  (with-current-buffer (get-buffer-create "*ffmpeg*")
    (erase-buffer)
    (setq subed-record-process
          (apply 'start-process "ffmpeg"
		             (current-buffer)
		             subed-record-ffmpeg-executable
		             (append subed-record-ffmpeg-args (list (expand-file-name filename)) nil)))
		(display-buffer (current-buffer))))

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
         "*sox*"
         subed-record-sox-executable
         (append subed-record-sox-args (list filename)))))

;;; Working with segments

(defun subed-record-accept-segment ()
  "Save the current timestamps for this segment and move to the next one."
  (interactive)
  (let ((end-time (subed-record-offset-ms)))
    (subed-set-subtitle-time-stop end-time)
		(subed-set-subtitle-comment
		 (concat
			(if (subed-subtitle-comment)
					(concat (string-trim (replace-regexp-in-string
									              "#\\+AUDIO: .*\\(\n\\|$\\)?" ""
									              (subed-subtitle-comment)))
									"\n")
				"")
			(format "#+AUDIO: %s" subed-record-filename)))
    (when (subed-forward-subtitle-text)
      (subed-set-subtitle-time-start end-time)
      (subed-set-subtitle-time-stop 0)
      (recenter))))

(defun subed-record-retry ()
  "Try recording this segment again."
  (interactive)
  (subed-set-subtitle-time-start (subed-record-offset-ms))
  (subed-set-subtitle-time-stop 0))

;;; Compiling

(defcustom subed-record-compile-caption-height nil "Number of pixels to leave at the bottom for captions in video.
If nil, do not leave space for captions."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-output-filename "output.webm" "Output filename."
  :type 'file :group 'subed-record)

;; NOTE BREAKING CHANGE: works with the whole list now
;; Replaces subed-record-compile-info-functions
(defcustom subed-record-compile-info-list-functions
	'(subed-record-compile-add-open-captions
		subed-record-compile-get-visuals-from-org
		subed-record-compile-get-audio-info
		subed-record-compile-get-output-file-from-org)
  "Functions to run to get information over all the segments.
Functions should accept one argument, the list of plists, and return the
modified list of plists."
  :type '(list function)
  :group 'subed-record)

(defvar subed-record-override-output-filename nil "If non-nil, use this as the output file instead.")

(defun subed-record-compile-get-output-file-from-org (list &optional context)
	(let ((filename
				 (or (plist-get context :output)
						 subed-record-override-output-filename
						 subed-record-compile-output-filename)))
		(mapcar (lambda (info)
							(if subed-record-override-output-filename
									(plist-put info :output filename)
								(let ((val (plist-get info :comment)))
									(when val
										(when (string-match "#\\+OUTPUT: \\(.+?\\)\\(\n\\|$\\)" val)
											(setq filename (expand-file-name (match-string 1 val)))
											(setq info (plist-put info :output filename))
											(setq val (replace-match "" nil t val))
											(setq info (plist-put info :comment val)))))
								(unless (plist-get info :output)
									(plist-put info :output filename)))
							info)
						list)))

;; there must be a better way to do this, but let's use this for now
(defun subed-record-parse-attributes (s)
	(read (concat "(" s ")")))

(defun subed-record-compile-add-open-captions (list &optional context)
  "Add open captions if specified."
	(let (open-captions)
		(when context
			(setq open-captions (plist-get context :open-captions)))
		(mapcar (lambda (info)
							(when (string-match "#\\+CLOSED_CAPTIONS" (or (plist-get info :comment) ""))
								(setq open-captions nil))
							(when (string-match "#\\+OPEN_CAPTIONS *\\(.*\\)" (or (plist-get info :comment) ""))
								(setq open-captions
											(if (match-string 1 (plist-get info :comment))
													(subed-record-parse-attributes (match-string 1 (plist-get info :comment)))
												t)))
							;; (when open-captions
							;; 	(plist-put info :description (plist-get info :caption)))
							(plist-put info :open-captions open-captions)
							info)
						list)))

(defun subed-record-compile-get-properties-from-comment (info properties)
	"Move info from comment into properties.
INFO should be a single cue."
	(let ((val (plist-get info :comment)))
		(when val
			(dolist (prop properties)
				(when (string-match (cadr prop) val)
					(setq info (plist-put info (car prop)
																(if (functionp (elt prop 2))
																		(funcall (elt prop 2) val)
																	(match-string 1 val))))
					(setq val (replace-match "" nil t val))
					(setq info (plist-put info :comment val)))))
		info))

(defun subed-record-compile-get-properties-from-comment-list (list properties)
	(mapcar (lambda (info)
						(subed-record-compile-get-properties-from-comment
						 info properties))
					list))

(defun subed-record-compile-set-first-property-from-context (result properties &optional context)
	(when result
		(dolist (prop properties)
			(when (and (null (plist-get (car result) (car prop)))
								 (plist-get context (car prop)))
				(setcar result (plist-put (car result) (car prop) (plist-get context (car prop)))))))
	result)

(defun subed-record-compile-get-visuals-from-org (list &optional context)
  "Get visual information from Org syntax and store it in INFO."
	(let* ((properties '((:description "#\\+CAPTION: \\(.+?\\)\\(\n\\|$\\)")
											 (:visual-file "\\[\\[\\(?:file:\\)?\\([^]]+?\\)\\].+\n?")
											 (:visual-start "#\\+VISUAL_START: \\(.+?\\) *\\(\n\\|$\\)")
											 (:visual-stop "#\\+VISUAL_START: \\(.+?\\) *\\(\n\\|$\\)")
											 (:options "#\\+OPTIONS: \\(.+?\\) *\\(\n\\|$\\)")))
				 (result (subed-record-compile-get-properties-from-comment-list list properties)))
		;; if nothing is specified for the first entry and context specifies things, copy those
		(subed-record-compile-set-first-property-from-context result properties context)
		result))

(defun subed-record-compile-copy-previous-property-if-nil (list properties)
	(reverse
	 (seq-reduce
		(lambda (prev val)
			(dolist (prop properties)
				(unless (plist-get val prop)
					(plist-put val prop (plist-get (car prev) prop))))
			(cons val prev))
		(cdr list)
		(list (car list)))))

(defun subed-record-compile-get-audio-info (list &optional context)
  "Get the audio file or current media file.
If CONTEXT is specified, copy those settings."
  (let* ((audio-file
					(if context
							(plist-get context :audio-file)
						(save-excursion (if (region-active-p) (goto-char (min (point) (mark)))
															(goto-char (point-min)))
														(or (subed-record-media-file) (subed-media-file)))))
				 (properties '((:audio-file "#\\+AUDIO: \\(.+?\\)\\(\n\\|$\\)")
											 (:pad-right "#\\+PAD_RIGHT: \\(.+?\\) *\\(\n\\|$\\)"
																	 (lambda (val)
																		 (floor
																			(* 1000
																				 (string-to-number (match-string 1 val))))))
											 (:pad-left "#\\+PAD_LEFT: \\(.+?\\) *\\(\n\\|$\\)"
																	(lambda (val)
																		(floor (* 1000
																							(string-to-number (match-string 1 val))))))))
				 (result (subed-record-compile-get-properties-from-comment-list list properties)))
		(setq result
					(subed-record-compile-set-first-property-from-context
					 result properties
					 (append context
									 (list :audio-file audio-file))))
		(setq result
					(subed-record-compile-copy-previous-property-if-nil
					 result
					 '(:audio-file)))
		result))

(defun subed-record-compile--process-selection (list &optional context)
	(seq-remove
	 (lambda (c)
		 (string-match "#\\+SKIP" (or (plist-get c :comment) "")))
	 (seq-reduce
		(lambda (val f)
			(funcall f val context))
		subed-record-compile-info-list-functions
		(mapcar (lambda (sub)
							(list :comment (and (elt sub 4) (substring-no-properties (elt sub 4)))
										:caption (substring-no-properties (elt sub 3))
										:start-ms (elt sub 1)
										:stop-ms (elt sub 2)))
						list))))

(defun subed-record-compile-get-base-selection (&optional beg end)
  "Return entries for each caption."
	;; Some directives apply to all succeeding subtitles,
	;; like #+OUTPUT, #+AUDIO, #+CLOSED_CAPTIONS, and #+OPEN_CAPTIONS.
	;; We need to process the previous ones as well.
	(setq beg (or beg (point-min)))
	(setq end (or end (point-max)))
	(save-restriction
		(widen)
		(let ((previous-captions (subed-record-compile--process-selection (subed-subtitle-list (point-min) (1- beg)))))
			;; copy :output, :audio-file
			(subed-record-compile--process-selection (subed-subtitle-list beg end) (car (last previous-captions))))))

(defun subed-record-compile--format-subtitles (list)
  "Make a temporary file containing the captions from LIST, set one after the other."
  (when list
    (let ((subtitle-file (make-temp-file "captions" nil ".vtt")))
			(subed-record-compile-subtitles subtitle-file list)
			subtitle-file)))

(defun subed-record-compile--format-tracks (list &optional include)
  "Prepare LIST for use in `compile-media.'"
	(setq include (or include '(text audio video subtitles)))
	(let ((text (and (member 'text include) (subed-record-compile--selection-descriptions list)))
				(video (and (member 'video include) (subed-record-compile--selection-visuals list)))
				(audio (and (member 'audio include) (subed-record-compile--selection-audio list))))
		(append
		 (when video (list (cons 'video video)))
		 (when audio (list (cons 'audio audio)))
		 (when text (list (cons 'text text))))))

(defun subed-record-compile-get-selection-for-region (beg end)
  "Return a `compile-media'-formatted set of tracks for the region."
  (interactive "r")
  (subed-record-compile--format-tracks (subed-record-compile-get-base-selection beg end)))

(defun subed-record-compile--selection-descriptions (selection)
	"Return selection segments containing descriptions or open captions."
	(let ((start-time 0))
		(seq-keep
		 (lambda (entry)
			 (prog1
					 (when (plist-get entry :description)
						 (list :start-ms start-time
									 :stop-ms (+ start-time (- (plist-get entry :stop-ms)
																						 (plist-get entry :start-ms)))
									 :text (plist-get entry :description)))
				 (setq start-time (+ start-time
														 (- (plist-get entry :stop-ms)
																(plist-get entry :start-ms))))))
		 selection)))

(defun subed-record-compile--selection-visuals (selection)
  "Return selection segments containing visuals with adjusted durations."
  (let (current result start stop)
    (while selection
      (setq start (plist-get (car selection) :start-ms))
      (setq stop (plist-get (car selection) :stop-ms))
      (when (plist-get (car selection) :visual-file)
        (setq current (copy-sequence (car selection)))
        (setq current (plist-put current :duration-ms 0))
				(when (and (plist-get (car selection) :comment)
									 (string-match "loop-if-shorter" (plist-get (car selection) :comment)))
					(setq current (plist-put current :loop-if-shorter t)))
  			(when (and (plist-get (car selection) :comment)
									 (string-match "same-edits" (or (plist-get (car selection) :options) "")))
					(setq current
								(plist-put
								 current :same-edits
								 (subed-record-compile--selection-audio selection))))
        (setq current
							(plist-put current :start-ms
												 (when (plist-get (car selection) :visual-start)
													 (subed-timestamp-to-msecs (plist-get (car selection) :visual-start)))))
        (setq current
							(plist-put current :stop-ms
												 (when (plist-get (car selection) :visual-stop)
													 (subed-timestamp-to-msecs (plist-get (car selection) :visual-stop)))))
        (setq result (cons (append (list :source
                                         (expand-file-name (plist-get (car selection) :visual-file)))
                                   current)
                           result)))
      (setf current (plist-put current :duration-ms (+ (or (plist-get current :duration-ms)
																													 (plist-get current :duration)
																													 0)
																											 (or (plist-get (car selection) :pad-left) 0)
																											 (or (plist-get (car selection) :pad-right) 0)
																											 (- (or stop 0) (or start 0)))))
      (setq selection (cdr selection)))
    (nreverse result)))

(defun subed-record-compile--selection-audio (list)
  "LIST is a plist of (:start-ms ... :stop-ms ... :audio-file ...).
Returns an audio track."
  (delq nil
        (seq-map (lambda (o)
                   (when (plist-get o :audio-file)
                     (append (list :source (expand-file-name (plist-get o :audio-file)))
                             o)))
                 list)))

;;;###autoload
(defun subed-record-compile-audio (&optional beg end &rest args)
  "Compile just the audio."
  (interactive)
  (apply 'subed-record-compile-video (append (list beg end '(audio)) args)))

;;;###autoload
(defun subed-record-compile-try-flow (&optional beg end make-video)
  "Try a segment to see if the audio flows well."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))
                     current-prefix-arg))
	(let ((subed-record-override-output-filename (make-temp-file "subed-record" nil ".opus")))
		(save-excursion
			(if make-video
					(subed-record-compile-video beg end nil t (lambda ()
																					(delete-file subed-record-override-output-filename)))
				(subed-record-compile-audio beg end t (lambda ()
																		(delete-file subed-record-override-output-filename)))))))

(defun subed-record-compile-group-by-output-file (list)
  "Return a list of ((output-filename sub sub sub) (output-filename sub sub sub))."
	(reverse (seq-group-by (lambda (o) (plist-get o :output)) list))
  ;; (let (result current)
  ;;   ;; default to subed-record-compile-output-filename if not specified
  ;;   (setq current
  ;;         (seq-take-while (lambda (o) (null (plist-get o :output))) list))
  ;;   (when current
  ;;     (setq result (list (cons (or subed-record-override-output-filename subed-record-compile-output-filename) current)))
  ;;     (setq list (seq-drop list (length current))))
  ;;   (while list
  ;;     (setq current (seq-take-while (lambda (o) (null (plist-get o :output))) (cdr list)))
  ;;     (setq result
  ;;           (cons
  ;;            (cons (plist-get (car list) :output)
  ;;                  (cons (car list)
  ;;                        current))
  ;;            result))
  ;;     (setq list (seq-drop list (1+ (length current)))))
  ;;   (reverse result))
	)

(defvar subed-record-sync t "Do it synchronously.")

;;;###autoload
(defun subed-record-compile-video (&optional beg end include play-afterwards after-func)
  "Create output file with video, audio, and subtitles.
INCLUDE should be a list of the form (video audio subtitles)."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))
                     '(video audio subtitles)))
  (setq include (or include '(video audio subtitles)))
  (let* ((selection
          (subed-record-compile-get-base-selection (or beg (point-min))
                                       (or end (point-max))))
         (output-groups (subed-record-compile-group-by-output-file selection))
				 (subed-record-sync (or (> (length output-groups) 1)
										subed-record-sync)))
    (mapc
     (lambda (output-group)
			 (when (and (member 'subtitles include)
									(not (plist-get (car (assoc-default 'subtitles selection)) :open-captions))
									(not (string=
												(buffer-file-name)
												(expand-file-name (concat (file-name-sans-extension (car output-group)) ".vtt")))))
				 (subed-record-compile-subtitles (concat (file-name-sans-extension (car output-group)) ".vtt")
																				 (cdr output-group)))
       (apply
        (if subed-record-sync #'compile-media-sync #'compile-media)
        (seq-filter
         (lambda (track) (member (car track) include))
         (subed-record-compile--format-tracks (cdr output-group)))
        (car output-group)
        (when (and play-afterwards (not subed-record-sync))
          (list
           :sentinel
           (lambda (_ event)
             (when (string-match "finished" event)
               (mpv-play (car output-group))
							 (when (functionp after-func)
								 (funcall after-func)))))))
			 (when (and subed-record-sync play-afterwards)
				 (mpv-play (car output-group))))
     output-groups)))

(defun subed-record-section ()
	"Return the start and end of the current section.
The current section is defined by #+OUTPUT commands."
	(let* ((start
					(save-excursion
						(if (string-match "#\\+OUTPUT:" (or (subed-subtitle-comment) ""))
								(progn (subed-jump-to-subtitle-comment) (point))
							;; keep going backwards
							(while (and (not (bobp))
													(if (subed-backward-subtitle-start-pos)
															(not (string-match "#\\+OUTPUT:" (or (subed-subtitle-comment) "")))
														(goto-char (point-min)))))
							(when (string-match "#\\+OUTPUT:" (or (subed-subtitle-comment) ""))
								(subed-jump-to-subtitle-comment)
								(point)))))
				 (end
					(save-excursion
						;; keep going backwards
						(while (and (not (eobp))
												(if (subed-forward-subtitle-start-pos)
														(not (string-match "#\\+OUTPUT:" (or (subed-subtitle-comment) "")))
													(goto-char (point-max)))))
						(when (string-match "#\\+OUTPUT:" (or (subed-subtitle-comment) ""))
							(subed-jump-to-subtitle-comment)
							(point)))))
		(when (and start end)
			(list start end))))

(defun subed-record-mark-section ()
	"Select the current section specified by #+OUTPUT:."
	(interactive)
	(when-let ((section (subed-record-section)))
		(setq deactivate-mark nil)
		(goto-char (car section))
		(set-mark (cadr section))))

(defun subed-record-compile-video-for-this-section ()
	"Find the current section that specifies #+OUTPUT:.
Compile the video for just that section."
	(interactive)
	(when-let ((section (subed-record-section)))
		(subed-record-compile-video (car section) (cadr section) nil t)))

(defun subed-record-compile-video-get-command (&optional beg end include play-afterwards after-func)
  "Copy the ffmpeg command."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))
                     '(video audio subtitles text)))
  (setq include (or include '(video audio subtitles text)))
  (let* ((selection
          (subed-record-compile-get-base-selection (or beg (point-min))
                                       (or end (point-max))))
				 result
         (output-groups (subed-record-compile-group-by-output-file selection)))
		(setq result
					(mapconcat
					 (lambda (output-group)
						 (compile-media-get-command
							(seq-filter
							 (lambda (track) (member (car track) include))
							 (subed-record-compile--format-tracks (cdr output-group)))
							(car output-group)))
					 output-groups
					 "\n"))
		(when (called-interactively-p 'any)
			(kill-new result))
		result))

;; todo: move this to compile-media?
(defun subed-record-compile-subtitles (filename &optional list)
	"Write subtitles to FILENAME.
Subtitles timestamps will be reset so one subtitle follows the
other, and directives will be removed."
	(interactive (list (read-file-name "Output file: ")))
	(let ((list (or list (subed-record-compile-get-base-selection))))
		(with-current-buffer (find-file-noselect filename)
			(erase-buffer)
			(subed-auto-insert)
			(let ((msecs 0)
						(subed-subtitle-spacing 0))
        (mapc (lambda (info)
								(let ((duration
											 (+ (or (plist-get info :pad-right) 0)
													(or (plist-get info :pad-left) 0)
													(- (plist-get info :stop-ms)
														 (plist-get info :start-ms)))))
									(subed-append-subtitle nil
																				 msecs
																				 (+ msecs (- duration 1))
																				 (plist-get info :caption)
																				 (let ((comment (string-trim
																												 (replace-regexp-in-string
																													"^#\\+.+" ""
																													(or (plist-get info :comment) "")))))
																					 (when (> (length comment) 0)
																						 comment)))
									(setq msecs (+ msecs duration))))
              list))
			(save-buffer)))
	filename)

(defun subed-record-compile-test-visuals (&optional limit)
  (interactive "p")
  (let* ((visuals
          (seq-filter (lambda (o) (plist-get o :visual-file))
                      (subed-record-compile-get-base-selection (point-min) (point-max))))
         (tracks
          (subed-record-compile--format-tracks
           (seq-map
            (lambda (o)
              (setf o (plist-put o :stop-ms (+ 1000 (plist-get o :start-ms))))
              o)
            (if (and (numberp limit) (> limit 1))
                (seq-take visuals limit)
              visuals)))))
    (compile-media tracks subed-record-compile-output-filename)))

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
                           (subed-vtt--msecs-to-timestamp ms)
                           (subed-vtt--msecs-to-timestamp (+ ms (- (plist-get o :stop-ms)
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
    (insert (subed-record-compile-format-as-audacity-labels
             (with-current-buffer (current-buffer)
               (unless (region-active-p)
                 (setq beg (point-min) end (point-max)))
               (subed-record-get-selection-for-region beg end))))))


(defun subed-record-media-file ()
	"Return the media file."
	(save-excursion
		(subed-jump-to-subtitle-end)
		(when (re-search-backward "#\\+AUDIO: \\(.+\\)" nil t)
			(match-string 1))))

(defun subed-record-set-up ()
	"Add #+AUDIO as the input and turn off time boundaries."
	(interactive)
	(remove-hook 'subed-mpv-file-loaded-hook #'subed-mpv-pause t)
	(remove-hook 'subed-mpv-file-loaded-hook #'subed-mpv-jump-to-current-subtitle t)
	(setq-local subed-enforce-time-boundaries nil)
	(add-hook 'subed-media-file-functions #'subed-record-media-file -100 t))

(defun subed-record-insert-audio-source-note (&optional prefix)
	"Add a comment setting #+AUDIO to the current media file.
Call with a prefix argument in order to set it to the MPV
#+AUDIO file."
	(interactive "p")
	(let ((comment (string-trim (or (subed-subtitle-comment) ""))))
		(subed-set-subtitle-comment
		 (concat comment
						 (if (string= comment "") "" "\n")
						 "#+AUDIO: "
						 (file-relative-name
							(if prefix
									(or (subed-record-media-file) (subed-media-file))
								subed-mpv-media-file))
						 "\n\n"))))

(defun subed-record-copy-assets-to-directory-and-rewrite (destination-dir)
	"Copy all the visual and audio assets to a specified directory."
	(interactive (list (file-relative-name
											(read-file-name "Destination: ")
											(file-name-directory (buffer-file-name)))))
	(subed-for-each-subtitle (point-min) (point-max) t
		(when-let ((comment (subed-subtitle-comment)))
			(when (string-match "#\\+AUDIO: \\(.*\\)\\(\n\\|$\\)" comment)
				(let ((filename (match-string 1 comment)))
					(unless (string= (expand-file-name filename)
													 (expand-file-name (file-name-nondirectory filename)
																						 destination-dir))
						(copy-file filename
											 (expand-file-name (file-name-nondirectory filename)
																				 destination-dir)
											 t)
						(setq comment
									(replace-regexp-in-string
									 (regexp-quote filename)
									 (file-name-concat destination-dir
																		 (file-name-nondirectory filename))
									 comment)))))
			(when (string-match "\\[\\[\\(?:file:\\)?\\(.*?\\)\\(\\]\\|\n\\|$\\)" comment)
				(let ((filename (match-string 1 comment)))
					(unless (string= (expand-file-name filename)
													 (expand-file-name (file-name-nondirectory filename)
																						 destination-dir))
						(copy-file filename
											 (expand-file-name (file-name-nondirectory filename)
																				 destination-dir)
											 t)
						(setq comment
									(replace-regexp-in-string
									 (regexp-quote filename)
									 (file-name-concat destination-dir
																		 (file-name-nondirectory filename))
									 comment)))))
			(subed-set-subtitle-comment comment))))

(defun subed-record-sum-time (&optional beg end)
	(interactive (list (and (region-active-p) (min (point) (mark)))
                     (and (region-active-p) (max (point) (mark)))))
	(let* ((selection (subed-record-compile-get-base-selection beg end))
				 (sum (apply '+
										 (mapcar
											(lambda (o)
												(+
												 (or (plist-get o :duration-ms) 0)
												 (or (plist-get o :duration) 0)
												 (or (plist-get o :pad-left) 0)
												 (or (plist-get o :pad-right) 0)
												 (- (or (plist-get o :stop-ms) 0)
														(or (plist-get o :start-ms) 0))))
											selection))))
		(when (called-interactively-p)
			(message "%s" (subed-msecs-to-timestamp sum)))
		sum))

;; (subed-record-parse-attributes ":bg \"&H66000000\" :font-name \"sachacHand\"")
(provide 'subed-record)

;;; subed-record.el ends here
