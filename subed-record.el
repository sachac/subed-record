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
;; Directives:
;;
;; #+OUTPUT:
;; #+AUDIO:
;; #+CLOSED_CAPTIONS
;; #+OPEN_CAPTIONS
;; #+SKIP
;; #+PAD_RIGHT: number of seconds
;; #+PAD_LEFT: number of seconds
;; #+TRIM: hh:mm:ss.MMM-hh:mm:ss.MMM,hh:mm:ss.MMM-hh:mm:ss.MMM
;; or #+TRIM: hh:mm:ss.MMM --> hh:mm:ss.MMM, hh:mm:ss.MMM --> hh:mm:ss.MMM

;; #+INTERLEAVE

;; For more information, see README.org.

(require 'compile-media)
(require 'subed)
(require 'subed-word-data)

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

(defvar-keymap subed-record-map
  "<up>" #'subed-backward-subtitle-text
  "<down>" #'subed-forward-subtitle-text
  "<left>" #'subed-record-start-segment
  "S-<left>" #'subed-record-copy-and-retry
  "c" #'subed-record-copy-and-retry
  "<right>" #'subed-record-accept-segment
  "q" #'subed-record-stop-recording
  "RET" #'subed-record-accept-and-stop)

(defcustom subed-record-offset-ms-from-end 100
  "Subtract this number of milliseconds from the end timestamp to account for keypresses."
  :type 'integer
  :group 'subed-record)

(defvar subed-record-filename nil)
(defvar subed-record-start-time nil "Emacs timestamp from when the recording was started.")
(defvar subed-record-process nil "Process for recording.")
(defvar subed-record-compile-ffmpeg-conversion-process nil "Process for compiling.")

;;; Recording

;;;###autoload
(defun subed-record ()
  "Start recording segments."
  (interactive)
  (unless (derived-mode-p 'subed-mode)
    (error "Not in `subed-mode' buffer."))
  (subed-record-set-up)
  (subed-record-start-recording))

(defvar subed-record-start-segment-hook nil
  "Functions to run when starting a segment.")

(defalias 'subed-record-retry #'subed-record-start-segment)
(defun subed-record-start-segment ()
  "Start a recording segment."
  (interactive)
  (subed-set-subtitle-time-start (subed-record-offset-ms))
  (subed-set-subtitle-time-stop 0)
  (recenter)
  (run-hooks 'subed-record-start-segment-hook))

(defun subed-record-start-recording (&optional filename)
  "Start recording. Save results to FILENAME."
  (interactive (list (when current-prefix-arg
                         (read-file-name "File: "))))
  (setq filename
        (or filename
            (concat (file-name-sans-extension (buffer-file-name))
								    "-"
								    (format-time-string "%Y-%m-%d-%H%M%S")
								    subed-record-extension)))
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
     (subed-record-sox-start filename)))
  (message "Recording...")
  (set-transient-map
   subed-record-map
   (lambda ()
     (and (not (memq this-command '(subed-record-stop-recording
                                    subed-record-accept-and-stop)))
          (lookup-key subed-record-map (this-command-keys)))))
  (subed-record-start-segment))

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

(defvar subed-record-finished-hook nil "Functions to run after stopping the recording.")
(defun subed-record-stop-recording (&optional time)
  "Finish recording."
  (interactive)
  (when (process-live-p subed-record-process) (quit-process subed-record-process))
  (setq subed-record-start-time nil)
	(pcase subed-record-backend
		('obs-old (obs-websocket-send "StopRecording"))
		('obs (obs-websocket-send "StopRecord" #'subed-record-get-filename-from-obs-stop-record-callback))))

(defun subed-record-is-recording-p ()
  "Return non-nil if we are currently recording."
  subed-record-start-time)

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
          (make-process
           :name "subed-record"
           :buffer (current-buffer)
           :command
           (append (list subed-record-ffmpeg-executable)
                   subed-record-ffmpeg-args
                   (list (expand-file-name filename))
                   nil)
           :sentinel
           (lambda (process status)
             (when (string-match "exited" status)
               (run-hook-with-args 'subed-record-finished-hook filename)))))))

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

(defvar subed-record-accept-segment-hook nil "Functions to run after accepting a segment.")
(defvar subed-record-moved-forward-hook nil "Functions to run after accepting a segment and moving forward.
Point is in the new segment.")
(defun subed-record-accept-segment (&optional stay-put)
  "Save the current timestamps for this segment and move to the next one."
  (interactive)
  (let ((end-time (subed-record-offset-ms)))
    (subed-set-subtitle-time-stop (- end-time subed-record-offset-ms-from-end))
		(subed-set-subtitle-comment
		 (concat
			(if (subed-subtitle-comment)
					(concat (string-trim (replace-regexp-in-string
									              "#\\+AUDIO: .*\\(\n\\|$\\)?" ""
									              (subed-subtitle-comment)))
									"\n")
				"")
			(format "#+AUDIO: %s" subed-record-filename)))
    (unless stay-put
      (run-hooks 'subed-record-accept-segment-hook)
      (when (subed-forward-subtitle-text)
        (run-hooks subed-record-moved-forward-hook)
        (subed-record-start-segment)))))

(defun subed-record-copy-and-retry ()
  "Duplicate this segment and start a new take."
  (interactive)
  (subed-record-accept-segment t)
  (subed-append-subtitle nil nil nil (subed-subtitle-text))
  (subed-record-start-segment))

(defun subed-record-accept-and-stop ()
  "Accept this segment and stop recording."
  (interactive)
  (subed-record-accept-segment t)
  (subed-record-stop-recording))

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
		subed-record-compile-get-output-file-from-org
    subed-record-compile-get-interleaved-file-from-org)
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

(defun subed-record-compile-get-interleaved-file-from-org (list &optional context)
  "Interleave a file between each segment."
	(let ((filename
				 (plist-get context :interleaved)))
		(mapcar (lambda (info)
							(let ((val (plist-get info :comment)))
								(if val
									  (if (string-match "#\\+INTERLEAVE: \\(.+?\\)\\(\n\\|$\\)" val)
                        (if (string= (match-string 1 val) "none")
                            (setq filename nil)
										      (setq filename (expand-file-name (match-string 1 val)))
										      (setq info (plist-put info :interleaved filename))
										      (setq val (replace-match "" nil t val))
										      (setq info (plist-put info :comment val)))))
                (unless (plist-get info :interleaved)
                  (plist-put info :interleaved filename)))
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

(defun subed-record-get-trim-info (value)
  "Parse trim information out of VALUE.
Return a list of ((start-msecs stop-msecs) (start-msecs stop-msecs) ...).
You can get the value with `subed-record-get-directive'."
  (mapcar
   (lambda (o)
     (mapcar #'subed-timestamp-to-msecs
             (split-string
              o
              (if (string-match " *--> *" o) " *--> *" "-"))))
   (split-string value ", *")))

(declare-function subed-waveform--update-overlay-svg "subed-waveform")

(defun subed-record-show-trims-in-waveform (overlay)
  "Show trimmed-away areas as translucent gray rectangles in OVERLAY.
Intended for use in `subed-waveform-make-overlay-hook'.
Read the #+TRIM directive from the current subtitle comment and
shade the portions of the waveform not covered by any trim interval."
  (let* ((comment (subed-subtitle-comment))
         (trim-value (subed-record-get-directive "#+TRIM" comment))
         (waveform-start (overlay-get overlay 'waveform-start))
         (waveform-stop (overlay-get overlay 'waveform-stop))
         (trim-intervals (if (subed-record-get-directive "#+SKIP" comment)
                             (list (list waveform-start waveform-stop))
                           (and trim-value (subed-record-get-trim-info trim-value))))
         (svg (get-text-property 0 'svg (overlay-get overlay 'before-string))))
    (when (and trim-intervals svg waveform-start waveform-stop)
          (let ((pos waveform-start)
                (total (float (- waveform-stop waveform-start)))
                (sorted (sort (copy-sequence trim-intervals)
                              (lambda (a b) (< (car a) (car b))))))
            (dolist (interval sorted)
              (let ((gap-start (max (car interval) waveform-start))
                    (gap-end (min (cadr interval) waveform-stop)))
                (svg-rectangle svg
                               (format "%.2f%%" (* 100.0 (/ (- gap-start waveform-start) total)))
                               "0%"
                               (format "%.2f%%" (* 100.0 (/ (- gap-end gap-start) total)))
                               "100%"
                               :fill "lightgray" :fill-opacity "0.3"
                               :class "trimmed"))
              (setq pos (max pos (cadr interval))))))))

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
                       (:trim "#\\+TRIM: \\(.+?\\) *\\(\n\\|$\\)"
                              (lambda (val)
                                (subed-record-get-trim-info (match-string 1 val))))
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
				(audio (and (member 'audio include) (subed-record-compile--selection-audio list)))
        (subtitles (and (member 'subtitles include) (subed-record-adjust-subtitle-list list))))
		(append
		 (when video (list (cons 'video video)))
		 (when audio (list (cons 'audio audio)))
		 (when text (list (cons 'text text)))
     (when subtitles (list (cons 'subtitles (list :subtitles subtitles)))))))

(defun subed-record-compile-get-selection-for-region (beg end)
  "Return a `compile-media'-formatted set of tracks for the region."
  (interactive "r")
  (subed-record-compile--format-tracks (subed-record-compile-get-base-selection beg end)))

(defun subed-record-compile--interleave (list)
  "Process interleaving instructions in LIST.
Returns a new list with the :interleaved file inserted after each segment
except the last one."
  (if (or (null list)
          (= (length list) 1))
      list
    ;; Build the interleaved list
    (let (result
          (durations (make-hash-table)))
      (dotimes (i (length list))
        (let ((segment (nth i list))
              (next-segment (nth (1+ i) list)))
          (push segment result)
          (when (and
                 next-segment
                 (plist-get segment :interleaved)
                 (string= (plist-get segment :interleaved)
                          (plist-get next-segment :interleaved)))
            (unless (gethash (plist-get segment :interleaved)
                             durations)
              (puthash (plist-get segment :interleaved)
                       (compile-media-get-file-duration-ms (plist-get segment :interleaved))
                       durations))
            (push (list
                   :source (plist-get segment :interleaved)
                   :visual-file (unless (member (file-name-extension
                                                 (plist-get segment :interleaved))
                                                subed-audio-extensions)
                                  (plist-get segment :interleaved))
                   :audio-file (plist-get segment :interleaved)
                   :start-ms 0
                   :stop-ms (gethash (plist-get segment :interleaved)
                                     durations)
                   :interleaved-segment t)
                  result))))
      (nreverse result))))

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
        (when (and (plist-get (car selection) :options)
									 (string-match "change-rate \\([\\.0-9]?\\)"
                                 (or (plist-get (car selection) :options) "")))
          (plist-put current :change-rate
                     (string-to-number (match-string 1 (plist-get (car selection) :options)))))
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

(defun subed-record-play-output (&optional beg end)
  "Play the output file(s)."
  (interactive (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min) (point-max))))
  (let ((output-files
         (mapcar
          (lambda (o)
            (expand-file-name (car o)))
          (subed-record-compile-group-by-output-file (subed-record-compile-get-base-selection
                                          (or beg (point-min))
                                          (or end (point-max)))))))
    (make-process :name "subed-record"
                  :command (cons "mpv"
                                 output-files))))

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
       (apply
        (if subed-record-sync #'compile-media-sync #'compile-media)
        (seq-filter
         (lambda (track) (member (car track) include))
         (subed-record-compile--format-tracks (subed-record-compile--interleave (cdr output-group))))
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

(defun subed-record-compile-subtitle-list (subtitles output-file &optional include context)
  "Compile SUBTITLES into OUTPUT-FILE.
INCLUDE should be a list of the form (video audio subtitles).
CONTEXT can be a plist that sets up the context (ex: :interleave)."
  (setq include (or include '(video audio subtitles)))
  (let* ((subed-record-override-output-filename output-file)
         (selection (subed-record-compile--process-selection
                     subtitles
                     (if (stringp context)
                         (list nil 0 0 context)
                       context)))
         (output-groups (subed-record-compile-group-by-output-file selection)))
    (mapc
     (lambda (output-group)
       (funcall
        #'compile-media-sync
        (seq-filter
         (lambda (track) (member (car track) include))
         (subed-record-compile--format-tracks (subed-record-compile--interleave (cdr output-group))))
        (car output-group)))
     output-groups)
    output-groups))

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
(defun subed-record-adjust-subtitle-list (&optional list)
  "Prepare subtitles for inclusion.
Subtitle timestamps will be reset so one subtitle follows the
other, and directives will be removed."
  (setq list (or list (subed-record-compile-get-base-selection)))
  (setq list (subed-record-compile--interleave list))
  (let ((msecs 0)
        (subed-subtitle-spacing 0))
    (mapcar
     (lambda (info)
       (let ((duration
              (+ (or (plist-get info :pad-right) 0)
                 (or (plist-get info :pad-left) 0)
                 (- (plist-get info :stop-ms)
                    (plist-get info :start-ms)))))
         (prog1
             (list
              nil
              msecs
              (+ msecs (- duration 1))
              (plist-get info :caption)
              (let ((comment (string-trim
                              (replace-regexp-in-string
															 "^#\\+.+" ""
															 (or (plist-get info :comment) "")))))
                (when (> (length comment) 0)
                  comment)))
           (setq msecs (+ msecs duration)))))
     list)))

(defun subed-record-compile-subtitles (filename &optional list)
	"Write subtitles to FILENAME.
Subtitles timestamps will be reset so one subtitle follows the
other, and directives will be removed."
	(interactive (list (read-file-name "Output file: ")))
  (setq list (or list (subed-record-compile-get-base-selection)))
	(subed-create-file filename (subed-record-adjust-subtitle-list list) t)
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

(defun subed-record-ensure-same-file ()
  (unless (string= (file-truename (subed-media-file))
                   subed-mpv-media-file)
    (subed-mpv-play-from-file (subed-media-file))))

;;;###autoload
(defun subed-record-set-up ()
	"Add #+AUDIO as the input and turn off time boundaries."
	(interactive)
	(remove-hook 'subed-mpv-file-loaded-hook #'subed-mpv-pause t)
	(remove-hook 'subed-mpv-file-loaded-hook #'subed-mpv-jump-to-current-subtitle t)
	(add-hook 'subed-section-comments-as-chapters-functions #'subed-record-remove-directives t)
	(setq-local subed-enforce-time-boundaries nil)
	(add-hook 'subed-media-file-functions #'subed-record-media-file -100 t)
  (add-hook 'subed-mpv-before-jump-hook #'subed-record-ensure-same-file nil t))

(with-eval-after-load 'subed-waveform
  (define-key subed-waveform-svg-map [drag-mouse-1] #'subed-record-waveform-trim)
	(add-hook 'subed-waveform-make-overlay-hook #'subed-record-show-trims-in-waveform))


(defun subed-record-insert-audio-source-note (&optional beg end prefix)
	"Add a comment setting #+AUDIO to the current media file.
Call with a prefix argument in order to set it to the MPV
#+AUDIO file."
	(interactive
   (if (region-active-p)
       (list (region-beginning)
             (region-end)
             current-prefix-arg)
     (list nil nil current-prefix-arg)))
  (let ((file (expand-file-name
               (if prefix
			             (or (subed-record-media-file) (subed-media-file))
		             subed-mpv-media-file))))
    (if (and beg end)
        (subed-for-each-subtitle beg end t
          (subed-record-set-directive "#+AUDIO" file))
      (subed-record-set-directive "#+AUDIO" file))))

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
			(message "%s" (subed-msecs-to-timestamp sum))
      (kill-new (subed-msecs-to-timestamp sum)))
		sum))

(defun subed-record-get-directive (prop &optional from-comment)
  "Return the value of PROP in the comments.
PROP should be a string like \"#+REFERENCE\"."
  (when (and from-comment (listp from-comment))
    (setq from-comment (elt from-comment 4)))
  (let ((comment (or from-comment (subed-subtitle-comment) "")))
    (when (string-match (concat (regexp-quote prop) "\\(?:\\(?:: \\)?\\(.*\\)\\)?\\(\n\\|$\\)") comment)
      (match-string 1 comment))))

(defun subed-record-set-directive (prop new-value &optional from-comment)
  "Set PROP to NEW-VALUE, or remove it if nil.
PROP should be a string like \"#+REFERENCE\"."
  (interactive (list (read-string "Directive: ") (read-string "Value: ")))
  (let ((comment (or from-comment (subed-subtitle-comment) "")))
    (cond
     ((string-match (concat (regexp-quote prop) "\\(: *.*\\)?\\(\n\\|$\\)") comment)
      (setq comment (if new-value
                        (replace-match (concat ": " new-value) t t comment 1)
                      (replace-match "" nil nil comment))))
     (new-value
      (setq comment (string-trim (concat (string-trim comment) "\n" prop ": " new-value)))))
    (unless from-comment
      (subed-set-subtitle-comment comment))
    comment))

(defun subed-record-remove-directives (subtitles)
	"Remove directives from SUBTITLES.
Works destructively.
If SUBTITLES is a string with the comments for a subtitle,
remove directives from that string."
	(if (stringp subtitles)
			(replace-regexp-in-string "^#.?+\\(\n\\|$\\)" "" subtitles)
		(seq-map
		 (lambda (cue)
			 (when (elt cue 4)
				 (setf (elt cue 4) (replace-regexp-in-string "^#.?+\\(\n\\|$\\)" "" (elt cue 4)))
				 (when (string= (string-trim (elt cue 4)) "")
					 (setf (elt cue 4) nil)))
			 cue)
		 subtitles)))

(defun subed-record-toggle-skip (&optional beg end)
  "Toggle the skip status of the current subtitle.
If a region is active, toggle the skip status of the subtitles in the region."
  (interactive (when (region-active-p)
                 (list (region-beginning)
                       (region-end))))
  (let ((do-skip (not (string-match "#\\+SKIP" (save-excursion (when beg (goto-char beg)) (or (subed-subtitle-comment) ""))))))
    (save-excursion
      (if beg
          (subed-for-each-subtitle beg end t
            (let ((new-comment
                   (string-trim (concat (string-trim (replace-regexp-in-string "^#\\+SKIP\n?" "" (or (subed-subtitle-comment) "")))
                                        (if do-skip "\n#+SKIP" "")))))
              (subed-set-subtitle-comment
               (unless (string= new-comment "")
                 new-comment))))
        (let ((new-comment
               (string-trim (concat (string-trim (replace-regexp-in-string "^#\\+SKIP\n?" "" (or (subed-subtitle-comment) "")))
                                    (if do-skip "\n#+SKIP" "")))))
          (subed-set-subtitle-comment
           (unless (string= new-comment "")
             new-comment)))))))

;;;###autoload
(defun subed-record-filter-skips (subtitles)
  "Return SUBTITLES without the skipped ones."
  (seq-remove (lambda (o)
                (subed-record-get-directive "#+SKIP" (or (elt o 4) "")))
              subtitles))

(defun subed-record-filter-for-directive (directive subtitles &optional match)
  "Return SUBTITLES that have DIRECTIVE.
If MATCH is specified, the directive value should match the regular
expression."
  (seq-filter
   (lambda (o)
     (if match
         (string-match match (or (subed-record-get-directive directive (or (elt o 4) "")) ""))
       (subed-record-get-directive directive (or (elt o 4) ""))))
   subtitles))

;; Probably should hook this into subed-word-data-normalizing-functions instead someday

;;;###autoload
(defun subed-record-simplify (s)
  "Prepare text for synthesis or comparison.
Remove speaker tags and some punctuation."
  (string-trim
   (replace-regexp-in-string
    "^\\[[^]]+\\]: "
    ""
    (replace-regexp-in-string
     "  +" " "
     (replace-regexp-in-string
      "^- \\(\\.\\.\\. \\)?\\|^\\*+ \\|{.+?}\\|*" ""
      s)))))

(defun subed-record-group-by-with-test (key-fn seq &optional test-fn)
  "Group SEQ by KEY-FN, using TEST-FN to compare keys.
This uses `identity' and `equal' by default."
  (let (groups)
    (setq key-fn (or key-fn 'identity))
    (setq test-fn (or test-fn 'equal))
    (mapc
     (lambda (item)
       (let* ((key (funcall key-fn item))
              (group (assoc key groups test-fn)))
         (if group
             (setcdr group (cons item (cdr group)))
           (push (list key item) groups))))
     seq)
    (mapcar (lambda (group)
              (cons (car group) (nreverse (cdr group))))
            (nreverse groups))))

(defun subed-record-keep-last (subtitles &optional simplify-fn compare-fn)
  "Return only the last instance of each item in SUBTITLES.
Simplify text and use approximate matches by default,
or use SIMPLIFY-FN and/or COMPARE-FN if specified.

Use `subed-record-filter-skips' before this step to focus on subtitles
you want to keep."
  (mapcar
   (lambda (group) (car (last group)))
   (subed-record-group-by-with-test
    (lambda (o) (funcall (or simplify-fn 'subed-record-simplify) (elt o 3)))
    subtitles
    (or compare-fn
        #'subed-word-data-compare-normalized-string-distance))))

(defun subed-record-format-approximate-matches-as-subtitles (results)
  "Reorganize RESULTS into the format expected by subed."
  (mapcar
   (lambda (o)
     (list nil
           (alist-get 'start o)
           (alist-get 'end o)
           (or (alist-get 'target o) (allist-get 'text))
           (when (alist-get 'window-string o) (format "#+ACTUAL: %s" (alist-get 'window-string o)))))
   results))

;;;###autoload
(defun subed-record-extract-all-approximately-matching-phrases (phrase-list
                                                    word-data-file
                                                    &optional
                                                    output-file
                                                    fuzz-before fuzz-after)
  "Fuzzy-match PHRASE-LIST against WORD-DATA-FILE and return a list of subtitles."
  (interactive (list (split-string
                      (if (region-active-p)
                          (buffer-substring
                           (region-beginning)
                           (region-end))
                        (read-string "Phrases (one per line): "))
                      "\n" t)
                     (read-file-name "Word timing data: ")
                     (read-file-name "Output file: ")))
  (setq fuzz-before (or fuzz-before -3))
  (setq fuzz-after (or fuzz-after 5))
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (all-words (if (listp word-data-file) word-data-file (subed-word-data-parse-file word-data-file)))
         (all-words-len (length all-words))
         results
         subtitles)
    (dolist (phrase phrase-list)
      (let* ((target-words (string-trim phrase))
             (target-string-length (length target-words))
             (target-word-count (length (split-string target-words " "))))
        (dotimes (i (length all-words))
          (let ((best-score 1.0)
                best-window)
            (cl-loop
             for flex from fuzz-before to (min (- all-words-len i) fuzz-after)
             do
             (let ((current-len (+ target-word-count flex)))
               (when (> current-len 0)
                 (let* ((window (seq-subseq all-words i (min all-words-len (+ i current-len))))
                        (window-string (string-join (mapcar (lambda (o) (alist-get 'text o)) window) " "))
                        (window-score (/ (string-distance window-string target-words)
                                         (* 1.0 (max (length window-string)
                                                     target-string-length)))))
                   (when (and (< window-score subed-word-data-compare-normalized-string-distance-threshold)
                              (< window-score best-score))
                     (setq best-score window-score)
                     (setq best-window `((start . ,(floor (alist-get 'start (car window))))
                                         (end . ,(floor (alist-get 'end (car (last window)))))
                                         (window . ,window)
                                         (score . ,window-score)
                                         (window-string . ,window-string)
                                         (target . ,target-words))))))))
            (when best-window
              ;; Possibly overlapping, compare scores
              (if (and (eq (alist-get 'end (car results))
                           (alist-get 'end best-window)))
                  (when (< (alist-get 'score best-window)
                           (alist-get 'score (car results)))
                    (setf (car results) best-window))
                (push best-window results)))))))
    (setq results (nreverse results))
    (setq subtitles (subed-record-format-approximate-matches-as-subtitles results))
    (let ((media-file (subed-guess-media-file nil word-data-file)))
      (mapc (lambda (o)
              (setf (elt o 4)
                    (subed-record-set-directive "#+AUDIO" media-file (or (elt o 4) ""))))
            subtitles))
    (cond
     ((stringp output-file)
      (subed-create-file output-file subtitles))
     ((eq output-file t) subtitles)
     (t results))))

(defun subed-record-extract-last-phrases (phrase-list
                              word-data-file
                              output-file
                              &optional fuzz-before fuzz-after)
  "Fuzzy-match PHRASE-LIST against WORD-DATA-FILE and return a list of subtitles..
This implements a sliding window search similar to the Python phrase-search.py,
using normalization and similarity functions from subed-word-data.el."
  (interactive (list (split-string
                      (if (region-active-p)
                          (buffer-substring
                           (region-beginning)
                           (region-end))
                        (read-string "Phrases (one per line): "))
                      "\n" t)
                     (read-file-name "Word timing data: ")
                     (read-file-name "Output file: ")))
  (let* ((data
          (subed-record-extract-all-approximately-matching-phrases
           phrase-list word-data-file nil fuzz-before fuzz-after))
         (grouped (seq-group-by (lambda (o) (alist-get 'target o))
                                data))
         (last (mapcar (lambda (o) (car (last (cdr o))))
                       grouped))
         (subtitles (subed-record-format-approximate-matches-as-subtitles last)))
    (cond
     ((stringp output-file)
      (subed-create-file output-file subtitles))
     ((eq output-file t) subtitles)
     (t last))))

(defun subed-record-extract-words (word word-data-file output-file)
  "Extract words matching WORD from WORD-DATA-FILE and write to OUTPUT-FILE."
  (let ((media-file (subed-guess-media-file nil word-data-file)))
    (subed-create-file
     output-file
     (mapcar (lambda (o)
               (list nil
                     (alist-get 'start o)
                     (alist-get 'end o)
                     (alist-get 'text o)
                     (string-join
                      (delq nil
                            (list
                             (and media-file (format "#+AUDIO: %s" media-file))
                             (and (alist-get 'score o)
                                  (format "#+WHISPER_SCORE: %d" (* (alist-get 'score o) 100)))
                             (and (alist-get 'speaker o)
                                  (format "#+SPEAKER: %s" (alist-get 'speaker o)))))
                      "\n")))
             (sort
              (seq-filter
               (lambda (o)
                 (subed-word-data-compare-normalized-string-distance
                  word
                  (alist-get 'text o)))
               (subed-word-data-parse-file
                word-data-file))
              :key (lambda (o) (alist-get 'score o))
              :reverse t))
     t)))


(defvar subed-record-extract-audio-args '("-ac" "1")
  "Extra arguments to pass to ffmpeg.")
(defvar subed-record-extract-audio-rate 16000
  "Audio rate for clips.")

(defun subed-record-extract-audio-for-current-subtitle-to-file (output-file &optional sub)
  "Save the audio for the current subtitle to OUTPUT-FILE."
  (interactive (list (read-file-name "Output file: ")))
    (if sub
        (with-temp-buffer
          (subed-vtt-mode)
          (insert "WEBVTT\n\n")
          (subed-append-subtitle-list (list sub))
          (subed-record-extract-audio-for-current-subtitle-to-file output-file))
      (let* ((subed-record-override-output-filename (expand-file-name output-file))
             (compile-media-ffmpeg-audio-rate subed-record-extract-audio-rate)
             (compile-media-ffmpeg-arguments subed-record-extract-audio-args)
             (sub-beg (subed-subtitle-start-pos))
             (sub-end (save-excursion
                        (subed-jump-to-subtitle-end)
                        (point))))
        (subed-record-compile-audio sub-beg sub-end))))

(defun subed-record-insert-file (input-file)
  "Add INPUT-FILE at point."
  (interactive (list (read-file-name "Media file: ")))
  (let* ((caption-file (concat (file-name-sans-extension input-file)
                               ".vtt"))
         (captions (and (file-exists-p caption-file)
                        (subed-parse-file caption-file))))
    (if captions
        (save-restriction
          (narrow-to-region (point) (point))
          (subed-append-subtitle-list captions)
          (goto-char (point-min))
          (subed-record-set-directive "#+AUDIO" (expand-file-name input-file)))
      (subed-append-subtitle
       nil 0
       (compile-media-get-file-duration-ms (expand-file-name input-file))
       "")
      (subed-record-set-directive "#+AUDIO" (expand-file-name input-file)))))

(defun subed-record-sort-by-directive (directive &optional beg end)
  "Sort subtitles by DIRECTIVE.
DIRECTIVE could be a string like \"SCORE\" or a function that takes a and b.
BEG could be a list of subtitles."
  (interactive (list (read-string "Directive (ex: SCORE): ")
                     (if (region-active-p) (region-beginning) (point-min))
                     (if (region-active-p) (region-end) (point-max))))
  (let* ((list (if (listp beg) beg (subed-subtitle-list beg end)))
         (sorted
          (cond
           ((stringp directive)
            (sort list
                  :key
                  (lambda (o)
                    (or (subed-record-get-directive
                         (if (string-match "^#" directive)
                             directive
                           (concat "#+" directive))
                         (elt o 4))
                        ""))))
           ((functionp directive)
            (sort list :lessp directive)))))
    (if (listp beg)
        sorted
      (delete-region beg end)
      (subed-append-subtitle-list sorted))))

;;;###autoload
(defun subed-record-waveform-trim (event)
  "Set the trim directive to remove the times indicated."
  (interactive "e")
	(subed-waveform--with-event-subtitle event
    (let ((obj (car (elt (cadr event) 4))))
			(when (get-text-property 0 'waveform-pixels-per-second obj)
				(let* ((x1 (car (elt (elt event 2) 2)))
							 (x2 (car (elt (elt event 1) 2)))
							 (msecs
								(floor (* 1000 (/ (- x1 x2) ; pixels moved
                                  (get-text-property 0 'waveform-pixels-per-second obj)))))) ; don't save this change
					(subed-record-set-directive "#+TRIM"
													(format "%s --> %s"
																	(subed-msecs-to-timestamp
																	 (subed-waveform--mouse-event-to-ms event (min x1 x2)))
																	(subed-msecs-to-timestamp
																	 (subed-waveform--mouse-event-to-ms event (max x1 x2)))))
					(subed-waveform-refresh-current-subtitle))))))

(provide 'subed-record)

;;; subed-record.el ends here
