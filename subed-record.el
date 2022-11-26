;;; subed-record.el --- Record audio in segments and compile it into a file  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Sacha Chua

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

(defcustom subed-record-compile-caption-height nil "Number of pixels to leave at the bottom for captions in video.
If nil, do not leave space for captions."
  :type 'integer :group 'subed-record)
(defcustom subed-record-compile-output-filename "output.webm" "Output filename."
  :type 'file :group 'subed-record)


(defcustom subed-record-compile-info-functions '(subed-record-compile-get-visuals-from-org
                                                 subed-record-compile-get-audio-info
                                                 subed-record-compile-get-output-file-from-org)
  "Functions to run to get information for the current segment.
Functions should accept one argument, the plist, and return the
modified plist."
  :type '(list function)
  :group 'subed-record)

(defun subed-record-compile-get-output-file-from-org (info)
  (mapc (lambda (field)
          (let ((val (plist-get info field)))
            (when val
              (when (string-match "#\\+OUTPUT: \\(.+?\\)\n" val)
                (setq info (plist-put info :output (match-string 1 val)))
                (setq val (replace-match "" nil t val))
                (setq info (plist-put info field val))))))
        '(:comment :caption))
  info)

(defun subed-record-compile-get-visuals-from-org (info)
  "Get visual information from Org syntax and store it in INFO."
  (mapc (lambda (field)
          (let ((val (plist-get info field)))
            (when val
              (when (string-match "#\\+CAPTION: \\(.+?\\)\n" val)
                (setq info (plist-put info :visual-description (match-string 1 val)))
                (setq val (replace-match "" nil t val)))
              (when (string-match "\\[\\[file:\\([^]]+?\\)\\].+\n" val)
                (setq info (plist-put info :visual-file (match-string 1 val)))
                (setq val (replace-match "" nil t val)))
              (setq info (plist-put info field val)))))
        '(:comment :caption))
  info)

(defun subed-record-compile-get-audio-info (info)
  "Get the audio file or current media file."
  (let (audio-file)
    (mapc (lambda (field)
            (let ((val (plist-get info field)))
              (when val
                (if (string-match "#\\+AUDIO: \\(.+?\\)\n" val)
                    (progn
                      (setq audio-file (match-string 1 val))
                      (setq val (replace-match "" nil t val)))
                  (setq audio-file subed-mpv-media-file))
                (setq info (plist-put info field val)))))
          '(:comment :caption))
    (plist-put info :audio-file audio-file)
    info))

(defun subed-record-compile-get-base-selection (beg end)
  "Return entries for each caption."
  (mapcar (lambda (sub)
            (seq-reduce
             (lambda (val f) (funcall f val))
             subed-record-compile-info-functions
             (list :comment (elt sub 4)
                   :caption (elt sub 3)
                   :start-ms (elt sub 1)
                   :stop-ms (elt sub 2))))
          (subed-subtitle-list beg end)))

(defun subed-record-compile--format-subtitles (list)
  "Make a temporary file containing the captions from LIST, set one after the other."
  (when list
    (let ((subtitle-file (concat (make-temp-name "/tmp/captions") ".vtt")))
      (with-temp-file subtitle-file
        (subed-vtt-mode)
        (let ((msecs 0))
          (mapc (lambda (info)
                  (subed-append-subtitle nil
                                         msecs
                                         (+ msecs (- (plist-get info :stop-ms)
                                                     (plist-get info :start-ms)
                                                     1))
                                         (plist-get info :caption))
                  (setq msecs (+ msecs (- (plist-get info :stop-ms)
                                          (plist-get info :start-ms)))))
                list)))
      subtitle-file)))

(defun subed-record-compile--format-tracks (list)
  "Prepare LIST for use in `compile-media.'"
  `((video ,@(subed-record-compile--selection-visuals list))
    (audio ,@(subed-record-compile--selection-audio list))
    (subtitles (:source ,(subed-record-compile--format-subtitles list) :temporary t))))

(defun subed-record-compile-get-selection-for-region (beg end)
  "Return a `compile-media'-formatted set of tracks for the region."
  (interactive "r")
  (subed-record-compile--format-tracks (subed-record-compile-get-base-selection beg end)))

(defun subed-record-compile--selection-visuals (selection)
  "Return selection segments containing visuals with adjusted durations."
  (let (current result start stop)
    (while selection
      (setq start (plist-get (car selection) :start-ms))
      (setq stop (plist-get (car selection) :stop-ms))
      (when (plist-get (car selection) :visual-file)
        (setq current (copy-sequence (car selection)))
        (setq current (plist-put current :duration 0))
        (setq current (plist-put current :start-ms nil))
        (setq current (plist-put current :stop-ms nil))
        (setq result (cons (append (list :source
                                         (expand-file-name (plist-get (car selection) :visual-file)))
                                   current)
                           result)))
      (setf current (plist-put current :duration (+ (or (plist-get current :duration) 0)
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
     t)))

(defun subed-record-compile-group-by-output-file (list)
  "Return a list of ((output-filename sub sub sub) (output-filename sub sub sub))."
  (let (result current)
    ;; default to subed-record-compile-output-filename if not specified
    (setq current
          (seq-take-while (lambda (o) (null (plist-get o :output))) list))
    (when current
      (setq result (list (cons subed-record-compile-output-filename current)))
      (setq list (seq-drop list (length current))))
    (while list
      (setq current (seq-take-while (lambda (o) (null (plist-get o :output))) (cdr list)))
      (setq result
            (cons
             (cons (plist-get (car list) :output)
                   (cons (car list)
                         current))
             result))
      (setq list (seq-drop list (1+ (length current)))))
    (reverse result)))

(defun subed-record-compile-video (&optional beg end include &rest play-afterwards)
  "Create output file with video, audio, and subtitles.
INCLUDE should be a list of the form (video audio subtitles)."
  (interactive (list (if (region-active-p) (min (point) (mark)) (point-min))
                     (if (region-active-p) (max (point) (mark)) (point-max))
                     '(video audio subtitles)))
  (setq include (or include '(video audio subtitles)))
  (let* ((selection
          (subed-record-compile-get-base-selection (or beg (point-min))
                                                   (or end (point-max))))
         (output-groups (subed-record-compile-group-by-output-file selection)))
    (mapc
     (lambda (output-group)
       (apply
        #'compile-media
        (seq-filter
         (lambda (track) (member (car track) include))
         (subed-record-compile--format-tracks (cdr output-group)))
        (car output-group)
        (when play-afterwards
          (list
           :sentinel     
           (lambda (process event)
             (when (string-match "finished" event)
               (mpv-play (car output-group))))))))
     output-groups)))

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


(provide 'subed-record)

;;; subed-waveform.el ends here
