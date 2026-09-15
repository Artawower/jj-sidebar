;;; jj-sidebar.el --- Simple sidebar with changed JJ files  -*- lexical-binding: t; -*-

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/jj-sidebar
;; Package-Requires: ((emacs "29.1") (vui "0.1.0"))
;; Version: 0.0.3
;; Copyright (C) 2026 Artur Yaroshenko

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a right/left side window with changed files from a
;; Jujutsu repository.  RET opens the selected file in the last active editing
;; window and leaves keyboard focus in that file.
;;
;; Files can also be marked as reviewed.  Review state is tied to the current
;; diff contents, so a reviewed file automatically becomes unreviewed if its
;; diff changes.  The last file opened with RET is highlighted in the sidebar
;; and can be persisted across Emacs restarts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'vui)

(defgroup jj-sidebar nil
  "Sidebar for Jujutsu changed files."
  :group 'tools
  :prefix "jj-sidebar-")

(defcustom jj-sidebar-executable "jj"
  "Jujutsu executable."
  :type 'string
  :group 'jj-sidebar)

(defcustom jj-sidebar-revisions "@"
  "Default revset shown by `jj-sidebar'."
  :type 'string
  :group 'jj-sidebar)

(defcustom jj-sidebar-buffer-name "*jj-sidebar*"
  "Sidebar buffer name."
  :type 'string
  :group 'jj-sidebar)

(defcustom jj-sidebar-side 'right
  "Side used for the sidebar window."
  :type '(choice (const right) (const left))
  :group 'jj-sidebar)

(defcustom jj-sidebar-width 0.32
  "Sidebar window width."
  :type '(choice integer float)
  :group 'jj-sidebar)

(defcustom jj-sidebar-slot 0
  "Sidebar side-window slot."
  :type 'integer
  :group 'jj-sidebar)

(defcustom jj-sidebar-show-counts t
  "Whether to show change counts."
  :type 'boolean
  :group 'jj-sidebar)

(defcustom jj-sidebar-count-position 'right
  "Position of the change count."
  :type '(choice (const right) (const left))
  :group 'jj-sidebar)

(defcustom jj-sidebar-dedicated-window t
  "Whether to make the sidebar window dedicated."
  :type 'boolean
  :group 'jj-sidebar)

(defcustom jj-sidebar-review-state-file
  (expand-file-name "jj-sidebar-review-state.el" user-emacs-directory)
  "File used to persist reviewed file state.

When nil, review state is kept only for the current Emacs session."
  :type '(choice (const :tag "Do not persist" nil) file)
  :group 'jj-sidebar)

(defcustom jj-sidebar-last-opened-state-file
  (expand-file-name "jj-sidebar-last-opened-state.el" user-emacs-directory)
  "File used to persist the last file opened from each sidebar.

When nil, the highlighted last-opened file is kept only for the current
Emacs session."
  :type '(choice (const :tag "Do not persist" nil) file)
  :group 'jj-sidebar)

(defface jj-sidebar-header-face
  '((t :inherit bold))
  "Face for the sidebar header."
  :group 'jj-sidebar)

(defface jj-sidebar-path-face
  '((t :inherit default))
  "Face for file paths."
  :group 'jj-sidebar)

(defface jj-sidebar-added-face
  '((t :inherit success))
  "Face for added files."
  :group 'jj-sidebar)

(defface jj-sidebar-deleted-face
  '((t :inherit error))
  "Face for deleted files."
  :group 'jj-sidebar)

(defface jj-sidebar-modified-face
  '((t :inherit warning))
  "Face for modified files."
  :group 'jj-sidebar)

(defface jj-sidebar-dim-face
  '((t :inherit shadow))
  "Face for secondary text."
  :group 'jj-sidebar)

(defface jj-sidebar-reviewed-face
  '((t :inherit success))
  "Face for reviewed checkboxes."
  :group 'jj-sidebar)

(defface jj-sidebar-current-file-face
  '((t :inherit hl-line :extend t))
  "Face for the last file opened from the sidebar."
  :group 'jj-sidebar)

(cl-defstruct jj-sidebar-entry
  status
  path
  change-count
  reviewed)

(defvar-local jj-sidebar--root nil)
(defvar-local jj-sidebar--revisions nil)
(defvar-local jj-sidebar--entries nil)
(defvar-local jj-sidebar--error nil)
(defvar-local jj-sidebar--content-width 80)

(defvar jj-sidebar--target-window nil)

(defvar jj-sidebar--reviewed-state nil
  "Stored review state as ((ROOT REVISIONS PATH) . DIFF-HASH) entries.")

(defvar jj-sidebar--reviewed-state-loaded nil)

(defvar jj-sidebar--last-opened-files nil
  "Alist mapping (ROOT REVISIONS) to the last opened file path.")

(defvar jj-sidebar--last-opened-state-loaded nil)

(defun jj-sidebar--call (directory &rest args)
  (let ((default-directory (or directory default-directory)))
    (with-temp-buffer
      (let ((exit-code
             (apply #'process-file
                    jj-sidebar-executable
                    nil
                    t
                    nil
                    "--no-pager"
                    "--color=never"
                    args)))
        (unless (zerop exit-code)
          (error "%s" (string-trim (buffer-string))))
        (buffer-string)))))

(defun jj-sidebar--repo-root (&optional directory)
  (file-name-as-directory
   (string-trim
    (jj-sidebar--call directory "root"))))

(defun jj-sidebar--revision-arg (revisions)
  (concat "--revisions=" revisions))

(defun jj-sidebar--parse-summary-line (line)
  (when (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+\\(.+\\)\\'" line)
    (cons (match-string 1 line)
          (match-string 2 line))))

(defun jj-sidebar--parse-stat-line (line)
  (cond
   ((string-match "|[[:space:]]*\\([0-9]+\\)\\([[:space:]]\\|$\\)" line)
    (string-to-number (match-string 1 line)))
   ((string-match "|[[:space:]]*Bin\\([[:space:]]\\|$\\)" line)
    0)
   (t nil)))

(defun jj-sidebar--collect-stat-counts (root revisions)
  (let* ((revision-arg (jj-sidebar--revision-arg revisions))
         (text (jj-sidebar--call root "diff" "--stat" revision-arg))
         (lines (split-string text "\n")))
    (seq-keep #'jj-sidebar--parse-stat-line lines)))

(defun jj-sidebar--added-line-p (line)
  (and (string-prefix-p "+" line)
       (not (string-prefix-p "+++" line))))

(defun jj-sidebar--deleted-line-p (line)
  (and (string-prefix-p "-" line)
       (not (string-prefix-p "---" line))))

(defun jj-sidebar--fallback-change-count-for-path (root revisions path)
  (let* ((revision-arg (jj-sidebar--revision-arg revisions))
         (text (jj-sidebar--call root
                                 "diff"
                                 "--git"
                                 "--context=0"
                                 revision-arg
                                 "--"
                                 path))
         (count 0))
    (dolist (line (split-string text "\n"))
      (when (or (jj-sidebar--added-line-p line)
                (jj-sidebar--deleted-line-p line))
        (setq count (1+ count))))
    count))

(defun jj-sidebar--summary-entries (root revisions)
  (let* ((revision-arg (jj-sidebar--revision-arg revisions))
         (summary (jj-sidebar--call root "diff" "--summary" revision-arg))
         (lines (seq-filter
                 (lambda (line)
                   (not (string-empty-p line)))
                 (split-string summary "\n"))))
    (seq-keep #'jj-sidebar--parse-summary-line lines)))

(defun jj-sidebar--load-review-state ()
  (unless jj-sidebar--reviewed-state-loaded
    (setq jj-sidebar--reviewed-state-loaded t)
    (when (and jj-sidebar-review-state-file
               (file-readable-p jj-sidebar-review-state-file))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents jj-sidebar-review-state-file)
            (goto-char (point-min))
            (let ((state (read (current-buffer))))
              (when (listp state)
                (setq jj-sidebar--reviewed-state state))))
        (error
         (message "jj-sidebar: could not load review state: %s"
                  (error-message-string err)))))))

(defun jj-sidebar--save-review-state ()
  (when jj-sidebar-review-state-file
    (condition-case err
        (progn
          (when-let* ((directory (file-name-directory jj-sidebar-review-state-file)))
            (make-directory directory t))
          (with-temp-file jj-sidebar-review-state-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 jj-sidebar--reviewed-state (current-buffer))
              (insert "\n"))))
      (error
       (message "jj-sidebar: could not save review state: %s"
                (error-message-string err))))))

(defun jj-sidebar--review-key (root revisions path)
  (list (file-truename root) revisions path))

(defun jj-sidebar--diff-fingerprint (root revisions path)
  (secure-hash
   'sha1
   (jj-sidebar--call root
                     "diff"
                     "--git"
                     "--context=0"
                     (jj-sidebar--revision-arg revisions)
                     "--"
                     path)))

(defun jj-sidebar--stored-review-hash (root revisions path)
  (jj-sidebar--load-review-state)
  (cdr (assoc (jj-sidebar--review-key root revisions path)
              jj-sidebar--reviewed-state)))

(defun jj-sidebar--reviewed-p (root revisions path)
  (when-let* ((stored-hash
               (jj-sidebar--stored-review-hash root revisions path)))
    (condition-case nil
        (equal stored-hash
               (jj-sidebar--diff-fingerprint root revisions path))
      (error nil))))

(defun jj-sidebar--set-reviewed-state (root revisions path reviewed)
  (jj-sidebar--load-review-state)
  (let ((key (jj-sidebar--review-key root revisions path)))
    (setq jj-sidebar--reviewed-state
          (cl-remove key
                     jj-sidebar--reviewed-state
                     :key #'car
                     :test #'equal))
    (when reviewed
      (push (cons key (jj-sidebar--diff-fingerprint root revisions path))
            jj-sidebar--reviewed-state))
    (jj-sidebar--save-review-state)))

(defun jj-sidebar--load-last-opened-state ()
  (unless jj-sidebar--last-opened-state-loaded
    (setq jj-sidebar--last-opened-state-loaded t)
    (when (and jj-sidebar-last-opened-state-file
               (file-readable-p jj-sidebar-last-opened-state-file))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents jj-sidebar-last-opened-state-file)
            (goto-char (point-min))
            (let ((state (read (current-buffer))))
              (when (listp state)
                (setq jj-sidebar--last-opened-files state))))
        (error
         (message "jj-sidebar: could not load last-opened state: %s"
                  (error-message-string err)))))))

(defun jj-sidebar--save-last-opened-state ()
  (when jj-sidebar-last-opened-state-file
    (condition-case err
        (progn
          (when-let* ((directory
                       (file-name-directory jj-sidebar-last-opened-state-file)))
            (make-directory directory t))
          (with-temp-file jj-sidebar-last-opened-state-file
            (let ((print-length nil)
                  (print-level nil))
              (prin1 jj-sidebar--last-opened-files (current-buffer))
              (insert "\n"))))
      (error
       (message "jj-sidebar: could not save last-opened state: %s"
                (error-message-string err))))))

(defun jj-sidebar--last-opened-key (root revisions)
  (list (file-truename root) revisions))

(defun jj-sidebar--last-opened-path (root revisions)
  (jj-sidebar--load-last-opened-state)
  (cdr
   (assoc
    (jj-sidebar--last-opened-key root revisions)
    jj-sidebar--last-opened-files)))

(defun jj-sidebar--remember-last-opened (root revisions path)
  (jj-sidebar--load-last-opened-state)
  (let ((key (jj-sidebar--last-opened-key root revisions)))
    (setq jj-sidebar--last-opened-files
          (cl-remove key
                     jj-sidebar--last-opened-files
                     :key #'car
                     :test #'equal))
    (push (cons key path)
          jj-sidebar--last-opened-files)
    (jj-sidebar--save-last-opened-state)))

(defun jj-sidebar--collect-entries (root revisions)
  (let ((summary-entries (jj-sidebar--summary-entries root revisions))
        (stat-counts (jj-sidebar--collect-stat-counts root revisions)))
    (cl-loop
     for parsed in summary-entries
     for index from 0
     for status = (car parsed)
     for path = (cdr parsed)
     for count = (or (nth index stat-counts)
                     (jj-sidebar--fallback-change-count-for-path root revisions path))
     collect
     (make-jj-sidebar-entry
      :status status
      :path path
      :change-count count
      :reviewed (jj-sidebar--reviewed-p root revisions path)))))

(defun jj-sidebar--status-label (status)
  (cond
   ((string-match-p "A" status) "+")
   ((string-match-p "D" status) "-")
   ((string-match-p "R" status) "R")
   ((string-match-p "C" status) "C")
   (t "M")))

(defun jj-sidebar--status-face (status)
  (cond
   ((string-match-p "A" status) 'jj-sidebar-added-face)
   ((string-match-p "D" status) 'jj-sidebar-deleted-face)
   (t 'jj-sidebar-modified-face)))

(defun jj-sidebar--truncate-middle (text width)
  (if (<= (length text) width)
      text
    (let* ((left (max 1 (/ (1- width) 2)))
           (right (max 1 (- width left 1))))
      (concat (substring text 0 left)
              "…"
              (substring text (- right))))))

(defun jj-sidebar--checkbox-text (entry)
  (if (jj-sidebar-entry-reviewed entry) "[x]" "[ ]"))

(defun jj-sidebar--count-text (entry)
  (if jj-sidebar-show-counts
      (number-to-string (or (jj-sidebar-entry-change-count entry) 0))
    ""))

(defun jj-sidebar--entry-line-right (entry width)
  (let* ((checkbox (jj-sidebar--checkbox-text entry))
         (status (jj-sidebar--status-label (jj-sidebar-entry-status entry)))
         (count-text (jj-sidebar--count-text entry))
         (count-width (length count-text))
         (prefix (format "%s %s " checkbox status))
         (path-width (max 8
                          (- width
                             (length prefix)
                             count-width
                             (if jj-sidebar-show-counts 1 0))))
         (path (jj-sidebar--truncate-middle
                (jj-sidebar-entry-path entry)
                path-width))
         (left (concat prefix path))
         (spaces (max 1 (- width (length left) count-width))))
    (if jj-sidebar-show-counts
        (concat left (make-string spaces ?\s) count-text)
      left)))

(defun jj-sidebar--entry-line-left (entry width)
  (let* ((checkbox (jj-sidebar--checkbox-text entry))
         (status (jj-sidebar--status-label (jj-sidebar-entry-status entry)))
         (count-text (jj-sidebar--count-text entry))
         (prefix (if jj-sidebar-show-counts
                     (format "%s %4s %s " checkbox count-text status)
                   (format "%s %s " checkbox status)))
         (path-width (max 8 (- width (length prefix))))
         (path (jj-sidebar--truncate-middle
                (jj-sidebar-entry-path entry)
                path-width)))
    (concat prefix path)))

(defun jj-sidebar--entry-line (entry width)
  (pcase jj-sidebar-count-position
    ('left (jj-sidebar--entry-line-left entry width))
    (_ (jj-sidebar--entry-line-right entry width))))

(defun jj-sidebar--entry-view (entry width)
  (vui-text
      (jj-sidebar--entry-line entry width)
    :face (if (jj-sidebar-entry-reviewed entry)
              'jj-sidebar-dim-face
            (jj-sidebar--status-face (jj-sidebar-entry-status entry)))))

(defun jj-sidebar--header-view (root revisions entries)
  (let ((reviewed-count (cl-count-if #'jj-sidebar-entry-reviewed entries)))
    (vui-vstack
     :spacing 0
     (vui-hstack
      :spacing 1
      (vui-text "jj-sidebar" :face 'jj-sidebar-header-face)
      (vui-text revisions :face 'jj-sidebar-dim-face))
     (vui-hstack
      :spacing 2
      (vui-text (format "%d files" (length entries))
        :face 'jj-sidebar-dim-face)
      (vui-text (format "%d/%d reviewed" reviewed-count (length entries))
        :face 'jj-sidebar-dim-face)
      (vui-button "refresh"
        :no-decoration t
        :on-click #'jj-sidebar-refresh)
      (vui-button (if jj-sidebar-show-counts "hide-counts" "show-counts")
        :no-decoration t
        :on-click #'jj-sidebar-toggle-counts)
      (vui-button "close"
        :no-decoration t
        :on-click #'jj-sidebar-close))
     (vui-text root :face 'jj-sidebar-dim-face))))

(defun jj-sidebar--view (root revisions entries error width)
  (vui-vstack
   :spacing 0
   (jj-sidebar--header-view root revisions entries)
   (vui-newline)
   (cond
    (error
     (vui-text error :face 'error))
    ((null entries)
     (vui-text "No changed files" :face 'jj-sidebar-dim-face))
    (t
     (mapcar
      (lambda (entry)
        (jj-sidebar--entry-view entry width))
      entries)))))

(defun jj-sidebar--sidebar-buffer-p (buffer)
  (eq buffer (get-buffer jj-sidebar-buffer-name)))

(defun jj-sidebar--sidebar-window-p (window)
  (and (windowp window)
       (window-live-p window)
       (jj-sidebar--sidebar-buffer-p (window-buffer window))))

(defun jj-sidebar--usable-target-window-p (window)
  (and (windowp window)
       (window-live-p window)
       (not (window-minibuffer-p window))
       (not (jj-sidebar--sidebar-window-p window))))

(defun jj-sidebar--remember-target-window (&rest _)
  (let ((window (selected-window)))
    (when (jj-sidebar--usable-target-window-p window)
      (setq jj-sidebar--target-window window))))

(defun jj-sidebar--fallback-target-window ()
  (seq-find
   #'jj-sidebar--usable-target-window-p
   (window-list (selected-frame) 'nomini)))

(defun jj-sidebar--target-window ()
  (cond
   ((jj-sidebar--usable-target-window-p jj-sidebar--target-window)
    jj-sidebar--target-window)
   ((jj-sidebar--fallback-target-window))
   (t
    (selected-window))))

(defun jj-sidebar--display-buffer (buffer)
  (let ((window
         (display-buffer-in-side-window
          buffer
          `((side . ,jj-sidebar-side)
            (slot . ,jj-sidebar-slot)
            (window-width . ,jj-sidebar-width)
            (window-parameters . ((no-delete-other-windows . t)))))))
    (when jj-sidebar-dedicated-window
      (set-window-dedicated-p window t))
    window))

(defun jj-sidebar--stored-root ()
  (when-let* ((buffer (get-buffer jj-sidebar-buffer-name)))
    (with-current-buffer buffer
      jj-sidebar--root)))

(defun jj-sidebar--stored-revisions ()
  (when-let* ((buffer (get-buffer jj-sidebar-buffer-name)))
    (with-current-buffer buffer
      jj-sidebar--revisions)))

(defun jj-sidebar--read-revisions ()
  (read-string "jj revisions: "
               (or (jj-sidebar--stored-revisions)
                   jj-sidebar-revisions)))

(defun jj-sidebar--line-path-at-point ()
  (or (get-text-property (point) 'jj-sidebar-path)
      (get-text-property (line-beginning-position) 'jj-sidebar-path)
      (get-text-property (max (line-beginning-position)
                              (1- (line-end-position)))
                         'jj-sidebar-path)))

(defun jj-sidebar--entry-at-point ()
  (when-let* ((path (jj-sidebar--line-path-at-point)))
    (seq-find
     (lambda (entry)
       (equal (jj-sidebar-entry-path entry) path))
     jj-sidebar--entries)))

(defun jj-sidebar--goto-path (path)
  (when path
    (let ((position (point-min))
          found)
      (while (and (< position (point-max))
                  (not found))
        (if (equal (get-text-property position 'jj-sidebar-path) path)
            (setq found position)
          (setq position
                (next-single-property-change
                 position
                 'jj-sidebar-path
                 nil
                 (point-max)))))
      (when found
        (goto-char found)
        (beginning-of-line)
        t))))

(defun jj-sidebar--open-button (button)
  (jj-sidebar-visit-file
   (button-get button 'jj-sidebar-root)
   (button-get button 'jj-sidebar-path)
   (button-get button 'jj-sidebar-revisions)))

(defun jj-sidebar--toggle-reviewed-mouse (event)
  "Toggle reviewed state for the entry clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (jj-sidebar-toggle-reviewed))

(defvar jj-sidebar-checkbox-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'jj-sidebar--toggle-reviewed-mouse)
    map)
  "Mouse-only keymap used by review checkboxes.

Keyboard activation is intentionally omitted so RET always opens the file.
Use SPC to toggle reviewed state.")

(defun jj-sidebar--attach-entry-buttons (buffer root revisions entries width)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (last-opened-path
           (jj-sidebar--last-opened-path root revisions)))
      (remove-text-properties
       (point-min)
       (point-max)
       '(jj-sidebar-path nil
                         jj-sidebar-root nil
                         jj-sidebar-revisions nil
                         keymap nil
                         mouse-face nil
                         help-echo nil))
      (save-excursion
        (goto-char (point-min))
        (dolist (entry entries)
          (let ((line (jj-sidebar--entry-line entry width))
                (path (jj-sidebar-entry-path entry)))
            (when (search-forward line nil t)
              (let* ((start (line-beginning-position))
                     (line-end (line-end-position))
                     (end (min (point-max) (1+ line-end)))
                     (checkbox-start start)
                     (checkbox-end (min line-end (+ start 3)))
                     (file-start (min line-end (+ checkbox-end 1))))
                ;; Keep entry metadata available from every character in the row.
                (add-text-properties
                 start
                 line-end
                 `(jj-sidebar-root ,root
                                   jj-sidebar-revisions ,revisions
                                   jj-sidebar-path ,path
                                   help-echo ,path))

                ;; Checkbox is mouse-clickable, but deliberately is not an
                ;; Emacs button.  RET is therefore never captured here.
                (when (< checkbox-start checkbox-end)
                  (add-text-properties
                   checkbox-start
                   checkbox-end
                   `(keymap ,jj-sidebar-checkbox-map
                            mouse-face highlight
                            help-echo
                            ,(if (jj-sidebar-entry-reviewed entry)
                                 "Mark as not reviewed (SPC)"
                               "Mark as reviewed (SPC)")
                            face
                            ,(if (jj-sidebar-entry-reviewed entry)
                                 'jj-sidebar-reviewed-face
                               'jj-sidebar-dim-face))))

                ;; The rest of the row is mouse-clickable as a file button.
                ;; RET is handled by the major-mode binding and always opens
                ;; the file, regardless of point being on the checkbox.
                (when (< file-start line-end)
                  (make-text-button
                   file-start
                   line-end
                   'follow-link t
                   'face (if (jj-sidebar-entry-reviewed entry)
                             'jj-sidebar-dim-face
                           (jj-sidebar--status-face
                            (jj-sidebar-entry-status entry)))
                   'mouse-face 'highlight
                   'help-echo path
                   'jj-sidebar-root root
                   'jj-sidebar-revisions revisions
                   'jj-sidebar-path path
                   'action #'jj-sidebar--open-button))

                ;; Apply this last so the current-file background wins over
                ;; VUI/button faces while their foreground colors are kept.
                ;; Including the newline lets :extend fill the whole row.
                (when (equal path last-opened-path)
                  (add-face-text-property
                   start
                   end
                   'jj-sidebar-current-file-face
                   nil))))))))))

(defun jj-sidebar--render-buffer
    (buffer root revisions entries error width &optional preserve-path)
  (with-current-buffer buffer
    ;; Re-entering a major mode kills buffer-local variables.  Only initialize
    ;; it once so sidebar state survives re-renders.
    (unless (derived-mode-p 'jj-sidebar-mode)
      (jj-sidebar-mode))
    (setq default-directory root)
    (setq jj-sidebar--root root)
    (setq jj-sidebar--revisions revisions)
    (setq jj-sidebar--entries entries)
    (setq jj-sidebar--error error)
    (setq jj-sidebar--content-width width)
    (setq-local truncate-lines t)
    (vui-render
     (jj-sidebar--view root revisions entries error width)
     buffer)
    (jj-sidebar--attach-entry-buttons buffer root revisions entries width)
    (unless (jj-sidebar--goto-path preserve-path)
      (goto-char (point-min)))))

(defun jj-sidebar--render-and-display (root revisions entries error)
  (let* ((buffer (get-buffer-create jj-sidebar-buffer-name))
         (existing-window (get-buffer-window buffer nil))
         (current-path
          (with-current-buffer buffer
            (when (derived-mode-p 'jj-sidebar-mode)
              (jj-sidebar--line-path-at-point))))
         ;; Preserve point only while an already visible sidebar is being
         ;; refreshed.  Reopening a closed sidebar does not move point to the
         ;; last-opened file; that state is represented only by the highlight.
         (preserve-path (and existing-window current-path))
         (selected-window-before-render (selected-window))
         (selected-buffer-before-render
          (window-buffer selected-window-before-render))
         (window (jj-sidebar--display-buffer buffer))
         (width (max 24 (1- (window-width window)))))
    (jj-sidebar--render-buffer
     buffer root revisions entries error width preserve-path)
    (when (and (window-live-p selected-window-before-render)
               (not (eq selected-window-before-render window))
               (eq (window-buffer selected-window-before-render) buffer))
      (set-window-buffer
       selected-window-before-render
       selected-buffer-before-render))
    (when (window-live-p selected-window-before-render)
      (select-window selected-window-before-render))
    window))

(defun jj-sidebar--rerender-current-buffer (&optional preserve-path)
  (jj-sidebar--render-buffer
   (current-buffer)
   jj-sidebar--root
   jj-sidebar--revisions
   jj-sidebar--entries
   jj-sidebar--error
   jj-sidebar--content-width
   preserve-path))

;;;###autoload
(defun jj-sidebar-open (&optional revisions)
  (interactive
   (list
    (when current-prefix-arg
      (jj-sidebar--read-revisions))))
  (jj-sidebar--remember-target-window)
  (let* ((revisions (or revisions jj-sidebar-revisions))
         (root nil)
         (entries nil)
         (error nil))
    (condition-case err
        (setq root (jj-sidebar--repo-root)
              entries (jj-sidebar--collect-entries root revisions))
      (error
       (setq root default-directory)
       (setq error (error-message-string err))))
    (jj-sidebar--render-and-display root revisions entries error)))

;;;###autoload
(defun jj-sidebar ()
  (interactive)
  (if (get-buffer-window jj-sidebar-buffer-name nil)
      (jj-sidebar-close)
    (jj-sidebar-open)))

;;;###autoload
(defun jj-sidebar-refresh ()
  (interactive)
  (jj-sidebar--remember-target-window)
  (let* ((root (or (jj-sidebar--stored-root)
                   (jj-sidebar--repo-root)))
         (revisions (or (jj-sidebar--stored-revisions)
                        jj-sidebar-revisions))
         (entries nil)
         (error nil))
    (condition-case err
        (setq entries (jj-sidebar--collect-entries root revisions))
      (error
       (setq error (error-message-string err))))
    (jj-sidebar--render-and-display root revisions entries error)))

(defun jj-sidebar-close ()
  (interactive)
  (when-let* ((window (get-buffer-window jj-sidebar-buffer-name nil)))
    (delete-window window)))

(defun jj-sidebar-toggle-counts ()
  (interactive)
  (setq jj-sidebar-show-counts (not jj-sidebar-show-counts))
  (jj-sidebar-refresh))

(defun jj-sidebar-set-count-position (position)
  (interactive
   (list
    (intern
     (completing-read "Count position: " '("right" "left") nil t))))
  (setq jj-sidebar-count-position position)
  (jj-sidebar-refresh))

(defun jj-sidebar-set-revisions (revisions)
  (interactive (list (jj-sidebar--read-revisions)))
  (setq jj-sidebar-revisions revisions)
  (jj-sidebar-open revisions))

(defun jj-sidebar-visit-file (root path &optional revisions)
  "Open PATH from ROOT and leave focus in the opened file.

Before switching windows, remember PATH as the last file opened from this
sidebar and update the sidebar highlight."
  (interactive
   (let ((path (jj-sidebar--line-path-at-point)))
     (unless path
       (user-error "No file at point"))
     (list jj-sidebar--root path jj-sidebar--revisions)))
  (let* ((revisions (or revisions
                        (jj-sidebar--stored-revisions)
                        jj-sidebar-revisions))
         (file (expand-file-name path root))
         (sidebar-buffer (get-buffer jj-sidebar-buffer-name))
         (window (jj-sidebar--target-window)))
    (unless (file-exists-p file)
      (user-error "File does not exist in workspace: %s" path))
    (unless (jj-sidebar--usable-target-window-p window)
      (user-error "No usable target window"))

    ;; Last-opened state belongs to the action, not to sidebar point.  Re-render
    ;; the sidebar first so the row stays highlighted after focus moves into
    ;; the opened file.
    (jj-sidebar--remember-last-opened root revisions path)
    (when (buffer-live-p sidebar-buffer)
      (with-current-buffer sidebar-buffer
        (when (derived-mode-p 'jj-sidebar-mode)
          (jj-sidebar--rerender-current-buffer path))))

    (select-window window)
    (find-file file)
    (setq jj-sidebar--target-window window)))

(defun jj-sidebar-open-file-at-point ()
  "Open the file on the current row.

RET always calls this command, including when point is inside the checkbox."
  (interactive)
  (if-let* ((entry (jj-sidebar--entry-at-point)))
      (jj-sidebar-visit-file
       jj-sidebar--root
       (jj-sidebar-entry-path entry)
       jj-sidebar--revisions)
    (user-error "No file at point")))

(defun jj-sidebar-toggle-reviewed (&optional root revisions path)
  "Toggle reviewed state for PATH.

When called interactively, use the file at point."
  (interactive)
  (let* ((root (or root jj-sidebar--root))
         (revisions (or revisions jj-sidebar--revisions))
         (path (or path (jj-sidebar--line-path-at-point))))
    (unless path
      (user-error "No file at point"))
    (when-let* ((entry
                 (seq-find
                  (lambda (candidate)
                    (equal (jj-sidebar-entry-path candidate) path))
                  jj-sidebar--entries)))
      (let ((reviewed (not (jj-sidebar-entry-reviewed entry))))
        (condition-case err
            (progn
              (jj-sidebar--set-reviewed-state
               root revisions path reviewed)
              (setf (jj-sidebar-entry-reviewed entry) reviewed)
              (jj-sidebar--rerender-current-buffer path))
          (error
           (user-error "Could not update review state: %s"
                       (error-message-string err))))))))

(defun jj-sidebar--current-entry-index ()
  (when-let* ((path (jj-sidebar--line-path-at-point)))
    (cl-position path
                 jj-sidebar--entries
                 :key #'jj-sidebar-entry-path
                 :test #'equal)))

(defun jj-sidebar--move-file (delta)
  "Move DELTA entries and preview the selected file."
  (unless jj-sidebar--entries
    (user-error "No changed files"))
  (let* ((count (length jj-sidebar--entries))
         (current-index (jj-sidebar--current-entry-index))
         (target-index
          (if current-index
              (mod (+ current-index delta) count)
            (if (> delta 0) 0 (1- count))))
         (entry (nth target-index jj-sidebar--entries))
         (path (jj-sidebar-entry-path entry)))
    (jj-sidebar--goto-path path)
    (jj-sidebar-visit-file jj-sidebar--root path jj-sidebar--revisions)))

(defun jj-sidebar-next-file ()
  "Move to and preview the next changed file."
  (interactive)
  (jj-sidebar--move-file 1))

(defun jj-sidebar-previous-file ()
  "Move to and preview the previous changed file."
  (interactive)
  (jj-sidebar--move-file -1))

(defvar jj-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vui-mode-map)
    map)
  "Keymap for `jj-sidebar-mode'.")

(defun jj-sidebar--install-mode-bindings ()
  "Install `jj-sidebar-mode' bindings.

This function is intentionally called both at load time and whenever
`jj-sidebar-mode' starts.  This makes re-evaluating the file update an
already existing keymap as well."
  (set-keymap-parent jj-sidebar-mode-map vui-mode-map)
  (define-key jj-sidebar-mode-map (kbd "RET") #'jj-sidebar-open-file-at-point)
  (define-key jj-sidebar-mode-map (kbd "SPC") #'jj-sidebar-toggle-reviewed)
  (define-key jj-sidebar-mode-map (kbd "v") #'jj-sidebar-toggle-reviewed)
  (define-key jj-sidebar-mode-map (kbd "j") #'jj-sidebar-next-file)
  (define-key jj-sidebar-mode-map (kbd "n") #'jj-sidebar-next-file)
  (define-key jj-sidebar-mode-map (kbd "k") #'jj-sidebar-previous-file)
  (define-key jj-sidebar-mode-map (kbd "p") #'jj-sidebar-previous-file)
  (define-key jj-sidebar-mode-map (kbd "g") #'jj-sidebar-refresh)
  (define-key jj-sidebar-mode-map (kbd "q") #'jj-sidebar-close)
  (define-key jj-sidebar-mode-map (kbd "s") #'jj-sidebar-toggle-counts)
  (define-key jj-sidebar-mode-map (kbd "P") #'jj-sidebar-set-count-position)
  (define-key jj-sidebar-mode-map (kbd "r") #'jj-sidebar-set-revisions))

;; Apply bindings immediately when this file is evaluated.
(jj-sidebar--install-mode-bindings)

(define-derived-mode jj-sidebar-mode vui-mode "JJ-Sidebar"
  "Major mode for `jj-sidebar'."
  (setq-local truncate-lines t)
  ;; Re-apply bindings for reload-driven development.
  (jj-sidebar--install-mode-bindings))

(define-minor-mode jj-sidebar-target-window-tracking-mode
  "Track the most recently selected non-sidebar window globally.

This mode is optional and disabled by default.  It installs
`jj-sidebar--remember-target-window' in `post-command-hook' only while the mode
is enabled.  The basic sidebar workflow does not require this mode: opening the
sidebar already remembers the selected editing window.

Enable it explicitly when you want the sidebar to follow window changes that
happen while the sidebar remains open."
  :global t
  :group 'jj-sidebar
  (if jj-sidebar-target-window-tracking-mode
      (add-hook 'post-command-hook #'jj-sidebar--remember-target-window)
    (remove-hook 'post-command-hook #'jj-sidebar--remember-target-window)))

(provide 'jj-sidebar)
;;; jj-sidebar.el ends here
