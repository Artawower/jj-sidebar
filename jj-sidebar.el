;;; jj-sidebar.el --- Simple sidebar with changed JJ files  -*- lexical-binding: t; -*-

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/jj-sidebar
;; Package-Requires: ((emacs "29.1") (vui "0.1.0"))
;; Version: 0.0.2
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
;; Jujutsu repository.  Each entry can be used to open the corresponding file
;; in the last active editing window.

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

(cl-defstruct jj-sidebar-entry
  status
  path
  change-count)

(defvar-local jj-sidebar--root nil)
(defvar-local jj-sidebar--revisions nil)
(defvar-local jj-sidebar--entries nil)
(defvar-local jj-sidebar--error nil)
(defvar-local jj-sidebar--content-width 80)

(defvar jj-sidebar--target-window nil)

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
      :change-count count))))

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

(defun jj-sidebar--count-text (entry)
  (if jj-sidebar-show-counts
      (number-to-string (or (jj-sidebar-entry-change-count entry) 0))
    ""))

(defun jj-sidebar--entry-line-right (entry width)
  (let* ((status (jj-sidebar--status-label (jj-sidebar-entry-status entry)))
         (count-text (jj-sidebar--count-text entry))
         (count-width (length count-text))
         (path-width (max 8 (- width 3 count-width)))
         (path (jj-sidebar--truncate-middle (jj-sidebar-entry-path entry) path-width))
         (left (format "%s %s" status path))
         (spaces (max 1 (- width (length left) count-width))))
    (if jj-sidebar-show-counts
        (concat left (make-string spaces ?\s) count-text)
      left)))

(defun jj-sidebar--entry-line-left (entry width)
  (let* ((status (jj-sidebar--status-label (jj-sidebar-entry-status entry)))
         (count-text (jj-sidebar--count-text entry))
         (prefix (if jj-sidebar-show-counts
                     (format "%4s %s " count-text status)
                   (format "%s " status)))
         (path-width (max 8 (- width (length prefix))))
         (path (jj-sidebar--truncate-middle (jj-sidebar-entry-path entry) path-width)))
    (concat prefix path)))

(defun jj-sidebar--entry-line (entry width)
  (pcase jj-sidebar-count-position
    ('left (jj-sidebar--entry-line-left entry width))
    (_ (jj-sidebar--entry-line-right entry width))))

(defun jj-sidebar--entry-view (entry width)
  (vui-text
   (jj-sidebar--entry-line entry width)
   :face (jj-sidebar--status-face (jj-sidebar-entry-status entry))))

(defun jj-sidebar--header-view (root revisions entries)
  (vui-vstack
   :spacing 0
   (vui-hstack
    :spacing 1
    (vui-text "jj-sidebar" :face 'jj-sidebar-header-face)
    (vui-text revisions :face 'jj-sidebar-dim-face))
   (vui-hstack
    :spacing 2
    (vui-text (format "%d files" (length entries)) :face 'jj-sidebar-dim-face)
    (vui-button "refresh"
                :no-decoration t
                :on-click #'jj-sidebar-refresh)
    (vui-button (if jj-sidebar-show-counts "hide-counts" "show-counts")
                :no-decoration t
                :on-click #'jj-sidebar-toggle-counts)
    (vui-button "close"
                :no-decoration t
                :on-click #'jj-sidebar-close))
   (vui-text root :face 'jj-sidebar-dim-face)))

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

(defun jj-sidebar--open-button (button)
  (jj-sidebar-visit-file
   (button-get button 'jj-sidebar-root)
   (button-get button 'jj-sidebar-path)))

(defun jj-sidebar--attach-entry-buttons (buffer root entries width)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (remove-text-properties
       (point-min)
       (point-max)
       '(jj-sidebar-path nil jj-sidebar-root nil mouse-face nil help-echo nil))
      (save-excursion
        (goto-char (point-min))
        (dolist (entry entries)
          (let ((line (jj-sidebar--entry-line entry width))
                (path (jj-sidebar-entry-path entry)))
            (when (search-forward line nil t)
              (let ((start (line-beginning-position))
                    (end (line-end-position)))
                (make-text-button
                 start
                 end
                 'follow-link t
                 'face (jj-sidebar--status-face (jj-sidebar-entry-status entry))
                 'help-echo path
                 'jj-sidebar-root root
                 'jj-sidebar-path path
                 'action #'jj-sidebar--open-button)
                (add-text-properties
                 start
                 end
                 `(jj-sidebar-root ,root
                                   jj-sidebar-path ,path
                                   mouse-face highlight
                                   help-echo ,path))))))))))

(defun jj-sidebar--render-buffer (buffer root revisions entries error width)
  (with-current-buffer buffer
    (jj-sidebar-mode)
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
    (jj-sidebar--attach-entry-buttons buffer root entries width)
    (goto-char (point-min))))

(defun jj-sidebar--render-and-display (root revisions entries error)
  (let* ((buffer (get-buffer-create jj-sidebar-buffer-name))
         (selected-window-before-render (selected-window))
         (selected-buffer-before-render (window-buffer selected-window-before-render))
         (window (jj-sidebar--display-buffer buffer))
         (width (max 24 (1- (window-width window)))))
    (jj-sidebar--render-buffer buffer root revisions entries error width)
    (when (and (window-live-p selected-window-before-render)
               (not (eq selected-window-before-render window))
               (eq (window-buffer selected-window-before-render) buffer))
      (set-window-buffer selected-window-before-render selected-buffer-before-render))
    (when (window-live-p selected-window-before-render)
      (select-window selected-window-before-render))
    window))

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

(defun jj-sidebar-visit-file (root path)
  (interactive)
  (let ((file (expand-file-name path root)))
    (unless (file-exists-p file)
      (user-error "File does not exist in workspace: %s" path))
    (let ((window (jj-sidebar--target-window)))
      (select-window window)
      (find-file file)
      (setq jj-sidebar--target-window window))))

(defun jj-sidebar-open-file-at-point ()
  (interactive)
  (if-let* ((entry (jj-sidebar--entry-at-point)))
      (jj-sidebar-visit-file
       jj-sidebar--root
       (jj-sidebar-entry-path entry))
    (if-let* ((button (button-at (point))))
        (push-button button)
      (user-error "No file at point"))))

(defvar jj-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vui-mode-map)
    (define-key map (kbd "RET") #'jj-sidebar-open-file-at-point)
    (define-key map (kbd "g") #'jj-sidebar-refresh)
    (define-key map (kbd "q") #'jj-sidebar-close)
    (define-key map (kbd "s") #'jj-sidebar-toggle-counts)
    (define-key map (kbd "p") #'jj-sidebar-set-count-position)
    (define-key map (kbd "r") #'jj-sidebar-set-revisions)
    map))

(define-derived-mode jj-sidebar-mode vui-mode "JJ-Sidebar"
  "Major mode for `jj-sidebar'."
  (setq-local truncate-lines t))

(add-hook 'post-command-hook #'jj-sidebar--remember-target-window)

(provide 'jj-sidebar)
;;; jj-sidebar.el ends here
