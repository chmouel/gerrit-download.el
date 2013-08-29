;; gerrit-review.el --- Download gerrit review and diff the change.

;; Copyright (C) 2013 Chmouel Boudjnah

;; Author: Chmouel Boudjnah <chmouel@chmouel.com>
;; Created: 20 Aug 2013
;; Keywords: tools
;; Package-Requires: ((magit "20130828.1540"))

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Change Log:

;; 29 Aug 2013 - Created

;;; Commentary:

;;; This is a mode that will download a review from gerrit using the
;;; `git-review' software and run show the diff for that changes.
;;;
;;; This is using magit and the `magit-repo-dirs` variable to download
;;; the change into.
;;;
;;;


;;; ToDo:

;; gnus integration.
;; magit interface key.
;; only diff is showing not log.

(require 'magit)

(defvar gerrit-review-program "git-review"
  "Path to gerrit review.")

(defun gerrit-check-if-repo-modified ()
  "Check if current repo has been modified."
  (null (mapcar (lambda (line)
                       (string-match "^[ \t]+M" line))
                     (magit-git-lines "status" "--porcelain"))))

(defun gerrit-get-local-directory (project)
  "Get local project on filesystem from magit-repo-dirs."
  (mapconcat 'identity
   (mapcar (lambda (path)
             (cond
              ((file-exists-p (concat path "/" project))
               (concat path "/" project))
              ((file-exists-p (concat path "/" (file-name-nondirectory
                                                project)))
               (concat path "/" (file-name-nondirectory project)))
              )) magit-repo-dirs) ""))

;;;###autoload
(defun gerrit-download (project review-id)
  (interactive
   (list (read-string "Project: ")
         (read-string "Review-ID: ")))
  (let ((default-directory
          (magit-get-top-dir
           (gerrit-get-local-directory project)))
        changes)
    (unless default-directory
      (error "Cannot find %s in magit-repos-dir" project))
    (unless (gerrit-check-if-repo-modified)
        (error "%s has changes, not processing" project))

    (let ((proc (concat "git-review" review-id)))
      (message "Starting git-review...")
      (start-process proc "*git review*" gerrit-review-program "-d" review-id)
      (set-process-sentinel
       (get-process proc)
       #'(lambda (process event)
           (if (string= event "finished\n")
               (magit-diff "HEAD^1")
             (error "Error while downloading review, check *git review* buffer.")
            ))))))

(provide 'gerrit-download)

;;; gerrit-download.el ends here
