;;; magit-vcsh.el --- git-vcsh plug-in for Magit

;; Copyright (C) 2013 Rémi Vanicat

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides vcsh functionality as a separate component of
;; Magit.

;;; Code:
(defgroup magit-vcsh nil
  "Controlling Vcsh from Emacs."
  :prefix "magit-vcsh"
  :group 'magit)

(defcustom magit-vcsh-executable "vcsh"
  "The name of the vcsh executable."
  :group 'magit-vcsh
  :type 'string)

(defvar-local magit-vcsh-name ()
  "the vcsh name variable")
(put 'magit-vcsh-name 'permanent-local t)
(defvar magit-vcsh-name*)

(defvar-local magit-vcsh-env ()
  "the vcsh environment variable")
(put 'magit-vcsh-env 'permanent-local t)
(defvar magit-vcsh-env*)

(defun magit-vcsh-string (&rest args)
  (let ((magit-git-executable magit-vcsh-executable)
        (magit-git-standard-options ()))
    (apply 'magit-git-string args)))

(defun magit-vcsh-get-worktree (name)
  (magit-vcsh-string "run" name "git" "config" "core.worktree"))

(defun magit-vcsh-get-env (name)
  "get env from vcsh.

Return it in a form switable to append to `process-environment'"
  (let ((git-dir (magit-vcsh-string "run" name "sh" "-c" "echo \$GIT_DIR"))
        (vcsh-directory (magit-vcsh-string "run" name "sh" "-c" "echo \$VCSH_DIRECTORY"))
        (vcsh-repo-name (magit-vcsh-string "run" name "sh" "-c" "echo \$VCSH_REPO_NAME")))
    (list (format "GIT_DIR=%s" git-dir)
          (format "VCSH_DIRECTORY=%s" vcsh-directory)
          (format "VCSH_REPO_NAME=%s" vcsh-repo-name))))

(defun magit-vcsh-for-magit-hook ()
  "Set the buffer local variable of magit-vcsh

use the `magit-vcsh-name*` and `magit-vcsh-env*` variable that
are bind dynamicly."
  (when (boundp 'magit-vcsh-name*)
    (setq magit-vcsh-name magit-vcsh-name*)
    (make-local-variable 'process-environment)
    (setq process-environment (append magit-vcsh-env* process-environment))
    (setq magit-vcsh-env magit-vcsh-env*)))

(add-hook 'magit-mode-hook 'magit-vcsh-for-magit-hook)

(defmacro magit-vcsh-set-env (name new-buffer &rest body)
  "Run BODY with correct environement"
  (declare (indent defun)
           (debug (symbolp symbolp
                           def-body)))
  `(let* ((magit-vcsh-env* (magit-vcsh-get-env ,name))
          (process-environment (append magit-vcsh-env* process-environment))
          (magit-vcsh-name* ,name)
          (magit-status-buffer-name-format (format "*magit-vsch: %s %%a" ,name)))
     (progn ,@body)))

;;;###autoload
(defun magit-vcsh-status (name)
  "Get the magit-status buffer of a vcsh repository."
  (interactive "Mvcsh repos: ")

  (magit-vcsh-set-env name t
      (magit-status (magit-vcsh-get-worktree name))))

(defadvice magit-git-string (around magit-vcsh-git-string (&rest ARGS))
  (let ((process-environment process-environment))
    ad-do-it))

(ad-activate 'magit-git-string)

(provide 'magit-vcsh)
