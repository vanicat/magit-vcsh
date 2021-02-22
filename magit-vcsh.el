;;; magit-vcsh.el --- git-vcsh plug-in for Magit

;; Copyright (C) 2013, 2015 Rémi Vanicat

;; Author: Rémi Vanicat <vanicat@debian.org>

;; Package-Requires: ((emacs "24.4") (magit "2.3.0"))
;; Keywords: tools
;; Homepage: https://github.com/vanicat/magit-vcsh
;; Version: 0.2

;; Magit-vcsh is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit-vcsh is distributed in the hope that it will be useful, but WITHOUT
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
(require 'magit-git)
(require 'magit-process)

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

(defun magit-vcsh-get-environment (name)
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
    (setq magit-vcsh-process-environment (append magit-vcsh-env* process-environment))
    (setq magit-vcsh-env magit-vcsh-env*)))

(add-hook 'magit-mode-hook 'magit-vcsh-for-magit-hook)

(defmacro magit-vcsh-set-env (name &rest body)
  "Run BODY with correct environement

if NAME is nil, then it will use `magit-vcsh-name' and `magit-vcsh-env' that must be set
if NAME not nil, it is a name of a vcsh repos"
  (declare (debug (body)))
  (let ((process (cl-gensym "process"))
        (name-format (cl-gensym "name-format"))
        (namesym (cl-gensym "name")))
    `(let* ((,namesym ,name)
            (magit-vcsh-env* (if ,namesym
                                 (magit-vcsh-get-environment ,namesym)
                               magit-vcsh-env))
            (magit-vcsh-name* (or ,namesym
                                  magit-vcsh-name))
            (,process process-environment)
            (,name-format magit-buffer-name-format))
       (unwind-protect
           (progn
             (setq process-environment (append magit-vcsh-env* process-environment))
             (setq magit-buffer-name-format (format "*vcsh%%v: %s, %%M%%v: %%t*" magit-vcsh-name*))
             ,@body)
         (setq process-environment ,process)
         (setq magit-buffer-name-format ,name-format)))))

(defun vcsh-repos ()
  "Executes the command 'vcsh list' and parse the result into a list"
  (split-string (shell-command-to-string "vcsh list")))

;;;###autoload
(defun magit-vcsh-status (name)
  "Get the magit-status buffer of a vcsh repository."
  (interactive
   (list
    (completing-read "vcsh repos: " (vcsh-repos))))
  (magit-vcsh-set-env name
    (magit-status (magit-vcsh-get-worktree name))))

(defun magit-vcsh-set-env-advice (oldfun &rest r)
  (if magit-vcsh-env
      (magit-vcsh-set-env nil (apply oldfun r))
    (apply oldfun r)))

;;; The following advice are needed mostly for function
;;; that create new buffer or that call git.

(advice-add 'magit-git-str :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-string :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-lines :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-items :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-file-status :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-exit-code :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-insert :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-patch-id :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-process-setup :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-process-file :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-start-process :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-apply-patch :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-reset-internal :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-git-command :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-shell-command :around 'magit-vcsh-set-env-advice)
(advice-add 'magit-stash-save :around 'magit-vcsh-set-env-advice)


(provide 'magit-vcsh)

;;; magit-vcsh.el ends here
