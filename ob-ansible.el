;;; ob-ansible.el --- ansible ad-hoc commands in org-mode babel

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-ansible
;; Version: 0.0.1
;; Keywords: org babel ansible
;; Package-Requires: ((org "8"))
;; Created: 28th Sep 2015

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ansible ad-hoc commands in org-mode babel(or better devops with org-mode)
;;

;;; Code:
(require 'ob)

(defconst org-babel-header-args:ansible
  '((inventory . :any)
    (hosts . :any)
    (forks . :any)
    (user . :any)
    (become . :any)
    (oneline . :any)
    (become-user . :any))
  "ansible header arguments")

(defun org-babel-execute:ansible (body params)
  (let* ((inventory (org-babel-ref-resolve
                     (cdr (assoc :inventory params))))
         (inventory-file (org-babel-temp-file "ob-ansible-inventory"))
         (module (or (cdr (assoc :module params)) "shell"))
         (hosts (or (cdr (assoc :hosts params)) "all"))
         (forks (cdr (assoc :forks params)))
         (user (or (cdr (assoc :user params)) "root"))
         (oneline (assoc :oneline params))
         (become (assoc :become params))
         (become-user (cdr (assoc :become-user params)))
         (playbook (assoc :playbook params))
         (args (concat " -i " inventory-file
                     (when user (format " -u %s" user))
                     (when become " --become")
                     (when become-user (format " --become-user %s" become-user))
                     (when forks (format " -f %s" forks)))))
    (with-temp-file inventory-file (insert inventory))
    (org-babel-chomp
     (let ((cmd
            (if playbook
                (let ((playbook-file (org-babel-temp-file "ob-ansible-playbook")))
                  (with-temp-file playbook-file (insert body))
                  (concat "ansible-playbook" args " " playbook-file))
              (concat "ansible"
                      (format " \"%s\"" hosts)
                      args
                      " --module-name " module
                      (when oneline " --one-line")
                      (format " --args %s" (shell-quote-argument
                                            (org-babel-ansible--preprocess-inline-src body)))))))
       (with-current-buffer (get-buffer-create "*ansible commands history*")
         (goto-char (point-max))
         (insert (concat cmd "\n")))
       (shell-command-to-string cmd)))))

(defun org-babel-ansible--preprocess-inline-src (body)
  (if (string-match "\\(src[ \t]*[:=][ \t]*\\)\\([^ \t]+\\)" body)
      (let ((begin (match-beginning 0))
            (end (match-end 0))
            (assign (match-string 1 body))
            (src (match-string 2 body))
            (resolved))
        (condition-case ex
            (setq resolved (org-babel-ref-resolve src))
          ('error))
        (if resolved
            (let ((tmp (org-babel-temp-file "ob-ansible-file")))
              (with-temp-file tmp (insert resolved))
              (concat (substring body 0 begin) assign tmp (substring body end)))
          body))
    body))

(provide 'ob-ansible)
;;; ob-ansible.el ends here
