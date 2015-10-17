;;; ob-ansible.el --- ansible ad-hoc commands in org-mode babel

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-ansible
;; Version: 0.0.1
;; Keywords: org babel ansible
;; Package-Requires: ((org "8"))
;; Created: 28th Sep 2015

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
    (become . :any))
  "ansible header arguments")

(defun org-babel-execute:ansible (body params)
  (let* ((inventory (org-babel-ref-resolve
                     (cdr (assoc :inventory params))))
         (inventory-file (org-babel-temp-file "ob-ansible-inventory"))
         (module (or (cdr (assoc :module params)) "command"))
         (hosts (or (cdr (assoc :hosts params)) "all"))
         (forks (cdr (assoc :forks params)))
         (user (cdr (assoc :user params)))
         (become (cdr (assoc :become params))))
    (with-temp-file inventory-file
      (insert inventory))
    (org-babel-chomp
     (shell-command-to-string
      (concat "ansible"
              (format " \"%s\"" hosts)
              " -i " inventory-file
              (when user (format " -u %s" user))
              (when become (format " -b %s" become))
              (when forks (format " -f %s" forks))
              " -m " module
              (format " -a \"%s\"" body))))))

(provide 'ob-ansible)
;;; ob-ansible.el ends here
