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
    (become . :any)
    (become-user . :any))
  "ansible header arguments")

(defun org-babel-execute:ansible (body params)
  (let* ((inventory (org-babel-ref-resolve
                     (cdr (assoc :inventory params))))
         (inventory-file (org-babel-temp-file "ob-ansible-inventory"))
         (module (or (cdr (assoc :module params)) "command"))
         (hosts (or (cdr (assoc :hosts params)) "all"))
         (forks (cdr (assoc :forks params)))
         (user (cdr (assoc :user params)))
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
                      " --one-line"
                      (format " --args \"%s\"" body)))))
       (with-current-buffer (get-buffer-create "*ansible commands history*")
         (goto-char (point-max))
         (insert (concat cmd "\n")))
       (shell-command-to-string cmd)))))

(provide 'ob-ansible)
;;; ob-ansible.el ends here
