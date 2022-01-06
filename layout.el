;;; layout.el --- personalization layer layout file for Spacemacs.
;;
;; Copyright (c) 2017-2022 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

(defun personalization/init-layout ()
  "Initialize layout."
  (setq default-frame-alist
        (append (list
	               '(min-height . 1)
                 '(height     . 45)
	               '(min-width  . 1)
                 '(width      . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe    . 1)
                 '(right-fringe   . 1)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0)))))


(provide 'layout)
