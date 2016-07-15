;;; packages.el --- personalization Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq personalization-packages
      '(gnuplot-mode
        goto-last-change))

;; List of packages to exclude.
(setq personalization-excluded-packages '())

(defun personalization/init-goto-last-change ()
  (use-package goto-last-change
    :bind ("C-x C-/" . goto-last-change)))

(defun personalization/init-gnuplot-mode ()
  (use-package gnuplot-mode
    :mode ("\\.gpi\\'" "\\.plt\\'" "\\.plot\\'")))

(defun personalization/init-sublime-themes ()
  nil)
