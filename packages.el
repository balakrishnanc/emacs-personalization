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

(defconst personalization-packages
  '(all-the-icons
    default-text-scale
    doom-themes
    gnuplot-mode
    goto-last-change
    neotree))

;; List of packages to exclude.
(setq personalization-excluded-packages '())

(defun personalization/init-gnuplot-mode ()
  (use-package gnuplot-mode
    :mode ("\\.gp\\'"
           "\\.gpi\\'"
           "\\.plt\\'"
           "\\.plot\\'")))

(defun personalization/init-goto-last-change ()
  (use-package goto-last-change
    :bind ("C-x C-/" . goto-last-change)))

(defun personalization/init-default-text-scale ()
  (use-package default-text-scale))

(defun personalization/init-all-the-icons ()
  (use-package all-the-icons))

(defun personalization/init-neotree ()
  (use-package neotree
    :config
    (global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(defun personalization/init-doom-themes ()
  (use-package doom-themes
    :config
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)

    (doom-themes-neotree-config)
    (setq doom-neotree-enable-variable-pitch t
          doom-neotree-file-icons 'simple
          doom-neotree-line-spacing 4)
    (doom-themes-org-config)))
