;;; packages.el --- personalization layer packages file for Spacemacs.
;;
;; Copyright (c) 2017 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `personalization-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `personalization/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `personalization/pre-init-PACKAGE' and/or
;;   `personalization/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst personalization-packages
  '(all-the-icons
    avy
    default-text-scale
    doom-themes
    gnuplot-mode
    goto-last-change
    neotree
    pretty-mode))

(defun personalization/init-all-the-icons ()
  (use-package all-the-icons))

(defun personalization/post-init-avy ()
  (use-package avy
    :bind (("C-:" . avy-goto-char)
           ("M-g g" . avy-goto-line))))

(defun personalization/init-default-text-scale ()
  (use-package default-text-scale
    :bind (("C-M-+" . default-text-scale-increase)
           ("C-M--" . default-text-scale-decrease))))

(defun personalization/init-doom-themes ()
  (use-package doom-themes
    :after (neotree org)
    :config (progn
              (setq doom-themes-enable-bold t
                    doom-themes-enable-italic t)
              (doom-themes-neotree-config)
              (setq doom-neotree-enable-variable-pitch t
                    doom-neotree-file-icons 'simple
                    doom-neotree-line-spacing 4)
              (doom-themes-org-config))))

(defun personalization/init-gnuplot-mode ()
  (use-package gnuplot-mode
    :mode ("\\.gp\\'"
           "\\.gpi\\'"
           "\\.plt\\'"
           "\\.plot\\'")))

(defun personalization/init-goto-last-change ()
  (use-package goto-last-change
    :bind ("C-x C-/" . goto-last-change)))

(defun personalization/post-init-neotree ()
  (use-package neotree
    :bind ([f8] . neotree-toggle)
    :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

(defun personalization/init-pretty-mode ()
  (use-package pretty-mode
    :config (global-pretty-mode t)))


;;; packages.el ends here
