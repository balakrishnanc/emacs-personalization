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
  '(doom-themes
    gnuplot-mode
    goto-last-change
    helm-projectile
    pretty-mode))

(defun personalization/init-doom-themes ()
  (use-package doom-themes
    :config (progn
              (setq doom-themes-enable-bold t
                    doom-themes-enable-italic t)
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

(defun personalization/post-init-helm-projectile ()
  (use-package helm-projectile
    :ensure t
    :init (helm-projectile-on)
    :config (progn
              (setq projectile-switch-project-action 'helm-projectile)
              (defvar helm-source-file-not-found
                (helm-build-dummy-source
                    "Create file"
                  :action 'find-file))
              (add-to-list 'helm-projectile-sources-list
                           helm-source-file-not-found t))))

(defun personalization/init-pretty-mode ()
  (use-package pretty-mode))


;;; packages.el ends here
