;;; packages.el --- personalization layer packages file for Spacemacs.
;;
;; Copyright (c) 2017-2021 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;; Simple customizations.

;;; Code:

(defconst personalization-packages
  '((bespoke-themes
     :location (recipe
                :fetcher github
                :repo "mclear-tools/bespoke-themes"))
    deft
    doom-themes
    gnuplot-mode
    goto-last-change
    helm-projectile
    modus-themes
    olivetti
    org-variable-pitch
    pretty-mode)
  "Packages required by the personalization layer.")

(defun personalization/init-bespoke-themes ()
  (use-package bespoke-themes
    :config (progn
              (setq
               bespoke-set-evil-cursors t
               bespoke-set-italic-comments t
               bespoke-set-italic-keywords t
               bespoke-set-variable-pitch t)
              (load-theme 'bespoke t))))

(defun personalization/post-init-deft ()
  (use-package deft
    :config
    (setq
     deft-directory "~/Documents/Shire/research/notes"
     deft-time-format "%H:%M %b. %d, %Y"
     deft-extensions '("org" "md" "txt")
     deft-use-filename-as-title nil
     deft-use-filter-string-for-filename t
     deft-file-naming-rules '((noslash . ".")
                              (nospace . "-")
                              (case-fn . downcase))
     deft-markdown-mode-title-level 1
     deft-org-mode-title-prefix t
     deft-strip-summary-regexp (concat
                                "\\("
                                ;; blank
                                "[\n\t]"
                                ;; Titles
                                "\\|\\*.*$"
                                ;; org-mode metadata
                                "\\|^#\\+[[:upper:]_]+:.*$"
                                ;; Reference metadata
                                "\\|^#\\+begin_example.*$"
                                "\\|^#\\+end_example.*$"
                                ;; Bibtex
                                "\\|^@.*$"
                                "\\|.*[[:alpha:]].*=.*"
                                "\\)")
     deft-separator "\n")))

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

(defun personalization/post-init-modus-themes ()
  (use-package modus-themes
    :ensure t
    :init (progn
            (setq modus-themes-italic-constructs t
                  modus-themes-bold-constructs nil
                  modus-themes-region '(bg-only no-extend))
            ;; Load the theme files before enabling a theme (else you get an error).
            (modus-themes-load-themes))
    :config
    (let* ((hr (nth 2 (decode-time (current-time)))))
      ;; Decide on a theme based on the hour of the day.
      (if (and (>= hr 8) (<= hr 18))
          (modus-themes-load-operandi)
        (modus-themes-load-vivendi)))
    (setq modus-themes-intense-markup t
          modus-themes-italic-constructs t
          modus-themes-mixed-fonts t
          modus-themes-scale-headings t
          modus-themes-variable-pitch-headings t)
    :bind ("<f6>" . modus-themes-toggle)))

(defun personalization/init-olivetti ()
  (use-package olivetti))

(defun personalization/init-org-variable-pitch ()
  (use-package org-variable-pitch
    :config
    (add-hook 'org-mode-hook
              'org-variable-pitch-minor-mode)))

(defun personalization/init-pretty-mode ()
  (use-package pretty-mode))

;;; packages.el ends here
