;;; funcs.el --- personalization layer functions file for Spacemacs.
;;
;; Copyright (c) 2017 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

;; --- Boilerplates. ---
;; --------------------------------------------------

(defun insert-timestamp ()
  "Insert timestamp at point."
  (interactive)
  (insert (format-time-string timestamp-fmt)))

(defun update-file-headers ()
  "Updates the variables in the header."
  (let ((buf-file-name (file-name-nondirectory buffer-file-name))
        (file-mod-ts (format-time-string timestamp-fmt)))
    (save-excursion
      (while (re-search-forward file-name-placeholder nil t)
        (replace-match buf-file-name t nil))
      (goto-char (point-min))
      (while (re-search-forward timestamp-placeholder nil t)
        (replace-match file-mod-ts t nil)))))

(defun update-project-name ()
  "Updates the project name in the header."
  (interactive)
  (let ((proj-name (read-from-minibuffer "Project Name? ")))
    (save-excursion
      (while (re-search-forward proj-name-placeholder nil t)
        (replace-match proj-name t nil)))))

(defun code-template (file-ext)
  "Given file extension provide the template to use."
  (let ((rule (lambda (lang ext)
                (cons (concat "\\." file-ext "$")
                      (vector
                       (concat lang "-template." ext)
                       #'update-file-headers)))))
    (cond ((string= "c"     file-ext) (funcall rule "c" "c"))
          ((string= "cc"    file-ext) (funcall rule "c" "c"))
          ((string= "clj"   file-ext) (funcall rule "clojure" "clj"))
          ((string= "cpp"   file-ext) (funcall rule "c" "c"))
          ((string= "erl"   file-ext) (funcall rule "erlang" "erl"))
          ((string= "go"    file-ext) (funcall rule "go" "go"))
          ((string= "gpi"   file-ext) (funcall rule "gnuplot" "plot"))
          ((string= "h"     file-ext) (funcall rule "c" "c"))
          ((string= "hs"    file-ext) (funcall rule "haskell" "hs"))
          ((string= "java"  file-ext) (funcall rule "java" "java"))
          ((string= "jl"    file-ext) (funcall rule "julia" "jl"))
          ((string= "lisp"  file-ext) (funcall rule "lisp" "lisp"))
          ((string= "pl"    file-ext) (funcall rule "perl" "pl"))
          ((string= "plot"  file-ext) (funcall rule "gnuplot" "plot"))
          ((string= "py"    file-ext) (funcall rule "python" "py"))
          ((string= "scala" file-ext) (funcall rule "scala" "scala"))
          ((string= "scm"   file-ext) (funcall rule "scheme" "scm"))
          ((string= "sh"    file-ext) (funcall rule "bash" "sh"))
          ((string= "sql"   file-ext) (funcall rule "sql" "sql")))))

(defun setup-file-boilerplates ()
  "Setup boilerplates for various files based on extensions."
  (setq-default auto-insert-directory code-templates-dir)
  (setq-default auto-insert-alist
                (cl-loop for i in '("c" "cc" "clj" "cpp" "erl"
                                    "go" "gpi" "h" "hs" "java"
                                    "jl" "lisp" "pl" "plot" "py"
                                    "scala" "scm" "sh" "sql")
                         collecting (code-template i)))
  ;; Automatically insert boilerplates.
  (add-hook 'find-file-hooks 'auto-insert))


;; --- Enable easy editing. ---
;; --------------------------------------------------

(defun indent-on-paste ()
  "Indent text on paste."
  (dolist (command '(yank yank-pop))
    (eval `(defadvice ,command (after indent-region activate)
             (and (not current-prefix-arg)
                  (member major-mode
                          '(emacs-lisp-mode
                            lisp-mode
                            clojure-mode
                            go-mode
                            ruby-mode
                            latex-mode
                            c-mode
                            c++-mode
                            ;; indent-on-paste sucks in the following modes.
                            ;; python-mode
                            ;; haskell-mode
                            ))
                  (let ((mark-even-if-inactive transient-mark-mode))
                    (indent-region (region-beginning) (region-end) nil)))))))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

;; --- Auto saves and backups. ---
;; --------------------------------------------------

(defun make-auto-save-file-name ()
  (if buffer-file-name
      (concat auto-saves-dir
              auto-save-file-name-prefix
              (file-name-nondirectory buffer-file-name)
              "-"
              system-name)
    (expand-file-name
     (concat auto-saves-dir
             auto-save-file-name-prefix
             (buffer-name)
             "-"
             system-name))))

(defun make-dir (dir-path)
  "Create directory, if it does not exist."
  (if (not (file-exists-p dir-path))
      (make-directory dir-path t)))

(defadvice auto-save-mode (around auto-saves-dir)
  "Use a standard location for auto-save files for non-file buffers."
  (if (not buffer-file-name)
      (let ((default-directory auto-saves-dir))
        ad-do-it)
    ad-do-it))

(defun setup-auto-saves-and-bkups ()
  "Setup auto-saves and auto-backups."
  (make-dir auto-saves-dir)
  (setq-default auto-save-list-file-prefix auto-saves-dir)
  (setq-default auto-save-file-name-transforms
                `((".*" ,auto-saves-dir t)))
  (make-dir auto-bkups-dir)
  (setq-default backup-directory-alist
                `((".*" . ,auto-bkups-dir)))
  (ad-activate 'auto-save-mode)
  (setq auto-save-interval 30)

  ;; Save when focus is lost.
  (add-hook 'focus-out-hook
            (defun save-all-unsaved-buffers ()
              (interactive)
              (save-some-buffers t))))

(defun recover-this-buffer ()
  "Recover current buffer from auto-save file, if any."
  (interactive)
  (if (buffer-file-name)
      (recover-this-file)
    (recover-file (buffer-name))))


;; --- Distraction free writing. ---
;; --------------------------------------------------

(defun disable-distraction-free-writing ()
  "Disable distraction-free-writing mode."
  (progn
    (olivetti-mode -1)
    ;; Disable visual line mode.
    (visual-line-mode -1)
    ;; NOTE: `visual-line-mode' affects `truncate-lines' and `word-wrap'.
    ;; Enable `truncate-lines' feature.
    (toggle-truncate-lines 1)
    ;; Disable centered cursor mode.
    (centered-cursor-mode -1)
    ;; Show mode line.
    (hidden-mode-line-mode -1)
    (message "distraction-free-writing mode disabled.")
    (put 'toggle-distraction-free-writing 'state nil)))

(defun enable-distraction-free-writing ()
  "Enable distraction-free-writing mode."
  (progn
    (olivetti-set-width 81)
    (olivetti-mode 1)
    (spacemacs/toggle-vi-tilde-fringe-off)
    ;; Enable visual line mode.
    (visual-line-mode 1)
    ;; Disable `truncate-lines' feature.
    (toggle-truncate-lines -1)
    ;; Keep text being edited in the center of the screen.
    (centered-cursor-mode 1)
    ;; Hide mode line.
    (hidden-mode-line-mode 1)
    (message "distraction-free-writing mode enabled.")
    (put 'toggle-distraction-free-writing 'state t)))

(defun toggle-distraction-free-writing (&optional arg)
  "Toggle distraction-free writing."
  (interactive "p")
  (if (get 'toggle-distraction-free-writing 'state)
      (disable-distraction-free-writing)
    (enable-distraction-free-writing)))


;; --- Customize look and feel. ---
;; --------------------------------------------------

(defun customize-look-and-feel ()
  "Customize how the editor frame or window looks and feels."
  ;; Increase space between text and the border or fringe.
  (add-to-list 'default-frame-alist '(internal-border-width . 20))

  (global-prettify-symbols-mode t)

  ;; Highlight current line.
  (global-hl-line-mode -1))


;; --- Customize modes.
;; --------------------------------------------------

(defun setup-lisp-mode ()
  "Customize `lisp-mode'."
  (add-hook 'lisp-mode-hook
            (lambda ()
              (progn
                (let ((slime-config (concat (file-name-as-directory ext-config-dir)
                                            (file-name-as-directory "quicklisp")
                                            "slime-helper.el")))
                  (when (file-exists-p slime-config)
                    (load slime-config)))

                (let ((sbcl-bin "/usr/local/bin/sbcl"))
                  (when (and (file-exists-p sbcl-bin)
                             (file-executable-p sbcl-bin))
                    (setq inferior-lisp-program sbcl-bin)))

                ;; ;; Minor mode to keep S-expressions safely balanced.
                ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-common-lisp-mode)

                (pretty-mode)))))

(defun setup-programming-mode ()
  "Add customizations to setup a development environment."
  ;; Turn on support for boilerplates.
  (setup-file-boilerplates)

  ;; Tack on customizations to the `prog-mode-hook'.
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Turn off line-truncation; wrap lines around.
              (toggle-truncate-lines -1)
              (pretty-mode)))

  ;; Set execute permissions automatically when saving scripts.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  (setup-lisp-mode))

(defun can-use-font? (font-name)
  "Returns 't' only if emacs can recognize the font face."
  (if (x-list-fonts font-name)
      t
    nil))

(defun fmt-org-bullets ()
  "Customize bullets in `org-mode'"
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([+]\\) "
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1)
                                                  (match-end 1)
                                                  "■"))))
                          ("^ *\\([-]\\) "
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1)
                                                  (match-end 1)
                                                  "•")))))))

(defun fmt-org-mode-style ()
  "Customize `org-mode' look and feel."
  (let* ((base-font-color (face-foreground 'default nil 'default))
             (heading-font    `(:font ,serif-font-face))
             (heading-attrs   `(:inherit default
                                         :weight bold)))

        (custom-theme-set-faces
         'user
         `(org-block ((t (:inherit fixed-pitch))))
         `(org-block-begin-line ((t (:inherit fixed-pitch))))
         `(org-block-end-line ((t (:inherit org-block-begin-line))))
         `(org-code ((t (:inherit (shadow fixed-pitch)))))
         `(org-document-info ((t (:foreground "#fb5607"))))
         `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
         `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
         `(org-link ((t (:foreground "#3a86ff" :underline t))))
         `(org-list-dt ((t (:inherit variable-pitch :height 0.7))))
         `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         `(org-property-value ((t (:inherit fixed-pitch))) t)
         `(org-special-keyword ((t (:inherit (font-lock-comment-face
                                              fixed-pitch)))))
         `(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
         `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
         `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
         `(org-document-title
           ((t (,@heading-attrs ,@heading-font
                                :height 1.5 :underline nil))))
         `(org-level-8   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-7   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-6   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-5   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-4   ((t (,@heading-attrs ,@heading-font :height 1.1))))
         `(org-level-3   ((t (,@heading-attrs ,@heading-font :height 1.2))))
         `(org-level-2   ((t (,@heading-attrs ,@heading-font :height 1.3))))
         `(org-level-1   ((t (,@heading-attrs ,@heading-font :height 1.4)))))))

(defun setup-org-mode ()
  "Customize `org-mode'."
  (with-eval-after-load 'org
    (progn
      (setq
       org-fontify-whole-heading-line t
       org-fontify-done-headline t
       org-fontify-emphasized-text t
       org-fontify-quote-and-verse-blocks t
       org-src-fontify-natively t
       org-hide-emphasis-markers t
       org-hide-leading-stars t
       org-indent-mode t
       org-pretty-entities t
       org-use-sub-superscripts "{}"
       org-startup-indented t
       spaceline-org-clock-p t)
      (fmt-org-bullets)
      (fmt-org-mode-style)
      (evil-leader/set-key "t ;" 'org-toggle-narrow-to-subtree)))

  (add-hook 'org-mode-hook
            (lambda ()
              (progn
                ;; For `org-mode' to _safely_ store notes.
                (auto-save-mode 1)
                ;; For customizing look and feel.
                (variable-pitch-mode 1)
                (enable-distraction-free-writing)))))

(defun setup-latex-mode ()
  "Customize `latex-mode'."
  (with-eval-after-load 'latex
    (progn
      (setq-default TeX-master nil)
      (custom-theme-set-faces
       'user
       `(font-latex-math-face ((t (:inherit (shadow fixed-pitch) :height 0.9))))
       `(font-lock-function-name-face ((t (:inherit fixed-pitch :height 1.0))))
       `(font-lock-constant-face ((t (:inherit fixed-pitch :height 1.0 :weight bold))))
       `(font-latex-sedate-face ((t (:inherit fixed-pitch :height 1.0))))
       `(font-latex-sectioning-2-face ((t (:inherit variable-pitch :height 1.0 :weight bold)))))))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (progn
                (variable-pitch-mode 1)
                (enable-distraction-free-writing))))

  ;; Perform full-document previews.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(defun setup-text-mode ()
  "Customize `text-mode'."
  (add-hook 'text-mode-hook
            (lambda ()
              (progn
                (variable-pitch-mode 1)
                (enable-distraction-free-writing)))))

(defun setup-markdown-mode ()
  "Customize `markdown-mode'."
  (with-eval-after-load 'markdown
    (progn
      (custom-theme-set-faces
       'user
       `(markdown-inline-code-face ((t (:inherit
                                        (markdown-code-face
                                         markdown-pre-face
                                         shadow
                                         fixed-pitch) :height 1.0)))))))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (progn
                (variable-pitch-mode 1)
                (typo-mode -1)
                (enable-distraction-free-writing)))))

(defun customize-modes ()
  "Customize different modes."
  (custom-theme-set-faces
    'user
    ;; `(default ((t (:family "Source Code Pro"
    ;;                        :slant normal
    ;;                        :height 130))))
    `(variable-pitch ((t (:family ,serif-font-face
                                  :height 1.4
                                  :weight normal))))
    `(fixed-pitch ((t (:family ,monospace-font-face
                              :slant normal
                              :height 0.9)))))
  (setup-programming-mode)
  (setup-org-mode)
  (setup-latex-mode)
  (setup-text-mode)
  (setup-markdown-mode))


;; --- Common editor preferences. ---
;; --------------------------------------------------

(defun handle-delete-frame-without-kill-emacs (event)
  "Handle delete-frame events from the X server."
  (interactive "e")
  (let ((frame (posn-window (event-start event)))
        (i 0)
        (tail (frame-list)))
    (while tail
      (and (frame-visible-p (car tail))
           (not (eq (car tail) frame))
           (setq i (1+ i)))
      (setq tail (cdr tail)))
    (if (> i 0)
        (delete-frame frame t)
      ;; Not (save-buffers-kill-emacs) but instead:
      (ns-do-hide-emacs))))

(defun use-utf-8-everywhere ()
  "Use utf-8 everywhere."
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8))

(defun customize-editor-behavior ()
  "Customize common editor behavior."
  ;; Start server.
  (setq-default dotspacemacs-enable-server t)

  (use-utf-8-everywhere)

  (setup-auto-saves-and-bkups)

  ;; Use '4' spaces instead of a 'tab' character.
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; By default, set column width to '80' characters.
  (setq-default fill-column 80)

  ;; Ensure files always end with a new line.
  (setq-default require-final-newline t)

  ;; Stop emacs from arbitrarily adding lines to the end of a file,
  ;;  when cursor is moved past the end of it.
  (setq-default next-line-add-newlines nil)

  ;; Show recently opened files in helm.
  (setq-default helm-ff-file-name-history-use-recentf t)

  ;; Configure projectile to use `.projectile' files, if any, in the project directory.
  (setq projectile-indexing-method 'hybrid)

  (when (eq system-type 'darwin)
    (advice-add 'handle-delete-frame :override
                #'handle-delete-frame-without-kill-emacs)))


;;; funcs.el ends here
