;;; funcs.el --- personalization layer functions file for Spacemacs.
;;
;; Copyright (c) 2017-2022 Balakrishnan Chandrasekaran
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
    ;; Warning: indent-on-paste sucks in the following modes.
    ;; python-mode
    ;; haskell-mode
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
    ;; Show mode line.
    (hidden-mode-line-mode -1)
    (message "distraction-free-writing mode disabled.")
    (put 'toggle-distraction-free-writing 'state nil)))

(defun enable-distraction-free-writing ()
  "Enable distraction-free-writing mode."
  (progn
    (olivetti-set-width 91)
    (olivetti-mode 1)
    ;; Enable visual line mode.
    (visual-line-mode 1)
    ;; Disable `truncate-lines' feature.
    (toggle-truncate-lines -1)
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
  (defun color-of (attr colors)
    (cdr (assq attr colors)))

  (setq theming-modifications
        `((modus-vivendi
           (deft-title-face
            :foreground ,(color-of 'fg-special-mild
                                   modus-themes-vivendi-colors))
           (deft-summary-face
            :foreground ,(color-of 'bg-special-warm
                                   modus-themes-vivendi-colors))
           (deft-time-face
            :height 0.8)
           (org-level-1
            :overline ,(color-of 'magenta-alt
                                 modus-themes-vivendi-colors)
            :background ,(color-of 'bg-alt
                                   modus-themes-vivendi-colors))
           (org-level-2
            :overline ,(color-of 'magenta-alt-other
                                 modus-themes-vivendi-colors)
            :background ,(color-of 'bg-alt
                                   modus-themes-vivendi-colors))
           (org-document-title :height 1.1))
          (modus-operandi
           (deft-title-face
            :foreground ,(color-of 'fg-special-mild
                                   modus-themes-operandi-colors))
           (deft-summary-face
            :foreground ,(color-of 'bg-special-warm
                                   modus-themes-operandi-colors))
           (deft-time-face
            :height 0.8)
           (org-level-1
            :overline ,(color-of 'magenta-alt
                                 modus-themes-operandi-colors)
            :background ,(color-of 'bg-alt
                                   modus-themes-operandi-colors))
           (org-level-2
            :overline ,(color-of 'magenta-alt-other
                                 modus-themes-operandi-colors)
            :background ,(color-of 'bg-alt
                                   modus-themes-operandi-colors))
           (org-document-title :height 1.1)))))


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
      (evil-leader/set-key "t ;" 'org-toggle-narrow-to-subtree))

    (add-hook 'org-mode-hook
              (lambda ()
                (progn
                  ;; For `org-mode' to _safely_ store notes.
                  (auto-save-mode 1)
                  ;; For customizing look and feel.
                  (enable-distraction-free-writing))))))

(defun setup-latex-mode ()
  "Customize `latex-mode'."
  (with-eval-after-load 'latex
    (progn
      (setq-default TeX-master nil)))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (progn
                (enable-distraction-free-writing))))

  ;; Perform full-document previews.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(defun setup-text-mode ()
  "Customize `text-mode'."
  (add-hook 'text-mode-hook
            (lambda ()
              (progn
                (enable-distraction-free-writing)))))

(defun setup-markdown-mode ()
  "Customize `markdown-mode'."
  (add-hook 'markdown-mode-hook
            (lambda ()
              (progn
                (enable-distraction-free-writing)))))

(defun customize-modes ()
  "Customize different modes."
  (setup-programming-mode)
  (setup-org-mode)
  (setup-latex-mode)
  (setup-text-mode)
  (setup-markdown-mode))

(defun personalization/config ()
  "Customize the Emacs environment."
  (add-to-list 'load-path personalization-base-dir)

  (require 'layout)
  (personalization/init-layout)
  (require 'minimal)
  (personalization/minimal-conf)
  (customize-look-and-feel)
  (customize-modes))

;;; funcs.el ends here
