;;; config.el --- personalization Configuration File for Spacemacs
;;
;; Copyright (c) 2015 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;; ---( Variables )---
;; `__

(defvar code-templates-dir
  (expand-file-name "~/.code_templates"))

(defvar emacs-config-dir
  (expand-file-name "~/.emacs.d"))

(defvar auto-saves-dir
  (concat temporary-file-directory
          (file-name-as-directory (user-login-name))
          (file-name-as-directory "emacs")
          (file-name-as-directory "auto-saves")))

(defvar auto-bkups-dir
  (concat temporary-file-directory
          (file-name-as-directory (user-login-name))
          (file-name-as-directory "emacs")
          (file-name-as-directory "auto-backups")))

;; ---( Auto-save and Auto-backup )---
;; `__

(defun make-auto-save-file-name ()
  (if buffer-file-name
      (concat auto-saves-dir
              ".save_"
              (file-name-nondirectory buffer-file-name)
              "-"
              system-name)
    (expand-file-name
     (concat auto-saves-dir
             ".save_"
             (buffer-name)
             "-"
             system-name))))

(defun make-dir (dir-path)
  "Create directory if it does not exist already."
  (if (not (file-exists-p dir-path))
      (make-directory dir-path t)))

(defun setup-auto-saves-and-bkups ()
  "Setup auto-saves and auto-backups."
  (make-dir auto-saves-dir)
  (setq-default auto-save-list-file-prefix
                (concat auto-saves-dir ".as-"))
  (setq-default auto-save-file-name-transforms
                `((".*" ,auto-saves-dir t)))
  (make-dir auto-bkups-dir)
  (setq-default backup-directory-alist
                `((".*" . ,auto-bkups-dir))))

;; ---( utility functions )---
;; `__

(defun insert-timestamp ()
  "Insert timestamp at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %R %z")))

(defun update-file-headers ()
  "Updates the variables in the header."
  (let ((buf-file-name (file-name-nondirectory buffer-file-name))
        (file-mod-ts (format-time-string "%Y-%m-%d %R %z")))
    (save-excursion
        (while (re-search-forward "__FILE_NAME__" nil t)
          (replace-match buf-file-name t nil))
        (goto-char (point-min))
        (while (re-search-forward "__TS__" nil t)
          (replace-match file-mod-ts t nil)))))

(defun update-project-name ()
  "Updates the project name in the header."
  (interactive)
  (let ((proj-name (read-from-minibuffer "Project Name? ")))
    (save-excursion
      (while (re-search-forward "__PROJECT__" nil t)
        (replace-match proj-name t nil)))))

(defun indent-on-paste ()
  "Indent text on paste"
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

(defun code-template (file-ext)
  "Given file extension provide the template to use."
  (let ((rule (lambda (lang ext)
                (cons (concat "\\." file-ext "$")
                      (vector
                       (concat lang "-template." ext)
                       #'update-file-headers)))))
    (cond ((string= "c" file-ext) (funcall rule "c" "c"))
          ((string= "cc" file-ext) (funcall rule "c" "c"))
          ((string= "clj" file-ext) (funcall rule "clojure" "clj"))
          ((string= "cpp" file-ext) (funcall rule "c" "c"))
          ((string= "erl" file-ext) (funcall rule "erlang" "erl"))
          ((string= "go" file-ext) (funcall rule "go" "go"))
          ((string= "gpi" file-ext) (funcall rule "gnuplot" "plot"))
          ((string= "h" file-ext) (funcall rule "c" "c"))
          ((string= "hs" file-ext) (funcall rule "haskell" "hs"))
          ((string= "java" file-ext) (funcall rule "java" "java"))
          ((string= "jl" file-ext) (funcall rule "julia" "jl"))
          ((string= "lisp" file-ext) (funcall rule "lisp" "lisp"))
          ((string= "pl" file-ext) (funcall rule "perl" "pl"))
          ((string= "plot" file-ext) (funcall rule "gnuplot" "plot"))
          ((string= "py" file-ext) (funcall rule "python" "py"))
          ((string= "scala" file-ext) (funcall rule "scala" "scala"))
          ((string= "scm" file-ext) (funcall rule "scehem" "scm"))
          ((string= "sh" file-ext) (funcall rule "bash" "sh"))
          ((string= "sql" file-ext) (funcall rule "sql" "sql")))))

(defun personalize ()
  "Load custom configuration."
  (setup-auto-saves-and-bkups)
  (setup-appearance)
  (setup-key-bindings)
  (setup-edit-prefs))

(defun setup-appearance ()
  "Customize spacemacs appearance."

  ;; Show line numbers in the left gutter (*always*)
  ;; (global-linum-mode t)

  ;; Highlight current line.
  (global-hl-line-mode -1)
  )

(defun setup-key-bindings ()
  "Setup additional keybindings to packages."

  ;; Fill-up text based on column width.
  (global-set-key (kbd "C-'") 'fill-region)

  ;; Align using regular-expression
  (global-set-key (kbd "C-M-'") 'align-regexp)

  ;; Comment or Uncomment region in buffer.
  (global-set-key (kbd "C-M-]") 'comment-region)
  (global-set-key (kbd "C-M-[") 'uncomment-region)

  ;; Comment or Uncomment region in buffer.
  (global-set-key (kbd "C-M-+") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease)

  (global-set-key (kbd "C-k") 'kill-and-join-forward)

  ;; Insert timestamp at point.
  (global-set-key '[f5] 'insert-timestamp)
  )

(defun setup-edit-prefs ()
  "Setup editor preferences for coding."

  ;; Use '4' spaces instead of a 'tab' character.
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; By default, set column width to '80' characters.
  (setq-default fill-column 80)

  ;; Ensure files always end with a new line.
  (setq require-final-newline t)

  ;; Stop emacs from arbitrarily adding lines to the end of a file,
  ;;  when cursor is moved past the end of it.
  (setq next-line-add-newlines nil)

  ;; Set execute permissions automatically when saving scripts.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; Automatically insert boilerplates.
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-directory code-templates-dir)
  (setq auto-insert-alist
        (loop for i in '("c" "cc" "clj" "cpp" "erl"
                         "go" "gpi" "h" "hs" "java"
                         "jl" "lisp" "pl" "plot" "py"
                         "scala" "scm" "sh" "sql")
              collecting (code-template i)))
)

(personalize)
