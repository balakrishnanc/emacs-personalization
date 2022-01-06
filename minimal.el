;;; minimal.el --- personalization layer defaults for Spacemacs.
;;
;; Copyright (c) 2017-2022 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

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


(defun personalization//minimal-conf--general ()
  "General configuration for a minimal editing environment."
  (setq
   ;; Disable startup screen and message.
   inhibit-startup-screen t
   inhibit-startup-message t
   inhibit-startup-echo-area-message t

   ;; Disable message in scratch buffer.
   initial-scratch-message nil

   ;; Initial buffer.
   initial-buffer-choice nil

   ;; Disable frame title.
   frame-title-format nil

   ;; Disable file dialog.
   use-file-dialog nil

   ;; Disable dialog box.
   use-dialog-box nil

   ;; Disable popup windows.
   pop-up-windows nil

   ;; Disable ugly button for checkboxes.
   widget-image-enable nil

   ;; Disable cursor in inactive windows.
   cursor-in-non-selected-windows nil

   ;; Use `text-mode' as initial mode.
   initial-major-mode 'text-mode

   ;; Text mode is default major mode
   default-major-mode 'text-mode

   ;; Moderate font lock.
   font-lock-maximum-decoration nil

   ;; Disable limit on font lock.
   font-lock-maximum-size nil

   ;; Draw underline at the same place as the descent line.
   x-underline-at-descent-line t

   ;; Use '4' spaces instead of a 'tab' character.
   tab-width 4
   indent-tabs-mode nil

   ;; Constrain column width.
   fill-column 80

   ;; Ensure files always end with a new line.
   require-final-newline t

   ;; Stop emacs from arbitrarily adding lines to the end of a file, when cursor
   ;;  is moved past the end of it.
   next-line-add-newlines nil

   ;; Disable empty line indicators.
   indicate-empty-lines nil

   ;; Show recently opened files in helm.
   helm-ff-file-name-history-use-recentf t

   ;; Configure projectile to use `.projectile' files, if any, in path.
   projectile-indexing-method 'hybrid)

  ;; Disable toolbar, menubar, scrollbar, and tooltips.
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode 0))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode 0))
  (if (fboundp 'tooltip-mode)
      (tooltip-mode 0))

  ;; Navigate windows using `shift+direction'.
  (windmove-default-keybindings)

  ;; "Use utf-8 everywhere."
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  (setup-auto-saves-and-bkups))


(defun personalization//minimal-conf--macos ()
  "MacOS-specific configuration for a minimal editing environment."
  (when (eq system-type 'darwin)
    (progn
      (setq
       ;; Use left-option as `meta', but retain right-option *as such*.
       mac-option-modifier 'meta
       mac-right-option-modifier nil

       ns-use-native-fullscreen t)

      (advice-add 'handle-delete-frame :override
                  #'handle-delete-frame-without-kill-emacs))))


(defun personalization/minimal-conf ()
  "Configure a minimal editing environment."
  (personalization//minimal-conf--general)
  (personalization//minimal-conf--macos))


(provide 'minimal)
