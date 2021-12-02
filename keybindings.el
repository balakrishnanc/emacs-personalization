;;; keybindings.el --- personalization layer key bindings file for Spacemacs.
;;
;; Copyright (c) 2017-2021 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

;; Fill-up text based on column width.
(global-set-key (kbd "C-'") 'fill-region)

;; Align using regular-expression
(global-set-key (kbd "C-M-'") 'align-regexp)

;; Increase or decrease text size.
(global-set-key (kbd "C-M-+") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)

;; Comment or Uncomment region in buffer.
(global-set-key (kbd "C-M-]") 'comment-region)
(global-set-key (kbd "C-M-[") 'uncomment-region)

(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; Insert timestamp at point.
(global-set-key '[f5] 'insert-timestamp)

;; Recover current file-visiting or non-file-visiting buffer.
(global-set-key (kbd "C-`") 'recover-this-buffer)

;; Enable distraction-free writing mode.
(global-set-key '[f12] 'toggle-distraction-free-writing)

;;; keybindings.el ends here
