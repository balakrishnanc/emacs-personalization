;;; config.el --- personalization layer configuration file for Spacemacs.
;;
;; Copyright (c) 2017 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

(defvar emacs-config-dir
  (expand-file-name "~/.emacs.d")
  "Path to directory containing spacemacs or emacs configuration.")

(defvar code-templates-dir
  (expand-file-name "~/.code_templates")
  "Path to directory containing boilerplates.")

(defvar ext-config-dir
  (expand-file-name "~/.config")
  "Path to directory containing configurations for tools and utilities.")

(defvar proj-name-placeholder "__PROJECT__"
  "String denoting the location of project name in boilerplates.")

(defvar file-name-placeholder "__FILE_NAME__"
  "String denoting the location of file name in boilerplates.")

(defvar timestamp-placeholder "__TS__"
  "String denoting the location of timestamp in boilerplates.")

(defvar timestamp-fmt "%Y-%m-%d %R %z"
  "Format for timestamps in file boilerplates.")

(defvar auto-saves-dir
  (concat (file-name-as-directory emacs-config-dir)
          (file-name-as-directory "auto-saves"))
  "Absolute path where automatic saves are stored.")

(defvar auto-bkups-dir
  (concat (file-name-as-directory emacs-config-dir)
          (file-name-as-directory "auto-backups"))
  "Absolute path where automatic backups are stored.")

(defvar auto-save-file-name-prefix ".save_"
  "Prefix for naming auto-save file names.")

(defvar serif-font-face "Courier New" ; "Charter" ; "Literata"
  "Default _serif_ font family for `org-mode', `text-mode', and others.
Check the font name with function `can-use-font?' to avoid errors.")

(defvar monospace-font-face "Fira Code"
  "Default _monospace_ font family for `org-mode', `text-mode', and others.
Check the font name with function `can-use-font?' to avoid errors.")


;; --- Run all customizations. ---
;; --------------------------------------------------
(customize-modes)
(customize-editor-behavior)
(customize-look-and-feel)


;;; config.el ends here
