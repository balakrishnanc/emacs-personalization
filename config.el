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

(defconst emacs-config-dir
  (expand-file-name "~/.emacs.d")
  "Path to directory containing spacemacs or emacs configuration.")

(defconst code-templates-dir
  (expand-file-name "~/.code_templates")
  "Path to directory containing boilerplates.")

(defconst ext-config-dir
  (expand-file-name "~/.config")
  "Path to directory containing configurations for tools and utilities.")

(defconst proj-name-placeholder "__PROJECT__"
  "String denoting the location of project name in boilerplates.")

(defconst file-name-placeholder "__FILE_NAME__"
  "String denoting the location of file name in boilerplates.")

(defconst timestamp-placeholder "__TS__"
  "String denoting the location of timestamp in boilerplates.")

(defconst timestamp-fmt "%Y-%m-%d %R %z"
  "Format for timestamps in file boilerplates.")

(defconst auto-saves-dir
  (concat (file-name-as-directory emacs-config-dir)
          (file-name-as-directory "auto-saves"))
  "Absolute path where automatic saves are stored.")

(defconst auto-bkups-dir
  (concat (file-name-as-directory emacs-config-dir)
          (file-name-as-directory "auto-backups"))
  "Absolute path where automatic backups are stored.")

(defconst auto-save-file-name-prefix ".save_"
  "Prefix for naming auto-save file names.")

(defconst base-font-face "SauceCodePro Nerd Font" ; "Source Code Pro"
  "Default font family.
Check the font name with function `can-use-font?' to avoid errors.")

(defconst serif-font-face "SauceCodePro Nerd Font" ; "Crimson Pro"
  "Default _serif_ font family for `org-mode', `text-mode', and others.
Check the font name with function `can-use-font?' to avoid errors.")

(defconst monospace-font-face "SauceCodePro Nerd Font" ; "Source Code Pro"
  "Default _monospace_ font family for `org-mode', `text-mode', and others.
Check the font name with function `can-use-font?' to avoid errors.")

(defconst custom-file
  (concat (file-name-as-directory
           (expand-file-name "~/.emacs.d")) "custom.el")
  "File for storing auto-generated custom variable definitions.")

(defconst opam-user-setup-file
  (concat
   (file-name-as-directory (expand-file-name "~/.emacs.d"))
   "opam-user-setup.el")
  "Auto-generated opam user-setup configuration file.")

;; --- Run all customizations. ---
;; --------------------------------------------------
(customize-modes)
(customize-editor-behavior)
(customize-look-and-feel)


;;; config.el ends here
