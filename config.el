;;; config.el --- personalization layer configuration file for Spacemacs.
;;
;; Copyright (c) 2017-2022 Balakrishnan Chandrasekaran
;;
;; Author: Balakrishnan Chandrasekaran <balakrishnan.c@gmail.com>
;; URL: https://github.com/balakrishnanc/emacs-personalization
;;
;;; License: MIT

;;; Commentary:

;;; Code:

(defconst personalization-base-dir
  (file-name-directory load-file-name)
  "Package directory.")

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

(defconst custom-file
  (concat (file-name-as-directory
           (expand-file-name "~/.emacs.d")) "custom.el")
  "File for storing auto-generated custom variable definitions.")

(defconst opam-user-setup-file
  (concat
   (file-name-as-directory (expand-file-name "~/.emacs.d"))
   "opam-user-setup.el")
  "Auto-generated opam user-setup configuration file.")

(defconst fixed-pitch-font-face
  "FiraCode Nerd Font"
  "Primary font face, for fixed-width text.")

(defconst variable-pitch-font-face
  "iA Writer Quattro V" ; "iA Writer Mono V"
  "Proportional font for variable-pitch mode.")

;;; config.el ends here
