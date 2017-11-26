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
  (concat (file-name-as-directory emacs-config-dir)
          (file-name-as-directory "auto-saves")))

(defvar auto-bkups-dir
  (concat (file-name-as-directory emacs-config-dir)
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
  (setq auto-save-interval 30))

(defun recover-this-buffer ()
  "Recover current buffer from auto-save file, if any."
  (interactive)
  (if (buffer-file-name)
      (recover-this-file)
    (recover-file (buffer-name))))

;; ---( Utility functions )---
;; `__

(defun insert-timestamp ()
  "Insert timestamp at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %R %z")))

(defun enable-distraction-free-writing ()
  "Turn off line-wrapping feature."
  (interactive)
  (toggle-truncate-lines -1)
  (visual-line-mode nil)
  (vi-tilde-fringe-mode -1))

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
          ((string= "scm"   file-ext) (funcall rule "scehem" "scm"))
          ((string= "sh"    file-ext) (funcall rule "bash" "sh"))
          ((string= "sql"   file-ext) (funcall rule "sql" "sql")))))

;; ---( Font setup: Fira Code Symbol )---
;; Ref: https://gist.github.com/mordocai/50783defab3c3d1650e068b4d1c91495
;; `__

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region
                    (match-beginning 1)
                    (match-end 1)
                    ;; The first argument to `concat' is a string containing a
                    ;;  literal tab.
                    ,(concat "	" (list
                                   (decode-char 'ucs
                                                (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ("\\(&&\\)"                    #Xe131)
            ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;;("\\(x\\)"                     #Xe16b)
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(defun setup-fira-code-font ()
  "Setup `Fira Code Symbol' font."

  ;; This works when using emacs --daemon + emacsclient.
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  ;; This works when using emacs without server/client.
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords))

;; ---( Customizations )---
;; `__

(defun setup-appearance ()
  "Customize spacemacs appearance."

  (if window-system
      (progn
        (global-prettify-symbols-mode +1)))

  ;; Highlight current line.
  (global-hl-line-mode -1)

  ;; Increase space between text and the border or fringe.
  (add-to-list 'default-frame-alist '(internal-border-width . 20)))

(defun setup-common-key-bindings ()
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

  ;; Avy.
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "M-g g") 'avy-goto-line)

  (global-set-key (kbd "C-k") 'kill-and-join-forward)

  ;; Insert timestamp at point.
  (global-set-key '[f5] 'insert-timestamp)

  ;; Recover current buffer.
  (global-set-key (kbd "C-`") 'recover-this-buffer))

(defun setup-file-templates ()
  "Setup templates for various files based on extensions."
  (setq-default auto-insert-directory code-templates-dir)
  (setq-default auto-insert-alist
                (loop for i in '("c" "cc" "clj" "cpp" "erl"
                                 "go" "gpi" "h" "hs" "java"
                                 "jl" "lisp" "pl" "plot" "py"
                                 "scala" "scm" "sh" "sql")
                      collecting (code-template i)))
  ;; Automatically insert boilerplates.
  (add-hook 'find-file-hooks 'auto-insert))

(defun customize-org-mode ()
  "Customize org-mode."
  (with-eval-after-load 'org
    (setq org-pretty-entities t
          org-hide-emphasis-markers t
          org-hide-leading-stars t
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-emphasized-text t
          org-fontify-quote-and-verse-blocks t
          spaceline-org-clock-p t)))

(defun register-hooks ()
  "Attach customizations to appropriate hooks."
  (setup-file-templates)
  ;; Set execute permissions automatically when saving scripts.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; To perform full-document previews.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Customize org mode.
  (customize-org-mode))

(defun register-new-modes ()
  "Register new modes."
  (define-global-minor-mode global-hidden-mode-line-mode
    hidden-mode-line-mode
    (lambda () (hidden-mode-line-mode)))

  ;; Hide mode line by default.
  (global-hidden-mode-line-mode))

(defun enable-utf-8 ()
  "Use utf-8 everywhere."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(defun setup-edit-prefs ()
  "Setup editor preferences."
  (enable-utf-8)
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
  (setq-default helm-ff-file-name-history-use-recentf t))

;; ---( Defining behaviors of modes )---
;; `__
(defun setup-TeX-mode ()
  (setq-default TeX-master nil))

(defun personalize ()
  "Load custom configuration."
  (setup-auto-saves-and-bkups)
  (setup-appearance)
  (register-new-modes)
  (register-hooks)
  (setup-common-key-bindings)
  (setup-edit-prefs)
  (setup-TeX-mode))

(personalize)
