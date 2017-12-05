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
                (loop for i in '("c" "cc" "clj" "cpp" "erl"
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
  (setq auto-save-interval 30))

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
    ;; Disable visual line mode.
    (visual-line-mode -1)
    ;; NOTE: `visual-line-mode' affects `truncate-lines' and `word-wrap'.
    ;; Enable `truncate-lines' feature.
    (toggle-truncate-lines 1)
    (message "distraction-free-writing mode disabled.")
    (put 'toggle-distraction-free-writing 'state nil)))

(defun enable-distraction-free-writing ()
  "Enable distraction-free-writing mode."
  (progn
    ;; Enable visual line mode.
    (visual-line-mode 1)
    ;; Disable `truncate-lines' feature.
    (toggle-truncate-lines -1)
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

(defun setup-fira-code-ligatures ()
  "Setup ligatures for `Fira Code' font."

  ;; `Fira Code' setup on emacs using code from [1].
  ;; [1] https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs

  ;; This works when using emacs --daemon + emacsclient
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
  ;; This works when using emacs without server/client
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  ;; I haven't found one statement that makes both of the above situations work,
  ;;  so I use both for now

  (defconst fira-code-font-lock-keywords-alist
    (mapcar
     (lambda (regex-char-pair)
       `(,(car regex-char-pair)
         (0 (prog1 ()
              (compose-region
               (match-beginning 1)
               (match-end 1)
               ;; The first argument to concat is a string containing a literal tab
               ,(concat "	"
                        (list (decode-char 'ucs (cadr regex-char-pair)))))))))
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
       ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
       ("[^:=]\\(:\\)[^:=]"           #Xe16c)
       ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
       ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  (defun add-fira-code-symbol-keywords ()
    (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

  (add-hook 'prog-mode-hook
            #'add-fira-code-symbol-keywords))


(defun customize-look-and-feel ()
  "Customize how the editor frame or window looks and feels."
  ;; Increase space between text and the border or fringe.
  (add-to-list 'default-frame-alist '(internal-border-width . 20))

  (when (string-equal "Fira Code"
                      (font-get (face-attribute 'default :font) :family))
    (setup-fira-code-ligatures))

  (global-prettify-symbols-mode t)

  ;; Highlight current line.
  (global-hl-line-mode -1))


;; --- Customize modes.
;; --------------------------------------------------

(defun setup-programming-mode ()
  "Add customizations to setup a development environment."
  ;; Turn on support for boilerplates.
  (setup-file-boilerplates)

  ;; Tack on customizations to the `prog-mode-hook'.
  (add-hook 'prog-mode-hook
            (lambda ()
              (progn
                ;; Turn off line-truncation; wrap lines around.
                (toggle-truncate-lines -1))))

  ;; Set execute permissions automatically when saving scripts.
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

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
       ;; org-bullets-bullet-list '(" ")
       org-fontify-whole-heading-line t
       org-fontify-done-headline t
       org-fontify-emphasized-text t
       org-fontify-quote-and-verse-blocks t
       org-src-fontify-natively t
       org-hide-emphasis-markers t
       org-hide-leading-stars t
       org-indent-mode t
       org-pretty-entities t
       ;; org-startup-indented t
       spaceline-org-clock-p t)

      (let* ((heading-font    `(:font ,serif-font-face))
             (heading-attrs   `(:inherit default :weight bold))
             (monospace-font  `(:font ,monospace-font-face))
             (monospace-attrs `(:inherit default)))

        (custom-theme-set-faces
         'user
         `(org-level-8   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-7   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-6   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-5   ((t (,@heading-attrs ,@heading-font))))
         `(org-level-4   ((t (,@heading-attrs ,@heading-font :height 1.1))))
         `(org-level-3   ((t (,@heading-attrs ,@heading-font :height 1.25))))
         `(org-level-2   ((t (,@heading-attrs ,@heading-font :height 1.5))))
         `(org-level-1   ((t (,@heading-attrs ,@heading-font :height 1.75))))
         `(org-code      ((t (,@monospace-attrs ,@monospace-font))))
         `(org-block     ((t (,@monospace-attrs ,@monospace-font))))
         `(org-verbatim  ((t (,@monospace-attrs ,@monospace-font))))
         `(org-table     ((t (,@monospace-attrs ,@monospace-font))))
         `(org-document-title
           ((t (,@heading-attrs ,@heading-font
                                :height 1.5 :underline nil))))))))

  ;; Turn on `auto-save' for `org-mode' to _safely_ store notes.
  (add-hook 'org-mode-hook 'auto-save-mode)

  (add-hook 'org-mode-hook
            (lambda ()
              (progn
                (setq buffer-face-mode-face `(:family ,serif-font-face))
                (buffer-face-mode)))))

(defun setup-latex-mode ()
  "Customize `latex-mode'."
  (with-eval-after-load 'latex
    (setq-default TeX-master nil))

  ;; Perform full-document previews.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(defun setup-markdown-mode ()
  "Customize `markdown-mode'."
  (add-hook 'markdown-mode-hook
            (lambda ()
              (progn
                (setq markdown-fontify-code-blocks-natively t
                      markdown-hide-markup t)))))

(defun setup-text-mode ()
  "Customize `text-mode'."
  (add-hook 'text-mode-hook
            (lambda ()
              (progn
                (setq buffer-face-mode-face `(:family ,serif-font-face))
                (buffer-face-mode)))))

(defun customize-modes ()
  "Customize different modes."
  (setup-programming-mode)
  (setup-org-mode)
  (setup-latex-mode)
  (setup-markdown-mode)
  (setup-text-mode))


;; --- Common editor preferences. ---
;; --------------------------------------------------

(defun use-utf-8-everywhere ()
  "Use utf-8 everywhere."
  (setq locale-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(defun customize-editor-behavior ()
  "Customize common editor behavior."
  (use-utf-8-everywhere)

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


;;; funcs.el ends here
