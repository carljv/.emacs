; Carl Vogel's init.el file.
;;; Commentary:


;;; CODE:

;;* Preface

;;** Tracking startup time

;; I like to keep track of the load time for my init file. I store
;; it in this global variable that I display in the scratch on startup.
(defvar *startup-time* 0)
(defvar *emacs-load-start* (current-time))
;;** Give the GC a rest

;; By default, Emacs runs the garbage collector when the heap exceeds 800KB.
;; To avoid GC pauses during startup, I increase that threshold to 100MB.
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;;** Basic global variables

;; Most of the time I'm working on something in my =~/projects/= folder,
;; so just start there. Also, I want automatic backups kept in a single
;; folder, not alongside the files themeselves.
(setq user-full-name "Carl Vogel"
      user-mail-address "carl.vogel@warbyparker.com"
      default-directory "~/projects/"
      backup-directory-alist `(("." . "~/.emacs.d/saves/")))

;; My own packages
(add-to-list 'load-path "~/.emacs.d/cjv")

;; Some emacs commands want you to type a full "yes" or "no" before
;; proceeding. This sets them to ask for "y" or "n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Cleanup whitespace on save
; (add-hook 'before-save-hook #'whitespace-cleanup)

;;** Convenience functions

;; There are hooks I want to apply to multiple modes. This is a
;; wrapper around =add-hook= that loops over a list of modes and
;; adds the hook.
(defun add-to-hooks (modes-hooks hook)
  "Add to each MODES-HOOKS list a given HOOK."
  (dolist (mode modes-hooks)
    (add-hook mode hook)))

;; Set right margin for text wrapping so lines are < 72 chars.
(defun toggle-margin-right ()
  "Set a margin on the right for prose buffers."
  (interactive)
  (if (null (cdr (window-margins)))
      (set-window-margins nil 5 (max 5 (- (window-body-width) 72)))
    (set-window-margins nil 0 0)))


;;* Visual settings

;; Get rid of nearly all visual bits of the emacs frame.
(setq inhibit-startup-screen t)
(when tool-bar-mode (tool-bar-mode -1))
(when scroll-bar-mode (scroll-bar-mode -1))
(when fringe-mode (set-fringe-mode 0))
(when (not column-number-mode) (column-number-mode t))
(delete-selection-mode 1)
(show-paren-mode 1)

;; This silences the error bell (beep) and flashes the mode line instead.
(setq visible-bell nil
      ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))


;;* Package management

;; Tell emacs where to find packages, and initialize installed packages.
(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; We need to load up =use-package= right away so we can use it to load
;; other packages.
(eval-when-compile (require 'use-package))


;;** Settings for GUI vs terminal clients

;; I only use terminal emacs for quick editing sessions, so I don't need
;; a lot of features running, and some themes and font-locking can look
;; bad in the terminal. So I only want to load some settings in GUI mode.
;; I solve this by running *two* separate daemons: one for GUI clients to
;; connect to, and one for terminal clients to connect to. A new instance
;; checks which daemon it's running so it knows which settings to load.
(defvar *gui-client*       (equal (daemonp) "gui"))
(defvar *term-client*      (equal (daemonp) "term"))

;; This just tells terminal/tty instances to not use colors.
(add-to-list 'default-frame-alist '(tty-color-mode  . -1))

;; This function applies a number of settings meant just for GUI instances.
;; It loads a color theme, sets a font, and makes some changes to font-lock
;; face rules. It also tells emacs to maximize the window when on start.
(defun graphic-setup ()
  "Settings for graphic Emacs instances."
  (progn
    ;; This theme can interfere w/ terminal theme in a weird way.
    (load-theme 'misterioso t)
    (set-face-attribute 'font-lock-constant-face nil
                        :slant 'normal :weight 'normal)
    ;; Brighten the cursor
    (set-cursor-color "white")
    ;; Set font and line-spacing
    (add-to-list 'default-frame-alist '(font . "Triplicate T4c-13"))
    ;; Fallback font with wider unicode coverage
    (set-fontset-font "fontset-default" 'unicode
                  (font-spec :name "DejaVu Sans" :size 10))
    (setq-default line-spacing 2)
    ;; Maximize window at start
    (add-to-list 'default-frame-alist '(fullscreen . maximized))))

(when (or *gui-client* (display-graphic-p))
  (graphic-setup))


;;* Navigation and convenience bindings

;;** Hate this keybinding - on OS X it minimizes the window.
(global-unset-key (kbd "C-z"))

;;** Mac friendly text zoom keys.

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") #'(lambda () (interactive) (text-scale-adjust "0")))


;;** Handy e-shell and ielm access access
(global-set-key (kbd "C-c $") 'eshell)
(global-set-key (kbd "C-c >") 'ielm)


;;** Copy iterm navigation shortcuts.

(global-set-key (kbd "s-]") 'other-window)
(global-set-key (kbd "C-c ]") 'other-window)
(global-set-key (kbd "s-[") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-c [") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-}") 'switch-to-next-buffer)
(global-set-key (kbd "s-{") 'switch-to-prev-buffer)
(global-set-key (kbd "s-d") '(lambda ()
                               (interactive)
                               (split-window-right)
                               (other-window 1)))
(global-set-key (kbd "s-D") '(lambda ()
                               (interactive)
                               (split-window-below)
                               (other-window 1)))
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-W") 'delete-other-windows)


;;** Easily kill help and other special pop-ups.
(setq help-window-select t)
(define-key special-mode-map "q" nil)
(define-key special-mode-map "q" '(lambda () (interactive) (quit-restore-window nil 'kill)))

;; Emacs gives a warning about this command being confusing. That's dumb.
(put 'narrow-to-region 'disabled nil)


;;** Quick init-file access
(defun open-my-init ()
  "Open init.el, or if already open, load it and kill the buffer."
  (interactive)
  (if (string-equal (buffer-file-name) user-init-file)
      ;; Calling function from init-file buffer.
      (if (buffer-modified-p)
          ;; Buffer has been modified, don't kill.
          (message "Buffer modified. Save, or use C-x k to kill.")
        ;; Buffer is saved, go ahead and kill.
        (progn (kill-buffer (current-buffer)) (load-file user-init-file)))
    ;; Call from a non-init file buffer. Open the file.
    (find-file user-init-file)))

;; Bind to CMD-, to match OS X preferences shortcut, or C-c ,
;; as backup for the terminal.
(global-set-key (kbd "s-,") 'open-my-init)
(global-set-key (kbd "C-c ,") 'open-my-init)


;;* Load up packages
;;** Exec-path-from-shell
(use-package exec-path-from-shell
  :defer 2
  :config (exec-path-from-shell-initialize))

;;** Flycheck

;; Flycheck is a linter backend. I've been adding this to major modes
;; explicitly. For python, I've been using =flake8= as the linter.
;;
;; TODO: Fix the R and elisp linter configurations, which are way too
;; aggressive.


(use-package flycheck
  :ensure t
  :defer t
  :init
  (setq flycheck-python-flake8-executable "flake8")
  (use-package flycheck-pycheckers
    :init
    (setq f≈lycheck-pycheckers-checkers '(flake8)
          flycheck-pycheckers-max-line-length 95))
  (add-to-hooks '(elpy-mode-hook ess-mode-hook) #'flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
  (setq flycheck-highlighting-mode nil)
  :commands flycheck-mode
  :diminish flycheck-mode)


;;** Company-mode

;;Company-mode does autocompletion. Again, I hook it to modes explicitly.
(use-package company
  :ensure t
  :defer t
  :init
  (add-to-hooks '(cider-mode-hook
                  cider-repl-mode-hook
                  comint-mode-hook
                  ;elpy-mode-hook
                  ess-mode-hook
                  ;inferior-python-mode-hook
                  inferior-ess-mode-hook)
                #'company-mode)
  :diminish company-mode
  :bind (:map company-active-map
              ("C-d" . company-show-doc-buffer)))

;;** SQL
(use-package sql
  :init
  (setq sql-connection-alist
        '(("helios"
           (sql-product 'postgres)
           (sql-user "carl.vogel")
           (sql-port 5432)
           (sql-server "localhost")
           (sql-database "helios")))))

;;** Dired

;; Dired is loaded by default, but I want to specify some options and
;; new keybindings for it.
(use-package dired
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-listing-switches "-alhp")
  :bind (:map dired-mode-map
              ("<return>" . dired-find-alternate-file)))

;;** Magit

;; Magit is a git interface for emacs. I want a keybinding to call up the git status screen.
(use-package magit
  :defer t
  :init (global-set-key (kbd "C-c g s") 'magit-status))

;;** Evil
;; Maintain some basic emacs keybinding for navigation in Insert mode.
(defun use-emacs-keys-in-insert ()
  "Preserve Emacs nav keys in insert mode."
  (when evil-mode
    (define-key evil-insert-state-map (kbd "C-a") #'evil-beginning-of-line)
    (define-key evil-insert-state-map (kbd "C-e") #'evil-append-line)
    (define-key evil-insert-state-map (kbd "C-k") #'evil-delete-line)))

(use-package key-chord
  :defer t
  :init
  (add-hook 'evil-mode-hook
            '(lambda ()
               (setq key-chord-two-keys-delay 0.5)
               (key-chord-mode 1))))

(use-package evil
  :init
  (evil-mode 1)
  :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (setq evil-symbol-word-search t)
  :bind (:map evil-insert-state-map
              ("C-a" . evil-beginning-of-line)
              ("C-e" . evil-append-line)
              ("C-k" . evil-delete-line)))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

;; evil mode isn't great at respecting visual lines.
;; this tries to remedy that. Also applied to emacs
;; movement keychords applied to evil movements.
(defun set-visual-line-evil ()
  "Make evil work better in visual-line mode."
  (when evil-mode
    (evil-define-key 'motion visual-line-mode-map "j" #'evil-next-visual-line)
    (evil-define-key 'motion visual-line-mode-map "k" #'evil-previous-visual-line)
    (evil-define-key 'motion visual-line-mode-map "$" #'evil-end-of-visual-line)
    (evil-define-key 'motion visual-line-mode-map "^" #'evil-first-non-blank-of-visual-line)
    (evil-define-key 'motion visual-line-mode-map "0" #'evil-beginning-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-a") #'evil-beginning-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-e") #'evil-end-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-k") #'kill-visual-line)))

(defun evil-set-comint ()
  "Provide reasonable evil-mode motions to 'comint-mode'."
  (evil-define-key 'motion comint-mode-map "0" 'comint-bol)
  (evil-define-key 'motion comint-mode-map "^" 'comint-bol))

;;** Visual lines
(add-hook 'visual-line-mode-hook
          '(lambda () (progn (toggle-margin-right)
                            (set-visual-line-evil))))

;;** Eshell
(defun eshell-evil-nav ()
  "Eshell has dumb BOL nav.  Fix it."
  (evil-define-key 'insert eshell-mode-map (kbd "C-a") 'eshell-bol)
  (evil-define-key 'motion eshell-mode-map (kbd "0")   'eshell-bol)
  (evil-define-key 'motion eshell-mode-map (kbd "^")   'eshell-bol))

(use-package eshell
  :defer t
  :init (eshell-evil-nav)
  :config
  (setq eshell-visual-commands
        '("less" "tmux" "htop" "top" "bash" "zsh" "fish")
        eshell-visual-subcommands
        '("git" "log" "l" "diff" "show"))
  (add-hook 'eshell-mode-hook
            '(lambda ()
               (define-key eshell-mode-map (kbd "C-a") #'eshell-bol))))

;;** Ivy, Counsel, and Swiper
(use-package ivy
  :defer t
  :config
  (ivy-mode 1)
  (setq
   ivy-extra-directories nil
   counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
  :diminish ivy-mode
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h f"   . counsel-describe-function)
  ("C-h v"   . counsel-describe-variable)
  ("C-c i"   . counsel-imenu)
  ("C-c k"   . counsel-ag)
  ("<f2> u"  . counsel-unicode-char)
  ("<f2> i"  . counsel-info-symbol-lookup)
  ("C-c C-r" . ivy-resume)
  ("s-b"     . ivy-switch-buffer))

;;** Projectile
(use-package projectile
  :defer 1
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  :bind
  (:map projectile-mode-map ("s-p" . projectile-command-map))
  :diminish projectile-mode)

(use-package counsel-dash
  :config (setq-default
           counsel-dash-common-docsets
           '("Emacs Lisp" "Python 3" "Bash" "R"
             "CSS" "C++" "PostgreSQL" "CMake" "SQLAlchemy" "Vim")
           helm-dash-browser-func #'eww
           helm-dash-enable-debugging nil)
  :bind
  (("C-c ?" . counsel-dash)))


;;** Cider / Clojure
(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer nil
        cider-pprint-fn 'fipp
        cider-repl-use-pretty-printing t
        nrepl-hide-special-buffers t
        cider-save-file-on-load t)
  (add-hook 'clojure-mode-hook #'cider-mode)
  :diminish clojure-mode
)

;;** ESS
(defun r-pipe-operator ()
  "R - %>% operator or 'then' pipe operator."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

(use-package ess-site
  :after exec-path-from-shell
  :commands
  (R-mode)
  :config
  (defun kill-ess-help-window ()
    (interactive)
    (quit-restore-window nil 'kill))
  (setq
   inferior-r-args "--no-resore"
   ess-ask-for-ess-directory nil
   ess-use-company t
   ess-style "RStudio"
   ess-indent-with-fancy-comments nil)
  (dolist (kw '(ess-R-fl-keyword:fun-defs
                ess-R-fl-keyword:keywords
                ess-fl-keyword:fun-calls
                ess-fl-keyword:delimiters))
    (setf (alist-get kw ess-R-font-lock-keywords) t))
   :bind (:map ess-mode-map
              ("C-." . r-pipe-operator)
              :map inferior-ess-mode-map
              ("C-." . r-pipe-operator)
              :map ess-help-mode-map
              ("q" . kill-ess-help-window)))

;;** Prettify-symbols
(use-package prettify-symbols
  :defer t
  :init
  (defun make-r-pretty ()
    (add-to-list 'prettify-symbols-alist '("%>%" . ?⧐))
    (add-to-list 'prettify-symbols-alist '("<-" . ?⟵))
    (add-to-list 'prettify-symbols-alist '("->" . ?⟶))
    (add-to-list 'prettify-symbols-alist '("%in%" . ?∈))
    (prettify-symbols-mode))
  (add-to-hooks '(ess-mode-hook inferior-ess-mode-hook)
                #'make-r-pretty))

;;** Auctex
(use-package tex-site
  :ensure auctex
  :config
  (setq
   TeX-view-program-list '(("Preview" "open -a /Applications/Preview.app %o"))
   TeX-view-program-selection '((output-pdf "Preview"))
   LaTeX-command "/usr/bin/xelatex"
   TeX-engine 'xetex
   TeX-PDF-mode t
   TeX-open-quote "\""
   TeX-close-quote "\""))

;;** Neotree
(use-package neotree
  :init (global-set-key (kbd "<f8>") 'neotree-toggle))

;;** Python
(defun eval-region-or-line-elpy ()
  "Send region (if selected) or current line to Python shell."
  (interactive)
  (if (region-active-p)
      (elpy-shell-send-region-or-buffer)
    (elpy-shell-send-current-statement)))

(use-package pyvenv
  :defer t
  :config (pyvenv-tracking-mode 1))

(defun conda-env-activate (envname)
  (interactive
  (let* ((conda-env-root (expand-file-name "~/opt/anaconda3/envs/"))
         (conda-env-root-subdirs (directory-files conda-env-root))
         (conda-env-names (remove "." (remove ".." conda-env-root-subdirs)))
         (completion-ignore-case t))
    (list (completing-read "Activate conda env: " conda-env-names nil t))))
  (let ((envdir (concat (expand-file-name "~/opt/anaconda3/envs/") envname "/")))
    (message (concat "Activating " envdir))
    (pyvenv-activate envdir)))

(use-package elpy
  :commands elpy-mode
  :init (add-hook 'python-mode-hook 'elpy-mode)
  :config
  (setenv "WORKON_HOME" "/Users/carl.vogelwarbyparker.com/opt/anaconda3/envs/")
  (setq python-shell-prompt-detect-failure-warning nil
        python-shell-completion-native-enable t
        python-shell-enable-font-lock t
        elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv)
        python-shell-interpreter "python" ;; "jupyter"
        ;; python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        elpy-rpc-backend "jedi")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (elpy-enable)
  :bind (:map elpy-mode-map
              ("C-<return>" . nil)
              ("C-<return>" . eval-region-or-line-elpy)
              ("C-c e w" . conda-env-activate)
              ("C-c e d" . pyvenv-deactivate)
              ("C-c ." . elpy-goto-definition))
  :diminish elpy-mode)


;;** Comint
(use-package comint
  :defer t
  :config
  (evil-set-comint)
  :bind (:map comint-mode-map
              ("<up>"   . comint-previous-matching-input-from-input)
              ("<down>" . comint-next-matching-input-from-input)
              ("C-p"    . comint-previous-matching-input-from-input)
              ("C-n"    . comint-next-matching-input-from-input)
              ("C-r"    . comint-history-isearch-backwards-regexp)))

;;** SLIME
(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"
        slime-backend "~/.emacs.d/elpa/slime-20170828.451/swank-loader.lisp"
        slime-header-line-p nil
        slime-load-failed-fasl 'never
        slime-contribs '(slime-banner
                         slime-fancy
                         slime-quicklisp
                         slime-company))
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (slime-setup))

(use-package olivetti
  :defer t
  :init (add-hook 'poly-markdown+r-mode-hook #'olivetti-mode))


(use-package imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil)
  (setq imenu-list-after-jump-hook #'imenu-list-quit-window))

;; (use-package polymode
;;   :defer t
;;   :mode
;;   ("\\.[rR]md\\'" . poly-markdown+r-mode)
;;   :init
;;   (use-package poly-R
;;     :defer t
;;     :config (use-package rmd)
;;     :bind
;;     (:map poly-markdown+r-mode-map
;;	  ("C-<return>" . cv/eval-knitr-chunk)
;;	  ("s-K"        . cv/knit-to-html)
;;	  ("C-}"        . polymode-next-chunk)
;;	  ("C-{"        . polymode-previous-chunk))
;;   (use-package poly-markdown :defer t)))

  ;;** Text and Markdown
(use-package markdown
  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'"       . gfm-mode)
   ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-math t)
  (setq indent-tabs-mode nil))

;;** CSVs
(use-package csv-mode
  :mode
  (("\\.csv\\'" . csv-mode)
   ("\\.tsv\\'" . csv-mode))
  :config
  (add-hook 'csv-mode-hook
            '(lambda () (csv-align-fields nil (point-min) (point-max))))
  (add-hook 'csv-mode-hook '(lambda () (font-lock-mode -1)))
  (add-hook 'csv-mode-hook 'read-only-mode)
  :bind
  (:map csv-mode-map
        ("<tab>" . csv-forward-field)
        ("S-<tab>" . csv-backward-field)))

;;** Flyspell
(use-package flyspell
  :defer t
  :config
  (add-to-hooks '(markdown-mode gfm-mode org-mode)
                'flyspell-mode)
  :diminish flyspell-mode)

;;** Recent files
(global-set-key (kbd "C-c f") 'counsel-recentf)

(setq-default recentf-auto-cleanup 'never)
(add-hook 'kill-emacs-hook
          '(lambda () (when (boundp 'recentf-cleanup) recentf-cleanup)))

;;** El-doc
(use-package eldoc
  :init
  (add-to-hooks '(cider-mode-hook
                  cider-repl-mode-hook
                  emacs-lisp-mode-hook)
                #'eldoc-mode)
  :diminish eldoc-mode)

;;** Undo-tree
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)


;;** Org-mode

;; Let =org-babel= execute code blocks from various languages.
(use-package org
  :defer t
  :config
  (setq-default org-babel-load-languages
                '((C . t)
                  (D . t)
                  (R . t)
                  (awk . t)
                  (cpp . t)
                  (emacs-lisp . t)
                  (julia . t)
                  (python . t)
                  (shell . t)
                  (sql . t))))

(use-package visual-line
  :defer t
  :config
  (add-hook 'org-mode-hooks #'visual-line-mode))


;;* Coda
(setq *startup-time*
      (float-time (time-subtract (current-time) *emacs-load-start*)))
(setq initial-scratch-message
      (format "Welcome to Emacs!\n%sStartup time: %3.2f seconds.\n\n"
              (if (daemonp) (concat "Server      : " (daemonp) "\n") "")
              *startup-time*))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" "d986619578e8a8dabb846e91c54090b82d937672f54ffa0ef247c0428813d602" "89f545ddc104836b27167696db89b371f23893d5b2f038d43383d877ee678d3d" "d7383f47263f7969baf3856ab8b3df649eb77eafdff0c5731bee2ad18e0faed2" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "53a9ec5700cf2bb2f7059a584c12a5fdc89f7811530294f9eaf92db526a9fb5f" "0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "b85fc9f122202c71b9884c5aff428eb81b99d25d619ee6fde7f3016e08515f07" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" default)))
 '(ess-default-style (quote RStudio))
 '(ess-smart-S-assign-key nil)
 '(ess-style (quote RStudio))
 '(exec-path-from-shell-check-startup-files nil)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (eglot humanoid-themes poly-R counsel-tramp docker-compose-mode docker-tramp dockerfile-mode green-phosphor-theme minimal-theme subatomic-theme flycheck-pycheckers flycheck-mypy stan-mode creamsody-theme 4clojure realgud smex imenu-list rainbow-mode polymode cider olivetti slime-company htmlize org bongo demo-it zenburn-theme yaml-mode use-package ujelly-theme subatomic256-theme sphinx-mode sphinx-doc soft-charcoal-theme slime pg neotree markdown-mode magit key-chord ivy-hydra flycheck exec-path-from-shell evil-surround evil-matchit ess elpy dracula-theme csv-mode counsel-projectile counsel-dash company-quickhelp color-theme-sanityinc-tomorrow auctex ag)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".domino" ".Rproj.user")))
 '(projectile-globally-ignored-files (quote (".Rhistory" "TAGS"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
