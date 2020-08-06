					; Carl Vogel's init.el file.


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                           PRELUDE / GLOBAL SETTINGS
;;                           -------------------------
;; This is I set most of the global settings: visual stuff, global keybindings,
;; etc.
;;
;; This is all the stuff that isn't specific to any particular mode or modes.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ============================================================
;; Tracking startup time
;; ---------------------
;; I like to keep track of the load time for my init file.
;; I store it in a global variable that I display in the
;; scratch buffer on startup. We'll initialize the timing
;; variables here, then update and print them at the end of the
;; file.
;; ============================================================

(defvar *carljv/startup-time* 0)
(defvar *carljv/emacs-load-start* (current-time))


;; ============================================================
;; Relaxing the GC
;; ---------------
;; By default, Emacs runs the garbage collector when the heap
;; exceed 800KB. To avoid GC pauses during startup, we can
;; increase that threshold to 100MB (which is fine on a
;; modern machine). We'll reset it after startup is done.
;; ============================================================

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))


;; ============================================================
;; Basic global variables
;; ----------------------
;; Most of the time I'm working on something in my ~/projects/
;; folder, so let's just start there. Also, I want automatic
;; backups kept in a single centralized folder, not alongside
;; the files themselves.
;;
;; Also, we want Emacs to know about the anaconda path, so
;; we can point it to the correct executables and environments.
;; ============================================================

(setq user-full-name    "Carl Vogel"
      user-mail-address "carl.vogel@warbyparker.com"
      default-directory "~/projects/"
      backup-directory-alist `(("." . "~/.emacs.d/saves/")))


(defconst carljv/anaconda-dir
  (expand-file-name "~/opt/anaconda3/"))


;; ============================================================
;; Basic visual settings
;; ---------------------
;; These settings apply to both GUI and terminal Emacs.
;; ============================================================

(defvar *carljv/my-theme* 'deeper-blue)
(defvar *carljv/my-font*  "Triplicate T4c-13")

;; Get rid of the visual doodads.
(setq inhibit-startup-screen t)
(when tool-bar-mode (tool-bar-mode -1))
(when scroll-bar-mode (scroll-bar-mode -1))
(when fringe-mode (set-fringe-mode 0))

;; Show the column number in the mode line.
(when (not column-number-mode) (column-number-mode t))

;; Type over a selection to replace it.
(delete-selection-mode 1)

;; Flicker matching parens under the cursor.
(show-paren-mode 1)

;; Only blink the cursor 5 times
(setq blink-cursor-blinks 5)

;; Flash the mode line instead of ringing the bell.
(defun carljv/flash-mode-line ()
  "Quickly 'flash' the mode line by inverting the colors quickly."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function #'carljv/flash-mode-line)


;; ============================================================
;; GUI visual settings
;; -------------------
;; There are a number of fancy visual changes I want to make
;; that only really work well in graphic/GUI instances, and
;; I don't want them applied when in terminal Emacs.
;; ============================================================

(defvar *gui-client*  (equal (daemonp) "gui"))
(defvar *term-client* (equal (daemonp) "term"))

;; Don't use colors in terminal/tty instances.
(add-to-list 'default-frame-alist '(tty-color-mode . -1))

;; A function that applies a number of visual settings meant
;; for GUI instances. It loads a color theme, sets a font,
;; and makes some changes to font-lock face rules. It also
;; tells Emacs to maximize its window on start.
(defun carljv/graphic-setup ()
  "Settings for graphic/GUI Emacs instances."
  (progn
    ;; Set the theme.
    (load-theme *carljv/my-theme* t)
    ;; The cursor can be hard to see with the misterioso theme.
    (when (eq *carljv/my-theme* 'misterioso)
      (set-cursor-color "white"))
    ;; Set fonts and line-spacing.
    (add-to-list 'default-frame-alist `(font . ,*carljv/my-font*))
    ;; Set a default/fallback font with wide unicode coverage.
    (set-fontset-font "fontset-default" 'unicode
		      (font-spec :name "DejaVu Sans" :size 13))
    (setq-default line-spacing 2)
    ;; Maximize the window on start.
    (add-to-list 'default-frame-alist '(fullscreen . maximized))))

;; Apply graphic settings when we're either running a GUI daemon
;; client or we're in a normal GUI instance.
(when (or *gui-client* (display-graphic-p))
  (carljv/graphic-setup))


;; ============================================================
;; Global navigation and convenience bindings
;; ------------------------------------------
;; These are a number of OS X-friendly keybindings for changing
;; buffers and managing windows.
;; ============================================================

;; I hate this keybinding, I often hit it by accident and it
;; minimized the window on OS X.
(global-unset-key (kbd "C-z"))

;; OS X-friendly text-zoom keys
(global-set-key (kbd "s-=") #'text-scale-increase)
(global-set-key (kbd "s-+") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)
(global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-adjust "0")))


;; ⌘-] and ⌘-[ cycle forwards and back around windows in the frame.
(global-set-key (kbd "s-]")   #'other-window)
(global-set-key (kbd "s-[")   (lambda () (interactive) (other-window -1)))

;; ⌘-} and ⌘-{ cycle through open buffers in the current window.
(global-set-key (kbd "s-}")   #'switch-to-next-buffer)
(global-set-key (kbd "s-{")   #'switch-to-prev-buffer)

;; ⌘-d splits the current window vertically,
;; ⌘-D splits it horizontally.
(global-set-key (kbd "s-d")   (lambda ()
				(interactive)
				(split-window-right)
				(other-window 1)))
(global-set-key (kbd "s-D")   (lambda ()
				(interactive)
				(split-window-below)
				(other-window 1)))

;; ⌘-w deletes the current window. ⌘-W keeps the current window,
;; and deletes all the others.
(global-set-key (kbd "s-w")    #'delete-window)
(global-set-key (kbd "s-W")    #'delete-other-windows)


;; Easily kill help buffers and other special popups with "q"
(setq help-window-select t)
(define-key special-mode-map "q" nil)
(define-key special-mode-map "q"
  (lambda () (interactive) (quit-restore-window nil 'kill)))


;; ============================================================
;; Misc. UI changes
;; ============================================================

;; Emacs sometimes requires "yes" or "no" instead of "y" and "n".
;; Make it always ask for single-letter confirmations.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs warns about this command being confusing. Let's not.
(put 'narrow-to-region 'disabled nil)


;; ============================================================
;; Fast init.el access
;; -------------------
;; Here I bind opening and closing the init.el file to the
;; general Mac shortcut for opening app preferences, ⌘-,.
;; ============================================================

(defun carljv/open-my-init ()
  "Open init.el or, if it's already open, load it and kill the buffer."
  (interactive)
  (if (string-equal (buffer-file-name) user-init-file)
      ;; If we're calling this from an open init.el, check if it's saved,
      ;; and if so, load it and kill the buffer.
      (if (buffer-modified-p)
	  (message "Buffer modified. Save, or use C-x to kill.")
	(progn (kill-buffer (current-buffer))
	       (load-file user-init-file)))
    ;; If we're calling from some other file, open init.el
    (find-file user-init-file)))

;; Bind this to ⌘-,.
(global-set-key (kbd "s-,") #'carljv/open-my-init)
(global-set-key (kbd "C-c ,") #'carljv/open-my-init)


;; ============================================================
;; Fast theme-switcher
;; -------------------
;; We can cycle through available themes using ⌘-T.
;; ============================================================

(defun load-next-theme (&optional current-theme)
  "Load the next theme in (CURRENT-AVAIBLE-THEMES). Disable the current custom theme.

If the last theme in (CURRENT-AVAILABLE-THEMES) is loaded, cycle back to the first."
  (interactive)
  (let* ((current-theme (or current-theme (car custom-enabled-themes)))
	 (next-theme
	  (when current-theme
	    (cadr
	     (seq-drop-while
	      (lambda (elt) (not (eq elt current-theme)))
	      (custom-available-themes)))))
	 (next-theme (or next-theme (car (custom-available-themes)))))
    (while custom-enabled-themes (disable-theme (car custom-enabled-themes)))
    (condition-case nil
	(progn (load-theme next-theme t nil)
	       (when (eq next-theme 'misterioso) (set-cursor-color "white"))
	       (disable-theme current-theme)
	       (message (symbol-name next-theme)))
      (error (load-next-theme next-theme)))))

(global-set-key (kbd "s-T") #'load-next-theme)


;; ============================================================
;; Globally-useful convenience functions
;; ============================================================

;; There are some things I want to apply to multiple mode-hooks
;; at once (e.g., having some minor mode load for several different
;; major modes). This is a wrapper around add-hook that loops
;; over a list of hooks and applies the function to the hook.
(defun carljv/add-to-hooks (hooks fn)
  "Add FN to each hook in a list of HOOKS."
  (dolist (hook hooks)
    (add-hook hook fn)))



;; ============================================================
;; Set up package repositories
;; ============================================================

;; Tell Emacs where to find packages, and initialize installed packages.
(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; We need to load up use-package right away so we can use it to load
;; other packages.
(eval-when-compile (require 'use-package))

;; ============================================================
;; Get PATH from system shell
;;
;; This adds 1+ seconds to startup time, so it's nice to defer
;; it, but some packages rely on it when they load, so we don't
;; want to defer it for long.
;; ============================================================
(use-package exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                          GENERAL PURPOSE (MINOR) MODES
;;                          -----------------------------
;;
;; These are modes that I use everywhere. They add general functionality like new
;; keybindings, navigation, autocompletion, etc.
;;
;; I use use-package to load packages for different modes and to apply
;; custom settings to them.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ============================================================
;; Evil
;; ----
;; evil-mode provides robust vim emulation.
;; ============================================================

;; Key-chord allows me to bind commands to keys combinations
;; even if they aren't hit simultaneously. I basically just
;; use this to map jj to <Esc> in evil mode.
(use-package key-chord
  :defer t
  :init
  (add-hook 'evil-mode-hook
	    (lambda ()
	      (setq key-chord-two-keys-delay 0.5)
	      (key-chord-mode 1))))


;; The main customizations I make to evil are to
;; (1) Map jj to <Esc> using keychord
;; (2) Preserve the use of some basic Emacs navigation keybindings
;;     in Insert mode.
(use-package evil
  :init
  (evil-mode 1)
  :config
  (setq evil-symbol-word-search t)
  (key-chord-define evil-insert-state-map  "jj" #'evil-normal-state)
  (key-chord-define evil-normal-state-map  "jj" #'evil-force-normal-state)
  (key-chord-define evil-visual-state-map  "jj" #'evil-change-to-previous-state)
  (key-chord-define evil-replace-state-map "jj" #'evil-normal-state)
  :bind
  (:map evil-insert-state-map
	("C-a" . evil-beginning-of-line)
	("C-e" . evil-append-line)
	("C-k" . evil-delete-line)))


(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))


(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))


;; ============================================================
;; Fixing evil keybindings in other modes
;; --------------------------------------
;; In modes with where "lines" are not straightforward --
;; like shells, prompts, visual-line-mode, etc. -- evil
;; navigation commands don't always work how I want.
;;
;; Here I add hooks to those modes that fix up evil navigation
;; behavior.
;; ============================================================

;; evil-mode isn't great at respecting visual lines.
;; This tries to remedy that. We'll add this to
;; visual-line-mode-hook later
(defun carljv/set-visual-line-evil ()
  "Make evil work better in 'visual-line-mode'."
  (when evil-mode
    (evil-define-key 'motion visual-line-mode-map "j" #'evil-next-visual-line)
    (evil-define-key 'motion visual-line-mode-map "k" #'evil-previous-visual-line)
    (evil-define-key 'motion visual-line-mode-map "$" #'evil-end-of-visual-line)
    (evil-define-key 'motion visual-line-mode-map "^" #'evil-first-non-blank-of-visual-line)
    (evil-define-key 'motion visual-line-mode-map "0" #'evil-beginning-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-a") #'evil-beginning-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-e") #'evil-end-of-visual-line)
    (evil-define-key 'insert visual-line-mode-map (kbd "C-k") #'kill-visual-line)))


;; In eshell we want evil/vim beginning-of-line keys
;; to go to just after the prompt, not the beginning of the
;; actual line on the screen (i.e., before the prompt).
;; We'll add this to eshell-mode-hook later.
(defun carljv/set-eshell-evil ()
  "Provide reasonable evil-mode motions to 'eshell-mode'."
  (evil-define-key 'insert eshell-mode-map (kbd "C-a") #'eshell-bol)
  (evil-define-key 'motion eshell-mode-map (kbd "0")   #'eshell-bol)
  (evil-define-key 'motion eshell-mode-map (kbd "^")   #'eshell-bol))


;; In comint-mode we want evil/vim beginning-of-line keys
;; to go to just after the prompt, not the beginning of the
;; actual line on the screen (i.e., before the prompt).
;; We'll add this to comint-mode-hook later.
(defun carljv/set-comint-evil ()
  "Provide reasonable comint-mode motions to 'eshell-mode'."
  (evil-define-key 'motion comint-mode-map (kbd "0") #'comint-bol)
  (evil-define-key 'motion comint-mode-map (kbd "^") #'comint-bol))



;; ============================================================
;; Ivy, Counsel, and Swiper
;; ------------------------
;; Ivy, counsel and swiper provide improved autocompletion
;; for minibuffer interaction (finding commands, files,
;; searching in buffers, etc.)
;; ============================================================

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
  ("<f2> i"  . counsel-info-symbol-lookup)
  ("<f2> u"  . counsel-unicode-char)
  ("C-c C-r" . ivy-resume)
  ("s-b"   . ivy-switch-buffer))


;; ============================================================
;; Magit
;; ----
;; Magit is a git porcelain. Doesn't need much customization,
;; but I do add a keybinding for bringing up the Git Status
;; buffer.
;; ============================================================
(use-package magit
  :defer t
  :init (global-set-key (kbd "C-c g s") #'magit-status))


;; ============================================================
;; Flycheck
;; --------
;; Flycheck is a linter backend, and a better alternative to
;; Flymake. I add this to specific major modes explicitly.
;;
;; I explicitly set the python checker executables, because
;; flycheck can fail to find them if exec-path-from-shell
;; hasn't run before flycheck has loaded. If this happens
;; flycheck will disable them.
;; ============================================================

(defconst carljv/lintr-linters
  "with_defaults(
     line_length_linter(100),
     object_usage_linter = NULL,
     open_curly_linter = NULL,
     closed_curly_linter = NULL)"

  "Linter settings for lintr with flycheck.")

(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq
   flycheck-r-linter-executable "/usr/local/bin/R"
   flycheck-python-flake8-executable (concat carljv/anaconda-dir "bin/flake8")
   flycheck-python-mypy-executable (concat carljv/anaconda-dir "bin/mypy")
   flycheck-lintr-linters carljv/lintr-linters)
  :hook
  (ess-mode . flycheck-mode)
  (elpy-mode . flycheck-mode)
  (cider-mode . flycheck-mode)
  :commands flycheck-mode
  :diminish flycheck-mode)


;; ============================================================
;; Company
;; ------
;; Company is an auto-completion mode. Again, I hook it to
;; specific modes explicitly.
;;
;; Again, for Python, we'll rely on elgot and the language-
;; server protocol for this.
;; ============================================================

(use-package company
  :defer t
  :init
  (carljv/add-to-hooks '(cider-mode-hook
			 cider-repl-mode-hook
			 comint-mode-hook
			 ess-mode-hook
			 inferior-ess-mode-hook)
   #'company-mode)
  :diminish company-mode
  :bind (:map company-active-map
	      ("C-d" . company-show-doc-buffer)))


;; ============================================================
;; Projectile
;; ----------
;; Projectile is a mode for managing "projects".
;; ============================================================

(use-package projectile
  :defer t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  :bind
  (:map projectile-mode-map ("s-p" . projectile-command-map))
  :diminish projectile-mode)


;; ============================================================
;; Eldoc
;; ============================================================
(use-package eldoc
  :defer t
  :hook
  cider-mode
  cider-repl-mode
  emacs-lisp-mode
  :diminish eldoc-mode)


;; ============================================================
;; Visual Line mode
;; ----------------
;; Visual line mode is mostly used for "prose" (instead of
;; "code") buffers.
;; ============================================================

(defun carljv/toggle-right-margin ()
  "Set a margin on the right for prose buffers."
  (interactive)
  (if (null (cdr (window-margins)))
      (set-window-margins 5 (max 5 (- (window-body-width) 90)))
    (set-window-margins nil 0 0)))

(defun carljv/set-prose-buffer ()
  "Set up a buffer intended for non-code (prose) content."
  (progn
    (flyspell-mode 1)
    (carljv/set-visual-line-evil)
    (carljv/toggle-right-margin)))

(add-hook 'visual-line-mode-hook 'carljv/set-prose-buffer)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                                BUILT-IN EMACS MODES
;;                                --------------------
;; These are modes that ship with Emacs, but whose behavior I want to customize.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ============================================================
;; Org-mode
;; ============================================================

(use-package org
  :defer t
  :config
  (visual-line-mode 1)
  (setq-default org-babel-load-languages
		'((R . t)
		  (cpp . t)
		  (emacs-lisp . t)
		  (julia . t)
		  (python . t)
		  (shell . t)
		  (sql .t))))

(add-hook 'org-mode 'carljv/set-prose-buffer)


;; ============================================================
;; Undo Tree
;; ============================================================

;; Don't show undo-tree mode in the mode line.
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)


;; ============================================================
;; Comint mode
;; ============================================================
(use-package comint
  :defer t
  :init
  (carljv/set-comint-evil)
  :bind (:map comint-mode-map
	      ("<up>"   . comint-previous-matching-input-from-input)
	      ("<down>" . comint-next-matching-input-from-input)
	      ("C-p"    . comint-previous-matching-input-from-input)
	      ("C-n"    . comint-next-matching-input-from-input)
	      ("C-r"    . comint-history-isearch-backwards-regexp)))



;; ============================================================
;; Eshell mode
;; ============================================================

(use-package eshell
  :defer t
  :init (carljv/set-eshell-evil)
  :config
  (setq eshell-visual-commands
	'("less" "tmux" "htop" "top" "bash" "zsh" "fish")
	eshell-visual-subcommands
	'("git" "log" "l" "diff" "show"))
  :bind
  (:map eshell-mode-map
	("C-a" . eshell-bol)))

(global-set-key (kbd "C-c $") #'eshell)


;; ============================================================
;; Ielm
;; ============================================================

(global-set-key (kbd "C-c >") #'ielm)



;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                            LANGUAGE-SPECIFIC MODES
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ============================================================
;; ESS (R)
;; ============================================================

(defun carljv/r-pipe-operator ()
  "Insert the R pipe operator, %>%."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))


;; I want to be able to quickly kill R help buffers.
;; I'll bind this function to "q" when ESS starts.
(defun carljv/kill-ess-help-window()
  "Kill ESS help windows."
  (interactive)
  (quit-restore-window nil 'kill))


(use-package ess
  :after exec-path-from-shell
  :commands (R-mode)
  :init
  (require 'ess-site)
  :config
  (setq
   inferior-r-args "--no-restore"
   ess-use-flymake nil
   ess-use-company t
   ess-style 'RStudio
   ess-indent-with-fancy-comments nil)
  (dolist (kw '(ess-R-fl-keyword:fun-defs
		ess-R-fl-keyword:keywords
		ess-fl-keyword:fun-calls
		ess-fl-keyword:delimiters))
    (setf (alist-get kw ess-R-font-lock-keywords) t))
  :bind
  (:map
   ess-r-mode-map
   ("C-." . carljv/r-pipe-operator)
   :map inferior-ess-mode-map
   ("C-." . carljv/r-pipe-operator)
   :map ess-help-mode-map
   ("q" . carljv/kill-ess-help-window)))

(add-hook 'ess-r-mode '(lambda () (setq indent-tabs-mode nil)))


;; Define some pretty symbols and activate them
;; when ESS runs.
(defun carljv/make-r-pretty ()
  (dolist (pretty-pair '(("%>%" . ?⧐)
			("<-" . ?⟵)
			("->" . ?⟶)
			("%in%" . ?∈)))
	  (add-to-list 'prettify-symbols-alist pretty-pair))
  (prettify-symbols-mode))

(carljv/add-to-hooks
 '(ess-mode-hook ess-inferior-mode-hook)
 'carljv/make-r-pretty)


;; ============================================================
;; Polymode / RMarkdown
;; ============================================================

(use-package polymode
  :defer t
  :mode
  ("\\.[rR]md\\'" . poly-markdown+R-mode )
  ("\\.md\\'" . poly-markdown-mode)
  :init
  (use-package poly-R
    :defer t)
  (use-package poly-markdown
    :defer t))


;; ============================================================
;; Python
;; ============================================================

;; For the moment, I'm using conda for Python
;; environment handling. Conda keeps its envs
;; in a central directory.


(defconst carljv/conda-env-dir
  (concat carljv/anaconda-dir "envs/"))


;; elpy has separate keybindings for sending the statement
;; under the point to the shell and for sending a region
;; to the shell. This delegates to one or the other command
;; depending on whether there's an active region or not.
(defun carljv/elpy-eval-region-or-line ()
  "Send region (if selected) or current line to Python shell."
  (interactive)
  (if (region-active-p)
      (elpy-shell-send-region-or-buffer)
    (elpy-shell-send-current-statement)))


;; pyvenv-activate doesn't work when you're using Conda environments.
;; this is a conda-suitable version of that command.
(defun carljv/conda-env-activate (envname)
  "Bring up a list of available Conda environments to activate and workon.

After selecting ENVNAME, work on that."
  (interactive
   (let* ((conda-env-root carljv/conda-env-dir)
	  (conda-env-root-subdirs (directory-files conda-env-root))
	  (conda-env-names (remove "." (remove ".." conda-env-root-subdirs)))
	  (completion-ignore-case t))
     (list (completing-read "Activate conda env: " conda-env-names nil t))))
  (let ((envdir (concat (expand-file-name "~/opt/anaconda3/envs/") envname "/")))
    (message (concat "Activating " envdir))
    (pyvenv-activate envdir)))


(use-package elpy
  :defer t
  :commands elpy-mode
  :init (add-hook 'python-mode-hook #'elpy-mode)
  :config
  (setenv "WORKON_HOME" carljv/conda-env-dir)
  (setq
   elpy-modules (delq 'elpy-module-flymake elpy-modules)
   elpy-modules '(elpy-module-sane-defaults
		  elpy-module-company
		  elpy-module-eldoc
		  elpy-module-pyvenv)
   elpy-rpc-backend "jedi")
  (elpy-enable)
  :bind (:map elpy-mode-map
	      ("C-<return>" . nil)
	      ("C-<return>" . carljv/elpy-eval-region-or-line)
	      ("C-c e w"    . carljv/conda-env-activate)
	      ("C-c e d"    . pyvenv-deactivate)
	      ("C-c ."      . elpy-goto-definition)
	      ("M-<left>"     . left-word)
	      ("M-<right>"    . right-word))
  :diminish elpy-mode)


;; ============================================================
;; Clojure
;; ============================================================
(use-package flycheck-clj-kondo
  :defer t)


(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))


(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer nil
	cider-pprint-fn 'fipp
	cider-repl-use-pretty-printing t
	nrepl-hide-special-buffers t
	cider-saved-file-on-load t)
  :hook clojure-mode
  :diminish clojure-mode)


;; ============================================================
;; Common Lisp / SLIME
;; ============================================================

(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"
	slime-header-line-p nil
	slime-load-failed-fasl 'never
	slime-contribs '(slime-banner
			 slime-fancy
			 slime-quicklisp
			 slime-company))
  :config
  (slime-setup))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                                   CODA
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ============================================================
;; Compute and display startup time
;; ============================================================

(setq *carljv/startup-time*
      (float-time (time-subtract (current-time) *carljv/emacs-load-start*)))

(setq initial-scratch-message
      (format ";; Welcome to Emacs!\n%s;; Startup time: %3.2f seconds.\n\n"
	      (if (daemonp) (concat "Server      : " (daemonp) "\n") "")
	      *carljv/startup-time*))

;; init.el ends here
