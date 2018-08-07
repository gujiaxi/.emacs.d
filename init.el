;;; init.el --- jiaxi's configuration

;; Copyright (c) 2016-2018 Jiaxi Gu

;; Author: Jiaxi Gu <imjiaxi@gmail.com>
;; URL: https://github.com/gujiaxi/.emacs.d
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This is my personal Emacs configuration.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; -------------------------------------------------------------------
;; Initialization
;; -------------------------------------------------------------------

(package-initialize)


;; -------------------------------------------------------------------
;; Basic settings
;; -------------------------------------------------------------------

;; Personal Infomation
(setq user-full-name "Jiaxi Gu")
(setq user-mail-address "imjiaxi@gmail.com")

;; set unicode encoding
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

;; no lockfile
(setq create-lockfiles nil)

;; backup and autosave
(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
(setq version-control t)
(setq delete-old-versions t)

;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/")

;; clean startup
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; no ring-bell
(setq ring-bell-function 'ignore)

;; nice frame
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; nice scrolling
(pixel-scroll-mode)
(setq scroll-margin 5)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; enable syntax highlight
(global-font-lock-mode t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default c-basic-offset 4)

;; delete selection
(delete-selection-mode t)

;; auto revert external changes
(global-auto-revert-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

;; show which function current line belongs to
(which-function-mode)

;; highlight current line
(global-hl-line-mode t)

;; display time in mode line
(display-time-mode t)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq system-time-locale "C")

;; display battery status
(display-battery-mode t)

;; set frame title
(setq frame-title-format "%b")

;; set a larger kill ring
(setq kill-ring-max 200)

;; use system clipboard
(setq save-interprogram-paste-before-kill t)

;; confirm before quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;; custom directory
(setq org-directory "~/Dropbox/Documents/EmacsFiles/")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))


;; -------------------------------------------------------------------
;; Built-in packages
;; -------------------------------------------------------------------

;; ----- plugin settings -----

;; abbrev [built-in]
(setq save-abbrevs nil)

;; calendar [built-in]
(setq calendar-location-name "Beijing, China")
(setq calendar-chinese-all-holidays-flag t)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
(setq cal-html-directory (expand-file-name "calendar" org-directory))
(setq diary-file (expand-file-name "org/diary.org" org-directory))
(global-set-key (kbd "C-c k") 'calendar)

;; dired [built-in]
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; display-line-numbers [built-in]
(setq display-line-numbers-type 'relative)
(mapc (lambda (hook) (add-hook hook 'display-line-numbers-mode))
      (list 'prog-mode-hook 'org-mode-hook 'text-mode-hook
            'bibtex-mode-hook 'markdown-mode-hook 'ess-mode-hook
            'LaTeX-mode-hook))

;; electric [built-in]
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; epa [built-in]
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-pinentry-mode 'loopback)

;; epg [built-in]
(setq epg-gpg-minimum-version "100")

;; flymake [built-in]
(add-hook 'prog-mode-hook 'flymake-mode)

;; flyspell [built-in]
(setq ispell-program-name "aspell")
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; grep [built-in]
(global-set-key (kbd "C-c g") 'zrgrep)

;; hideshow [built-in]
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ido [built-in]
(ido-mode t)
(ido-everywhere t)

;; org [built-in]
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(defun org-publish-site ()
  "A function for publishing a site.
The site configuration is defined in index.org."
  (interactive)
  (let ((index-file (expand-file-name "org/index.org" org-directory)))
    (find-file index-file)
    (org-babel-load-file index-file)
    (kill-this-buffer)))
(global-set-key (kbd "C-c p") 'org-publish-site)

;; paren [built-in]
(show-paren-mode t)

;; savehist [built-in]
(savehist-mode t)

;; recentf [built-in]
(setq recentf-max-saved-items 500)
(recentf-mode t)

;; reftex [built-in]
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-horizontally t)
(setq reftex-default-bibliography (list (expand-file-name "org/bib/main.bib" org-directory)))

;; saveplace [built-in]
(save-place-mode t)

;; tramp [built-in]
(setq tramp-backup-directory-alist backup-directory-alist)

;; windmove [built-in]
(windmove-default-keybindings)

;; winner [built-in]
(winner-mode t)

;; zone [built-in]
(autoload 'zone-when-idle "zone" nil t)
(zone-when-idle 18000)

;; ----- key binding -----

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


;; -------------------------------------------------------------------
;; Package bootstrap
;; -------------------------------------------------------------------

;; ----- package archives -----

(setq package-archives
      '(("gnu" . "https://elpa.emacs-china.org/gnu/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org" . "https://elpa.emacs-china.org/org/")))

;; ----- use-package -----

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)


;; -------------------------------------------------------------------
;; Orgmode
;; -------------------------------------------------------------------

;; basic org options
(setq org-startup-with-inline-images nil)
(setq org-image-actual-width nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-archived-trees nil)
(setq org-export-html-style-include-scripts nil)
(setq org-export-html-style-include-default nil)
(setq org-html-postamble t)
(setq org-archive-location (expand-file-name "org/archive.org::" org-directory))
(setq org-refile-targets '((org-agenda-files :level . 1)
                           (nil :level . 1)))
(setq org-html-postamble-format '(("en" "&copy; %a / %C")))
(setq org-priority-faces '((?A . (:foreground "red" :weight bold))
                           (?B . (:foreground "orange" :weight bold))
                           (?C . (:foreground "yellow" :wegith bold))))

;; org-agenda
(setq org-agenda-files (list (expand-file-name "org/agenda.org" org-directory)))
(setq org-agenda-include-diary nil)
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((tags "PRIORITY={A}"
                ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority:")))
          (agenda "" ((org-agenda-span (quote day))))
          (alltodo ""
                   ((org-agenda-skip-function '(or (org-agenda-skip-subtree-if 'scheduled)
                                                   (org-agenda-skip-subtree-if 'todo '("WAIT"))
                                                   (org-agenda-skip-subtree-if 'regexp "\\[#A\\]")))
                    (org-agenda-overriding-header "Others tasks:")))
          (todo "WAIT"
                ((org-agenda-skip-function '(org-agenda-skip-subtree-if 'regexp "\\[#A\\]"))
                 (org-agenda-overriding-header "Postponed tasks:")))))))

;; org-capture
(setq org-default-notes-file (expand-file-name "org/agenda.org" org-directory))
(setq org-capture-templates
      '(("a" "Appt" entry (file+headline "org/agenda.org" "Appointments")
         "* %?\n%t")
        ("t" "Task" entry (file+headline "org/agenda.org" "Tasks")
         "* TODO %?\n%U\n%a")
        ("n" "Note" entry (file+headline "org/notes.org" "Inbox")
         "* %?\n%U\n%a")
        ("j" "Journal" plain (file+datetree "org/journal.org")
         "%U %?\n")
        ("p" "Publish" plain (file "org/p-scratch.org")
         "%?\n\n%U\n-----")))

;; org
(use-package org
  :ensure org-plus-contrib
  :after org
  :config
  (use-package htmlize)
  ;; ox-html
  (setq org-html-htmlize-output-type 'css)
  ;; ox-latex
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-logfiles-extensions "tex")
    (add-to-list 'org-latex-packages-alist '("" "amsthm"))
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("scheme=plain" "ctex")))
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(("breaklines" "true")
                                     ("basicstyle" "\\ttfamily")
                                     ("numbers" "left")
                                     ("frame" "single")))
  ;; ox-bibtex
  (require 'ox-bibtex)
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (calc . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (R . t)
     (scheme . t)
     (shell . t))))


;; -------------------------------------------------------------------
;; TeX
;; -------------------------------------------------------------------

;; auctex
(use-package tex
  :ensure auctex
  :after latex
  :config
  (add-to-list 'TeX-command-list '("Latexmk" "latexmk -pdf -quiet %s" TeX-run-command nil t :help "Run latexmk"))
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'TeX-output-mode 'emacs))
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-clean-confirm nil))

;; company-auctex
(use-package company-auctex
  :after tex
  :config (company-auctex-init))


;; -------------------------------------------------------------------
;; Theme
;; -------------------------------------------------------------------

;; ----- color theme -----

;; zenburn-theme
(use-package zenburn-theme
  :init
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg"   . "#3F3F3F")
      ("zenburn-bg-1" . "#5F5F5F")))
  :config
  (load-theme 'zenburn t)
  (set-face-attribute 'fringe nil :background "#3F3F3F"))

;; ----- mode-line -----

;; Crafted for battery-mode, evil-mode and eyebrowse-mode
(setq-default mode-line-format
              (list '(:eval (propertize (evil-generate-mode-line-tag evil-state) 'face '(:inherit font-lock-function-name-face)))
                    "%e"
                    'mode-line-mule-info
                    'mode-line-modified
                    'mode-line-remote " "
                    '(:eval (propertize " %b " 'face (if (buffer-modified-p) '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
                                                       '(:background "#268bd2" :foreground "#fdf6e3" :weight light))
                                        'help-echo (buffer-file-name)))
                    '(:propertize " %p/%I " face (:background "gray30" :foreground "#fdf6e3")
                                  help-echo (count-words--buffer-message))
                    '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator) " ")))
                    '(:eval (propertize (format-time-string "%p·%H:%M ") 'help-echo (format-time-string "%F %a") 'face '(:inherit font-lock-doc-face :slant normal)))
                    'battery-mode-line-string
                    '(:propertize (which-func-mode (" " which-func-format)))
                    '(:eval (when (> (window-width) 70)
                              (propertize " {%m}" 'face '(:weight normal))))
                    '(:eval (when (and (> (window-width) 70) vc-mode)
                              (propertize vc-mode 'face '(:inherit font-lock-keyword-face :weight bold))))
                    "-%-"))


;; -------------------------------------------------------------------
;; Evil
;; -------------------------------------------------------------------

;; evil
(use-package evil
  :config
  (evil-mode t)
  (mapc (lambda (my-mode) (evil-set-initial-state my-mode 'emacs))
        (list 'calendar-mode 'comint-mode 'completion-mode
              'dired-mode 'diff-mode 'epa-info-mode
              'epa-key-list-mode 'eshell-mode 'eww-mode
              'eww-bookmark-mode 'help-mode
              'inferior-emacs-lisp-mode 'inferior-python-mode
              'Info-mode 'message-mode 'newsticker-treeview-mode
              'process-menu-mode 'profiler-report-mode
              'shell-mode 'speedbar-mode 'special-mode
              'term-mode))
  :custom (evil-want-abbrev-expand-on-insert-exit nil)
  :bind (("<f5>" . evil-make)
         :map evil-normal-state-map
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         :map evil-visual-state-map
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         :map evil-emacs-state-map
         ("C-w" . evil-window-map)))

;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode t))

;; evil-matchit
(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))


;; -------------------------------------------------------------------
;; Helm
;; -------------------------------------------------------------------

;; helm
(use-package helm
  :defer 2
  :config
  (require 'helm-config)
  (helm-mode)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'grep-mode 'emacs)
    (evil-set-initial-state 'helm-grep-mode 'emacs))
  :custom
  (helm-full-frame nil)
  (helm-split-window-inside-p t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-follow-mode-persistent t)
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-grep-ag-command "rg --color always --smart-case --no-heading --line-number %s %s %s")
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-s" . helm-occur)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x g" . helm-do-grep-ag)))

;; helm-bibtex
(use-package helm-bibtex
  :after helm
  :custom
  (bibtex-completion-bibliography (list (expand-file-name "org/bib/main.bib" org-directory)))
  (bibtex-completion-notes-symbol "✎")
  (bibtex-completion-notes-path (expand-file-name "org/research-notes.org" org-directory))
  (bibtex-completion-pdf-symbol "⌘")
  (bibtex-completion-library-path (list (expand-file-name "pdf" org-directory)))
  (bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-notes-template-one-file "\n* ${title} (${year})\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:END:\n")
  :bind ("C-c b" . helm-bibtex))


;; -------------------------------------------------------------------
;; Company
;; -------------------------------------------------------------------

;; company
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-global-modes '(not comint-mode eshell-mode org-mode))
  :config
  (setq company-backends (delete 'company-dabbrev company-backends))
  (add-to-list 'company-backends 'company-files)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))


;; -------------------------------------------------------------------
;; Deft
;; -------------------------------------------------------------------

(use-package deft 
  :custom
  (deft-directory (expand-file-name "org" org-directory))
  (deft-extensions '("org" "md" "tex"))
  (deft-default-extension "org")
  (deft-recursive t)
  (deft-auto-save-interval nil)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'deft-mode 'emacs))
  :hook (deft-open-file . deft-filter-clear)
  :bind ("C-c d" . deft))


;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

;; anaconda-mode
(use-package anaconda-mode
  :after python
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; company-anaconda
(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))


;; -------------------------------------------------------------------
;; Haskell
;; -------------------------------------------------------------------

;; haskell-mode
(use-package haskell-mode
  :config
  (with-eval-after-load 'aggressive-indent
    (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'haskell-error-mode 'emacs)
    (evil-set-initial-state 'haskell-interactive-mode 'emacs))
  :hook (haskell-mode . interactive-haskell-mode))


;; -------------------------------------------------------------------
;; Markdown
;; -------------------------------------------------------------------

;; markdown-mode
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom
  (markdown-enable-math t)
  (markdown-command "pandoc --mathml --quiet")
  (markdown-css-paths '("http://jiaxi.sdf.org/static/md.css"))
  (markdown-xhtml-header-content "\n<meta name=\"viewport\" content=\"width=device-width\">"))


;; -------------------------------------------------------------------
;; Web
;; -------------------------------------------------------------------

;; web-mode
(use-package web-mode
  :mode (("\\.html?$" . web-mode)
         ("\\.jsx?$"  . web-mode)
         ("\\.php$"   . web-mode)
         ("\\.s?css$"  . web-mode)))


;; -------------------------------------------------------------------
;; Other packages
;; -------------------------------------------------------------------

;; aggressive-indent
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'latex-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode))

;; avy
(use-package avy
  :custom (avy-background t)
  :bind* ("C-'" . avy-goto-char-2))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; eyebrowse
(use-package eyebrowse
  :custom (eyebrowse-mode-line-separator ",")
  :config
  (eyebrowse-mode t)
  (set-face-attribute 'eyebrowse-mode-line-active nil :inherit font-lock-warning-face))

;; general
(use-package general
  :config
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "c SPC" 'evilnc-comment-or-uncomment-lines
   "s"     'avy-goto-char-2))

;; quickrun
(use-package quickrun
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'quickrun--mode 'emacs))
  :bind ("C-c q" . quickrun))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; sr-speedbar
(use-package sr-speedbar
  :custom
  (speedbar-show-unknown-files t)
  (speedbar-enable-update t)
  (sr-speedbar-skip-other-window-p t)
  (sr-speedbar-auto-refresh t)
  :bind (("<f9>" . sr-speedbar-toggle)
         :map speedbar-mode-map
         ("a" . speedbar-toggle-show-all-files)
         ("l" . sr-speedbar-refresh-toggle)))

;; symon
(use-package symon
  :custom (symon-delay 33)
  :config (symon-mode))

;; which-key
(use-package which-key
  :config (which-key-mode))

;; yasnippet
(use-package yasnippet
  :defer 2
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode t))


;; -------------------------------------------------------------------
;; Other settings
;; -------------------------------------------------------------------

;; ----- Font -----

(when (member "Menlo" (font-family-list))
  (set-face-attribute 'default nil :font "Menlo-14"))
(when (member "PragmataPro" (font-family-list))
  (add-hook 'org-mode-hook (lambda () (setq buffer-face-mode-face '(:family "PragmataPro")) (buffer-face-mode))))

;; ----- Python -----

(setq python-shell-interpreter "python3")
(setq org-babel-python-command "python3")

;; ----- MacOS -----

(when (eq system-type 'darwin)
  (let ((envpath (list "/usr/local/bin/"
                       "/Library/TeX/texbin/"
                       "/Applications/Android Studio.app/Contents/jre/jdk/Contents/Home/bin/")))
    ;; (setenv "PATH" (mapconcat 'identity (push (getenv "PATH") envpath) ":"))
    (setenv "PATH" (mapconcat 'identity (add-to-list 'envpath (getenv "PATH") t) ":"))
    (setq exec-path (append envpath exec-path)))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (menu-bar-mode 1))


;;; init.el ends here
