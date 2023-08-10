;;; init.el --- Isaac's Emacs configuration

;; Copyright (c) 2016-2022 Isaac Gu

;; Author: Isaac Gu <imjiaxi@gmail.com>
;; URL: https://github.com/gujiaxi/.emacs.d
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a personal Emacs configuration.

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
;; Basic settings
;; -------------------------------------------------------------------

;; Personal Infomation
(setq user-full-name "Isaac Gu")
(setq user-mail-address "imjiaxi@gmail.com")

;; set unicode encoding
(prefer-coding-system 'utf-8)

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

;; initial buffer
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

;; use y/n instead of yes/no
(setq use-short-answers t)

;; word wrap for CJK
(setq word-wrap-by-category t)

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
(setq org-directory "~/Library/CloudStorage/Dropbox/Documents/OrgDir/")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))


;; -------------------------------------------------------------------
;; Built-in packages
;; -------------------------------------------------------------------

;; ----- customizations -----

;; abbrev [built-in]
(setq save-abbrevs nil)

;; calendar [built-in]
(setq calendar-location-name "Beijing, China")
(setq calendar-chinese-all-holidays-flag t)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
(setq diary-file (expand-file-name "diary.org" org-directory))
(global-set-key (kbd "C-c k") 'calendar)

;; dired [built-in]
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; display-line-numbers [built-in]
(setq display-line-numbers-type 'relative)
(mapc (lambda (hook) (add-hook hook 'display-line-numbers-mode))
      (list 'prog-mode-hook 'bibtex-mode-hook))

;; electric [built-in]
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; epa [built-in]
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-pinentry-mode 'loopback)

;; epg [built-in]
(setq epg-gpg-minimum-version "100")

;; flyspell [built-in]
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; grep [built-in]
(global-set-key (kbd "C-c g") 'zrgrep)

;; help-at-pt [bilt-in]
(custom-set-variables
 '(help-at-pt-timer-delay 0.5)
 '(help-at-pt-display-when-idle t))

;; hideshow [built-in]
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; org-mode [built-in]
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(defun org-publish-site ()
  "A function for publishing a site.
The site configuration is defined in index.org."
  (interactive)
  (let ((index-file (expand-file-name "index.org" org-directory)))
    (find-file index-file)
    (org-babel-load-file index-file)
    (kill-this-buffer)))
(global-set-key (kbd "C-c p") 'org-publish-site)

;; savehist [built-in]
(savehist-mode t)

;; recentf [built-in]
(setq recentf-max-saved-items 500)
(recentf-mode t)

;; reftex [built-in]
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-horizontally t)

;; saveplace [built-in]
(save-place-mode t)

;; tab-bar [built-in]
(setq tab-bar-show 1)

;; tramp [built-in]
(setq tramp-backup-directory-alist backup-directory-alist)

;; use-package [built-in]
(setq use-package-always-ensure t)

;; windmove [built-in]
(windmove-default-keybindings)

;; winner [built-in]
(winner-mode t)

;; zone [built-in]
(autoload 'zone-when-idle "zone" nil t)
(zone-when-idle 18000)

;; ----- key binding -----

(when (display-graphic-p)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)


;; -------------------------------------------------------------------
;; Package Bootstrap
;; -------------------------------------------------------------------

;; package repository
(setq package-archives
      '(("gnu" . "https://mirrors.163.com/elpa/gnu/")
        ("melpa" . "https://mirrors.163.com/elpa/melpa/")))
(package-initialize)


;; -------------------------------------------------------------------
;; Flymake
;; -------------------------------------------------------------------

;; flymake [built]
(use-package flymake
  :hook (prog-mode . flymake-mode))


;; -------------------------------------------------------------------
;; Org Mode
;; -------------------------------------------------------------------

;; org [built-in]
(use-package org
  :defer t
  :custom
  ;; general
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-startup-folded nil)
  (org-src-preserve-indentation t)
  (org-image-actual-width nil)
  (org-catch-invisible-edits 'show-and-error)
  (org-confirm-babel-evaluate nil)
  (org-export-with-archived-trees nil)
  (org-export-html-style-include-scripts nil)
  (org-export-html-style-include-default nil)
  (org-html-postamble t)
  (org-html-postamble-format '(("en" "Edited by %a on %C")))
  (org-archive-location (expand-file-name "archive.org::" org-directory))
  (org-refile-targets '((org-agenda-files :level . 1)
                        (nil :level . 1)))
  (org-priority-faces '((?A . (:foreground "red" :weight bold))
                        (?B . (:foreground "orange" :weight bold))
                        (?C . (:foreground "yellow" :wegith bold))))
  ;; org-agenda
  (org-agenda-files (list (expand-file-name "agenda.org" org-directory)))
  (org-agenda-include-diary nil)
  (org-agenda-custom-commands
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
  (org-default-notes-file (expand-file-name "agenda.org" org-directory))
  (org-capture-templates
   '(("a" "Appt" entry (file+headline "agenda.org" "Appointments")
      "* %?\n%t")
     ("t" "Task" entry (file+headline "agenda.org" "Tasks")
      "* TODO %?\n%U\n%a")
     ("n" "Note" entry (file+headline "notes.org" "Inbox")
      "* %?\n%U\n%a")
     ("j" "Journal" plain (file+olp+datetree "journal.org")
      "%U %?\n")
     ("p" "Publish" plain (file "p-scratch.org")
      "%?\n\n%U\n-----")))
  :config
  ;; ox-html
  (use-package htmlize)
  (setq org-html-htmlize-output-type 'css)
  ;; ox-latex
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-logfiles-extensions "tex")
    (add-to-list 'org-latex-packages-alist '("" "listings" nil))
    (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" nil))
    (add-to-list 'org-latex-packages-alist '("scheme=plain" "ctex" nil))
    (setq org-latex-hyperref-template "\\hypersetup{pdfauthor={%a},pdftitle={%t},hidelinks}"))
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (setq org-latex-listings t)
  (setq org-latex-listings-options
        '(("breaklines" "true")
          ("basicstyle" "\\ttfamily")
          ("frame" "single")))
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (calc . t)
     (emacs-lisp . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (R . t)
     (scheme . t)
     (shell . t)))
  ;; key bindings in org-mode
  (unbind-key "C-'" org-mode-map))


;; -------------------------------------------------------------------
;; TeX
;; -------------------------------------------------------------------

;; auctex
(use-package tex
  :ensure auctex
  :after latex
  :config
  (add-to-list 'TeX-command-list '("Latexmk" "latexmk -xelatex -quiet %s" TeX-run-command nil t
                                   :help "Run latexmk"))
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'TeX-output-mode 'emacs))
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-clean-confirm nil))


;; -------------------------------------------------------------------
;; Theme
;; -------------------------------------------------------------------

;; ----- color theme -----

;; modus-themes: operandi for day and vivendi for night
(load-theme 'modus-operandi)
(setq modus-themes-no-mixed-fonts t)
(run-at-time "09:00" (* 60 60 24) #'load-theme 'modus-operandi)
(run-at-time "17:30" (* 60 60 24) #'load-theme 'modus-vivendi)

;; ----- mode-line -----

;; nyan-mode
(use-package nyan-mode)

;; Crafted for battery-mode, vc-mode and evil-mode
(setq-default mode-line-format
              (list '(:eval (when (boundp 'evil-mode)
                              (propertize
                               (evil-generate-mode-line-tag evil-state)
                               'face '(:inherit font-lock-function-name-face))))
                    'mode-line-mule-info 'mode-line-modified 'mode-line-remote " "
                    '(:eval (propertize
                             " %b "
                             'face (if (buffer-modified-p)
                                       '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
                                     '(:background "#268bd2" :foreground "#fdf6e3" :weight light))
                             'help-echo (buffer-file-name)))
                    '(:eval (when (boundp 'nyan-mode) (nyan-create)))
                    '(:propertize
                      " %p L%l "
                      face (:background "gray30" :foreground "#fdf6e3"))
                    '(:propertize
                      " (%m)"
                      face (:inherit font-lock-function-name-face :weight bold))
                    '(:eval (when (> (window-width) 64)
                              (concat
                               (when vc-mode
                                 (propertize
                                  vc-mode
                                  'face '(:inherit font-lock-keyword-face :weight bold)))
                               (propertize
                                (format-time-string " %p·%R ")
                                'help-echo (format-time-string "%F %A"))
                               (replace-regexp-in-string
                                "%" "%%" battery-mode-line-string))))
                    "-%-"))


;; -------------------------------------------------------------------
;; Evil
;; -------------------------------------------------------------------

;; evil
(use-package evil
  :config
  (evil-mode)
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
  (unbind-key "SPC" evil-normal-state-map) ;; reserve for leader key
  (unbind-key "TAB" evil-motion-state-map) ;; reserve for org-mode
  (unbind-key "C-n" evil-insert-state-map) ;; reserve for auto complete
  (unbind-key "C-p" evil-insert-state-map) ;; reserve for auto complete
  :custom (evil-undo-system 'undo-redo)
  (evil-want-abbrev-expand-on-insert-exit nil)
  :bind (("<f5>" . evil-make)
         :map evil-normal-state-map
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         ("SPC w" . save-buffer)
         ;; tab-bar-mode
         ("g o" . tab-bar-new-tab)
         ("g c" . tab-bar-close-tab)
         ("g t" . tab-bar-switch-to-next-tab)
         ("g T" . tab-bar-switch-to-prev-tab)
         :map evil-visual-state-map
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         :map evil-emacs-state-map
         ("C-w" . evil-window-map)))

;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :after evil
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)
         :map evil-normal-state-map
         ("SPC c SPC" . evilnc-comment-or-uncomment-lines)))

;; evil-surround
(use-package evil-surround
  :config (global-evil-surround-mode))

;; evil-matchit
(use-package evil-matchit
  :config (global-evil-matchit-mode))


;; -------------------------------------------------------------------
;; Completion System
;; -------------------------------------------------------------------

;; Vertico
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  (use-package orderless
    :custom (completion-styles '(substring orderless)))
  (use-package marginalia
    :init (marginalia-mode)
    :custom (marginalia-annotator-registry
             '((command marginalia-annotate-binding builtin none))))
  (use-package consult
    :custom (consult-preview-key "M-.")
    :bind (("C-x b" . consult-buffer)
           ("C-s" . consult-line)
           ("M-y" . consult-yank-pop)
           ("C-c i" . consult-imenu))))


;; -------------------------------------------------------------------
;; Auto Complete
;; -------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  :config (global-corfu-mode)
  (use-package cape
    :config
    (add-to-list 'completion-at-point-functions #'cape-abbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)))


;; -------------------------------------------------------------------
;; LSP (Language Server Protocol)
;; -------------------------------------------------------------------

;; eglot
(use-package eglot
  :custom
  (eglot-autoreconnect nil)
  (eglot-autoshutdown t)
  :hook (python-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c C-e f" . eglot-format)
         ("C-c C-e i" . eglot-code-action-organize-imports)))


;; -------------------------------------------------------------------
;; Deft
;; -------------------------------------------------------------------

(use-package deft
  :custom
  (deft-directory org-directory)
  (deft-extensions '("org" "md" "tex"))
  (deft-default-extension "org")
  (deft-ignore-file-regexp "\\(?:index\\|readme\\|sitemap\\|archive\\)")
  (deft-recursive t)
  (deft-auto-save-interval 0)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'deft-mode 'emacs))
  :hook (deft-open-file . deft-filter-clear)
  :bind ("C-c d" . deft))


;; -------------------------------------------------------------------
;; Markdown
;; -------------------------------------------------------------------

;; markdown-mode
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom
  (markdown-command "pandoc --quiet --mathjax --no-highlight -f markdown")
  (markdown-css-paths '("https://isaac.ttm.sh/org/css/md.css"))
  (markdown-xhtml-header-content "\n<meta name=\"viewport\" content=\"width=device-width\">\n<script src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-MML-AM_CHTML\" async></script>"))


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
  (global-aggressive-indent-mode t))

;; avy
(use-package avy
  :custom (avy-background t)
  :bind (("C-'" . avy-goto-char-2)
         :map evil-normal-state-map
         ("SPC s" . avy-goto-char-2)))

;; bind-dict
(use-package bing-dict
  :bind ("C-c l" . bing-dict-brief))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; quickrun
(use-package quickrun
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'quickrun--mode 'emacs))
  (quickrun-add-command "c++/clang++"
    '((:exec . ("%c -std=c++14 -x c++ %o -o %e %s" "%e %a")))
    :override t)
  :bind ("C-c r" . quickrun))

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
  :bind (("C-c s" . sr-speedbar-toggle)
         :map speedbar-mode-map
         ("a" . speedbar-toggle-show-all-files)
         ("l" . sr-speedbar-refresh-toggle)))

;; symon
(use-package symon
  :custom (symon-delay 33)
  :config (symon-mode))

;; tempel
(use-package tempel
  ;; User-defined templates are put in `~/.emacs.d/templates'.
  :init (global-tempel-abbrev-mode)
  :config (use-package tempel-collection)
  :bind (:map tempel-map
              ("<tab>" . tempel-next)
              ("<S-tab" . tempel-previous)))

;; which-key
(use-package which-key
  :config (which-key-mode))


;; -------------------------------------------------------------------
;; Other settings
;; -------------------------------------------------------------------

;; ----- macOS -----

(when (eq system-type 'darwin)
  ;; environment path
  (let ((envpath '("/usr/local/bin/" "/Library/TeX/texbin/" "~/Library/Python/3.11/bin/")))
    (setenv "PATH" (mapconcat 'identity (add-to-list 'envpath (getenv "PATH") t) ":"))
    (setq exec-path (append envpath exec-path)))
  ;; keys
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;; display
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when (display-graphic-p) (menu-bar-mode 1))
  ;; font
  (when (member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :font "Menlo-14"))
  (when (member "PragmataPro" (font-family-list))
    (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-table nil :family "PragmataPro"))))
  ;; python
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3")
  (setq python-shell-completion-native-disabled-interpreters '("python3"))
  (setq python-indent-guess-indent-offset-verbose nil))


;;; init.el ends here
