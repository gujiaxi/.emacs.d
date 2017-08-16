;;; init.el --- jiaxi's configuration

;; Copyright (C) 2017 Jiaxi Gu

;; Author: Jiaxi Gu <imjiaxi@gmail.com>
;; Version: 0.2.0
;; Keywords: emacs, dotfile
;; Package-Requires: ((emacs "25.2"))

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

;;; Commentary:

;; jiaxi's Emacs configuration
;;
;; See documentation on https://git.io/v96UQ

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

;; no startup message
(setq inhibit-startup-message t)

;; no ring-bell
(setq ring-bell-function 'ignore)

;; nice frame
(when (display-graphic-p)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

;; nice scrolling
(setq scroll-margin 5)

;; enable y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; enable syntax highlight
(global-font-lock-mode t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)

;; turn off truncate lines
(setq-default truncate-lines t)

;; deltet selection
(delete-selection-mode t)

;; auto revert external changes
(global-auto-revert-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

;; show which function current line belongs to
(which-function-mode)
(setq which-func-modes (list 'emacs-lisp-mode 'c++-mode 'python-mode))

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

;; dodge the mouse from cursor
(mouse-avoidance-mode 'animate)

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
(setq abbrev-file-name (expand-file-name "snippets/abbrev.el" user-emacs-directory))

;; calendar [built-in]
(setq calendar-location-name "Beijing, China")
(setq calendar-latitude 39.91)
(setq calendar-longitude 116.40)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
(setq cal-html-directory (expand-file-name "calendar" org-directory))
(setq diary-file (expand-file-name "org/diary.org" org-directory))
(global-set-key (kbd "C-c k") 'calendar)

;; compile [built-in]
(global-set-key (kbd "<f6>") 'compile)

;; css-mode [built-in]
(setq css-indent-offset 2)

;; dired [built-in]
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; electric [built-in]
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; epa [built-in]
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setf epa-pinentry-mode 'loopback)

;; epg [built-in]
(setq epg-gpg-minimum-version "100")

;; flyspell [built-in]
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'flyspell-mode)

;; grep [built-in]
(global-set-key (kbd "C-c g") 'zrgrep)

;; hideshow [built-in]
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ido [built-in]
(ido-mode t)
(ido-everywhere t)

;; linum [built-in]
(mapc (lambda (hook) (add-hook hook 'linum-mode))
      (list 'bibtex-mode-hook 'ess-mode-hook 'LaTeX-mode-hook 'markdown-mode-hook
            'org-mode-hook 'prog-mode-hook 'text-mode-hook))

;; newsticker [built-in]
(setq newsticker-url-list-defaults nil)
(setq newsticker-retrieval-interval 0)
(setq newsticker-url-list '(("湾区日报" "http://wanqu.co/feed/" nil nil nil)
                            ("一天世界" "https://blog.yitianshijie.net/feed/" nil nil nil)))

;; rcirc [built-in]
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :nick "caasi"
         :channels ("#archlinux-cn" "#ubuntu-cn"))))

;; org [built-in]
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; paren [built-in]
(show-paren-mode t)

;; savehist [built-in]
(savehist-mode t)

;; recentf [built-in]
(setq recentf-max-saved-items 500)
(recentf-mode t)

;; reftex [built-in]
(setq reftex-plug-into-AUCTeX t)
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


;; -------------------------------------------------------------------
;; Package bootstrap
;; -------------------------------------------------------------------

;; ----- package archives -----

(setq package-archives
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; ----- use-package -----

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)


;; -------------------------------------------------------------------
;; Gnus
;; -------------------------------------------------------------------

;; I use it only for emails
(setq gnus-select-method
      '(nnimap ""
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))
;; send email via SMTP
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
;; more attractive summary view
(when window-system
  (setq gnus-sum-thread-tree-indent          "  ")
  (setq gnus-sum-thread-tree-root            "● ")
  (setq gnus-sum-thread-tree-false-root      "◯ ")
  (setq gnus-sum-thread-tree-single-indent   "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat "%0{%U%R%z%}"
              "%3{│%}" "%1{%d%}" "%3{│%}" "  "
              "%4{%-20,20f%}" "  "
              "%3{│%}" " "
              "%1{%B%}"
              "%s\n"))
;; directories
(setq gnus-use-dribble-file nil)
(setq gnus-startup-file (expand-file-name "gnus/.newsrc" user-emacs-directory))
(setq gnus-default-directory (expand-file-name "gnus" user-emacs-directory))
(setq gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
(setq gnus-dribble-directory (expand-file-name "gnus" user-emacs-directory))
(setq gnus-directory (expand-file-name "gnus/news" user-emacs-directory))
(setq gnus-article-save-directory (expand-file-name "gnus/news" user-emacs-directory))
(setq gnus-kill-files-directory (expand-file-name "gnus/news" user-emacs-directory))
(setq gnus-agent-directory (expand-file-name "gnus/news/agent" user-emacs-directory))
(setq gnus-cache-directory (expand-file-name "gnus/news/cache" user-emacs-directory))
(setq gnus-cache-active-file (expand-file-name "gnus/news/cache/active" user-emacs-directory))
(setq message-directory (expand-file-name "gnus/mail" user-emacs-directory))
(setq message-auto-save-directory (expand-file-name "gnus/mail/drafts" user-emacs-directory))
(setq mail-source-directory (expand-file-name "gnus/mail/incoming" user-emacs-directory))
(setq nnfolder-directory (expand-file-name "gnus/mail/archive" user-emacs-directory))
(setq nnmail-message-id-cache-file (expand-file-name "gnus/.nnmail-cache" user-emacs-directory))
(setq nntp-marks-directory (expand-file-name "gnus/news/marks" user-emacs-directory))
(setq mml-default-directory (expand-file-name "gnus/attachments" user-emacs-directory))
;; gpg
(setq mm-verify-option 'always)
(setq mm-decrypt-option 'always)
(setq mm-encrypt-option 'guided)
;; misc
(setq gnus-permanently-visible-groups "INBOX")
(setq gnus-message-archive-group nil)
(setq gnus-show-threads t)
(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))
(setq gnus-summary-display-arrow t)
(setq gnus-activate-level 1)
(setq gnus-use-full-window nil)
(setq gnus-inhibit-startup-message t)
(setq gnus-novice-user nil)
(setq gnus-expert-user t)
(setq gnus-interactive-exit t)
(setq gnus-asynchronous t)
(setq gnus-use-dribble-file nil)
(setq gnus-always-read-dribble-file nil)
(setq gnus-preserve-marks nil)
(setq message-confirm-send t)
(setq message-from-style 'angles)
(setq message-signature "Jiaxi\n0x3F938F7B")
;; type `B m' to move entries
(setq gnus-move-split-methods
      '((".*" "INBOX")
        (".*" "[Gmail]/All Mail")
        (".*" "[Gmail]/Trash")))


;; -------------------------------------------------------------------
;; OrgMode
;; -------------------------------------------------------------------

;; basic org options
(setq org-startup-with-inline-images nil)
(setq org-image-actual-width nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-html-style-include-scripts nil)
(setq org-export-html-style-include-default nil)
(setq org-html-postamble t)
(setq org-archive-location (expand-file-name "org/archive.org::" org-directory))
(setq org-html-postamble-format '(("en" "&copy; %a / %C")))
(setq org-priority-faces '((?A . (:foreground "red" :weight bold))
                           (?B . (:foreground "orange" :weight bold))
                           (?C . (:foreground "yellow" :wegith bold))))

;; org-agenda
(setq org-agenda-files (list (expand-file-name "org/agenda.org" org-directory)))
(setq org-agenda-include-diary nil)

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
         "_%<%H:%M>_ / %?\n")
        ("w" "Wish" entry (file+headline "org/wish.org" "Wishlist")
         "* WANT %?\n%U")
        ("p" "Publish" plain (file "org/p-tweets.org")
         "*** %?\n%U\n-----")))

;; org
(use-package org
  :ensure org-plus-contrib
  :after org
  :config
  ;; ox-latex
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (setq org-latex-listings t)
  (setq org-latex-listings-options '(("breaklines" "true")))
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; ox-bibtex
  (require 'ox-bibtex)
  ;; org-publish
  (defun org-html-publish-index (prop)
    "Generate index.html."
    (let ((index-org (expand-file-name "org/index.org" org-directory))
          (export-dir "/ssh:jiaxi@sdf.org:~/html/"))
      (org-html-publish-to-html nil index-org export-dir)))
  (setq org-publish-timestamp-directory user-emacs-directory)
  (setq org-publish-project-alist
        `(("org"
           :base-directory ,(expand-file-name "org/" org-directory)
           :base-extension "org"
           :publishing-directory "/ssh:jiaxi@sdf.org:~/html/"
           :publishing-function org-html-publish-to-html
           :exclude "^\\([^p]\\|p[^-]\\).*"
           :completion-function org-html-publish-index
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble "<nav><a href='/'>$HOME</a></nav>"
           :html-head "<link rel='stylesheet' type='text/css' href='static/org.css'/>"
           :html-mathjax "path:https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"
           :html-doctype "html5"
           :html-html5-fancy t
           :htmlized-source t
           :auto-sitemap t
           :sitemap-title ""
           :sitemap-filename "sitemap.org"
           :sitemap-sort-files anti-chronologically
           :sitemap-file-entry-format "%d  »  %t")
          ("static"
           :base-directory ,(expand-file-name "static/" org-directory)
           :base-extension "css\\|js\\|pdf"
           :publishing-directory "/ssh:jiaxi@sdf.org:~/html/static/"
           :publishing-function org-publish-attachment)
          ("website"
           :components ("org" "static"))))
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (calc . t)
     (emacs-lisp . t)
     (haskell . t)
     (julia . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (R . t)
     (shell . t))))

;; htmlize
(use-package htmlize
  :after org)


;; -------------------------------------------------------------------
;; TeX
;; -------------------------------------------------------------------

;; auctex
(use-package tex
  :defer t
  :ensure auctex
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'TeX-output-mode 'emacs))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-clean-confirm nil)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-to-list 'TeX-command-list '("Latexmk" "latexmk -pdf -quiet %s" TeX-run-command nil t :help "Run latexmk")))

;; company-math
(use-package company-math
  :after tex
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode))

;; company-auctex
(use-package company-auctex
  :after tex
  :config
  (company-auctex-init))


;; -------------------------------------------------------------------
;; Theme
;; -------------------------------------------------------------------

;; ----- color theme -----

;; solarized-theme
(use-package solarized-theme
  :config
  (setq x-underline-at-descent-line t)
  (setq solarized-emphasize-indicators nil))

;; theme-changer
(use-package theme-changer
  :if window-system
  :config
  (change-theme 'solarized-light 'solarized-dark))

;; ----- mode-line -----

;; Crafted for battery-mode, evil-mode and eyebrowse-mode
(setq-default mode-line-format
              (list '(:eval (propertize (evil-generate-mode-line-tag evil-state) 'face '(:inherit font-lock-comment-face)))
                    "%e"
                    'mode-line-mule-info
                    'mode-line-modified
                    'mode-line-remote " "
                    '(:eval (propertize " %b " 'face (if (buffer-modified-p) '(:background "#d33682" :foreground "#fdf6e3" :weight bold)
                                                       '(:background "#268bd2" :foreground "#fdf6e3" :weight light))
                                        'help-echo (buffer-file-name)))
                    '(:propertize " %p/%I " face (:background "gray60" :foreground "#fdf6e3")
                                  help-echo (count-words--buffer-message))
                    '(:eval (propertize (concat " " (eyebrowse-mode-line-indicator) " ")))
                    '(:eval (propertize (format-time-string "%p·%H:%M ") 'help-echo (format-time-string "%F %a") 'face '(:inherit font-lock-doc-face)))
                    'battery-mode-line-string
                    '(:propertize (which-func-mode (" " which-func-format)))
                    '(:eval (when (> (window-width) 100) (propertize " {%m}" 'face '(:weight normal))))
                    '(:eval (when (> (window-width) 100) (propertize vc-mode 'face '(:inherit font-lock-keyword-face :weight bold))))
                    "-%-"))


;; -------------------------------------------------------------------
;; Evil
;; -------------------------------------------------------------------

;; evil
(use-package evil
  :demand t
  :config
  (evil-mode t)
  (mapc (lambda (my-mode) (evil-set-initial-state my-mode 'emacs))
        (list 'calendar-mode 'comint-mode 'completion-mode
              'dired-mode 'epa-key-list-mode 'eshell-mode
              'eww-mode 'eww-bookmark-mode 'help-mode
              'inferior-python-mode 'Info-mode 'message-mode
              'newsticker-treeview-mode 'process-menu-mode
              'profiler-report-mode 'shell-mode 'speedbar-mode
              'special-mode))
  :bind
  (("<f5>" . evil-make)
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
  :bind
  (("M-;" . evilnc-comment-or-uncomment-lines)
   :map evil-normal-state-map
   (", c SPC" . evilnc-comment-or-uncomment-lines)))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode t))

;; evil-matchit
(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))

;; evil-mc
(use-package evil-mc
  :defer 5
  :config
  (global-evil-mc-mode t))


;; -------------------------------------------------------------------
;; Helm
;; -------------------------------------------------------------------

;; helm
(use-package helm
  :defer 3
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'helm-grep-mode 'emacs))
  (require 'helm-config)
  (helm-mode)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)
  (helm-push-mark-mode t)
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-follow-mode-persistent t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-grep-ag-command "rg --color always --smart-case --no-heading --line-number %s %s %s")
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-s" . helm-occur)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-x g" . helm-do-grep-ag)))

;; helm-bibtex
(use-package helm-bibtex
  :after helm
  :config
  (setq bibtex-completion-bibliography (list (expand-file-name "org/bib/main.bib" org-directory)))
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-notes-path (expand-file-name "org/research-notes.org" org-directory))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-library-path (list (expand-file-name "pdf" org-directory)))
  (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
  (setq bibtex-completion-notes-template-one-file "\n* ${title} (${year})\n:PROPERTIES:\n:Custom_ID: ${=key=}\n:END:\n")
  :bind ("C-c b" . helm-bibtex))


;; -------------------------------------------------------------------
;; Company
;; -------------------------------------------------------------------

;; company
(use-package company
  :init
  ;; enable company globally
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  ;; enable file name completion
  (add-to-list 'company-backends 'company-files)
  ;; exclude annoying dabbrev completion
  (setq company-backends (delete 'company-dabbrev company-backends))
  ;; turn company off in some specific modes
  (setq company-global-modes '(not comint-mode eshell-mode org-mode))
  :bind
  (:map
   company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)))


;; -------------------------------------------------------------------
;; Deft
;; -------------------------------------------------------------------

(use-package deft 
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'deft-mode 'emacs))
  (setq deft-directory (expand-file-name "org" org-directory))
  (setq deft-extensions '("org" "md" "tex"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-auto-save-interval nil)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (add-hook 'deft-open-file-hook 'deft-filter-clear)
  :bind ("C-c d" . deft))


;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------

;; basic settings
(setq-default c-basic-offset 4)

;; comapny-c-headers
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; cmake-mode
(use-package cmake-mode
  :config
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


;; -------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------

;; anaconda-mode
(use-package anaconda-mode
  :after python
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; company-anaconda
(use-package company-anaconda
  :after python
  :config
  (add-to-list 'company-backends 'company-anaconda))


;; -------------------------------------------------------------------
;; Haskell
;; -------------------------------------------------------------------

;; haskell-mode
(use-package haskell-mode
  :after (evil haskell-mode)
  :config
  (with-eval-after-load 'evil
    (progn (evil-set-initial-state 'haskell-interactive-mode 'emacs)
           (evil-set-initial-state 'haskell-error-mode 'emacs)))
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc))


;; -------------------------------------------------------------------
;; R & Julia
;; -------------------------------------------------------------------

;; ess
(use-package ess
  :after (evil julia-mode)
  :init
  (setq inferior-julia-program-name "julia")
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'inferior-ess-mode 'emacs))
  (require 'ess-site)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-eval-visibly nil)
  (setq ess-history-file nil))


;; -------------------------------------------------------------------
;; Markdown
;; -------------------------------------------------------------------

;; markdown-mode
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (setq markdown-enable-math t)
  (setq markdown-command "pandoc --mathml")
  (setq markdown-css-paths '("http://tilde.works/~isaac/static/md.css"))
  (add-hook 'markdown-mode-hook 'flyspell-mode))


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
  :config
  (setq avy-background t)
  :bind* ("C-'" . avy-goto-char-2))

;; bbdb
(use-package bbdb
  :defer 5
  :config
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

;; bing-dict
(use-package bing-dict
  :bind ("C-c t" . bing-dict-brief))

;; dumb-jump
(use-package dumb-jump
  :defer 3
  :after prog-mode
  :config
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-mode-line-separator ",")
  (set-face-attribute 'eyebrowse-mode-line-active nil :inherit font-lock-warning-face))

;; flycheck
(use-package flycheck
  :defer 2
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'flycheck-error-list-mode 'emacs))
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; linum-relative
(use-package linum-relative
  :config
  (linum-relative-mode)
  (setq linum-relative-current-symbol ""))

;; projectile
(use-package projectile
  :defer 2
  :config
  (projectile-global-mode))

;; quickrun
(use-package quickrun
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'quickrun--mode 'emacs))
  :bind ("C-c q" . quickrun))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :after prog-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rainbow-mode
(use-package rainbow-mode
  :after css-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

;; smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings))

;; smooth-scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t))

;; sr-speedbar
(use-package sr-speedbar
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-enable-update t)
  (setq sr-speedbar-skip-other-window-p t)
  (setq sr-speedbar-auto-refresh t)
  :bind ("<f9>" . sr-speedbar-toggle))

;; symon
(use-package symon
  :config
  (setq symon-delay 33)
  (symon-mode))

;; wgrep
(use-package wgrep
  :after grep)

;; which-key
(use-package which-key
  :config (which-key-mode))

;; yasnippet
(use-package yasnippet
  :defer 2
  :config (yas-global-mode t))


;; -------------------------------------------------------------------
;; Compatibility
;; -------------------------------------------------------------------

;; ----- Windows -----

(when (eq system-type 'windows-nt)
  ;; set font
  (when (member "Input" (font-family-list))
    (set-face-attribute 'default nil :font "Input-13"))
  (when (member "微软雅黑" (font-family-list))
    (set-fontset-font t 'han "微软雅黑")))

;; ----- MacOS -----

(when (eq system-type 'darwin)
  ;; 1. fix PATH problem
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-copy-env "PATH"))
  ;; 2. command => meta; option => super
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;; 3. set font
  (when (member "PragmataPro" (font-family-list))
    (set-face-attribute 'default nil :font "PragmataPro-14"))
  ;; 4. fix some binareis
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3"))


;;; init.el ends here
