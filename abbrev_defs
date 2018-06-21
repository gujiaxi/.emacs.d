;; -*- mode: emacs-lisp; -*-

(define-abbrev-table 'global-abbrev-table
  '(("em" "imjiaxi@gmail.com")))

(define-abbrev-table 'org-mode-abbrev-table
  '(("doc" "#+TITLE: $title\n#+TODO: TODO(t) WAIT(w) | DONE(d) UNDO(u)\n#+STARTUP: hidestars indent content align inlineimages\n#+OPTIONS: toc:nil num:nil ^:{} html-style:nil html-scripts:nil html-postamble:t\n#+HTML_HEAD: <link rel='stylesheet' type='text/css' href='http://jiaxi.sdf.org/static/org.css'/>\n#+LaTeX_HEADER: \\usepackage[scheme=plain]{ctex}\n#+LaTeX_HEADER: \\usepackage[margin=1in]{geometry}")
    ("bib" "#+BIBLIOGRAPHY: $1 plain limit:t option:-nobibsource option:-nokeywords")))

(define-abbrev-table 'python-mode-abbrev-table
  '(("#\!" "#!/usr/bin/env python\n# -*- coding: utf-8 -*-")
    ("plot" "bfs = 9\nplt.style.use('seaborn-paper')\nplt.rc('font', **{'family': 'sans-serif', 'size': bfs})\nplt.rc('font', **{'sans-serif': ['Helvetica', 'Arial']})\nplt.rc('xtick', **{'direction': 'in', 'labelsize': bfs, 'major.size': 3.5})\nplt.rc('ytick', **{'direction': 'in', 'labelsize': bfs, 'major.size': 3.5})\nplt.rc('axes', **{'labelsize': bfs})\nplt.rc('legend', **{'fontsize': bfs})\nplt.rc('grid', **{'linestyle': 'dashed'})\nplt.rc('figure', **{'figsize': (3.5, 2.2), 'autolayout': True})\nplt.rc('savefig', **{'bbox': 'tight'})")))

(define-abbrev-table 'haskell-mode-abbrev-table
  '(("#\!" "#!/usr/bin/env stack runghc")))

(define-abbrev-table 'tex-mode-abbrev-table
  '(("bib" "\bibliographystyle{plain}\n\bibliography{main}")))
