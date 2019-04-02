;;; snippets.el --- my snippets for yasnippet

;;; Commentary:

;; These are my personal snippets for yasnippet.

;;; Code:

(yas-define-snippets 'fundamental-mode
                     '(("date" "`(format-time-string \"%F\")`" "date")
                       ("time" "`(format-time-string \"%T\")`" "time")
                       ("dtime" "`(format-time-string \"%F %T%z\")`" "dtime")
                       ("lorem" "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet." "lorem")))

(yas-define-snippets 'makefile-mode
                     '(("check" "check-syntax:\n\tCC -Wall -Wextra -pedantic -std=c++14 -fsyntax-only $(CHK_SOURCES)" "check" nil nil ((yas-indent-line nil)))))

(yas-define-snippets 'org-mode
                     '(("bib" "#+BIBLIOGRAPHY: $0 plain limit:t option:-nobibsource option:-nokeywords" "bib")
                       ("doc" "#+TITLE: $1
#+TODO: TODO(t) WAIT(w) | DONE(d) UNDO(u)
#+STARTUP: hidestars indent
#+OPTIONS: toc:nil num:nil html-style:nil html-scripts:nil html-postamble:t
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://jiaxi.sdf.org/css/org.css\"/>
#+LaTeX_HEADER: \\usepackage[margin=1in]{geometry}
#+LaTeX_HEADER: \\hypersetup{hidelinks=true}
#+LaTeX_HEADER: \\usepackage{amsthm}
#+LaTeX_HEADER: \\newtheorem{theorem}{Theorem}[section]
#+LaTeX_HEADER: \\newtheorem{lemma}[theorem]{Lemma}
#+LaTeX_HEADER: \\newtheorem{proposition}[theorem]{Proposition}
#+LaTeX_HEADER: \\newtheorem{claim}[theorem]{Claim}
#+LaTeX_HEADER: \\newtheorem{corollary}[theorem]{Corollary}
#+LaTeX_HEADER: \\newtheorem{definition}[theorem]{Definition}
$0" "doc")))

(yas-define-snippets 'python-mode
                     '(("plot" "bfs = 9
plt.style.use('seaborn-paper')
plt.rc('font', **{'family': 'sans-serif', 'size': bfs})
plt.rc('font', **{'sans-serif': ['Helvetica', 'Arial']})
plt.rc('xtick', **{'direction': 'in', 'labelsize': bfs, 'major.size': 3.5})
plt.rc('ytick', **{'direction': 'in', 'labelsize': bfs, 'major.size': 3.5})
plt.rc('axes', **{'labelsize': bfs})
plt.rc('legend', **{'fontsize': bfs})
plt.rc('grid', **{'linestyle': 'dashed'})
plt.rc('figure', **{'figsize': (3.5, 2.2), 'autolayout': True})
plt.rc('savefig', **{'bbox': 'tight'})" "plot")))


;;; snippets.el ends here
