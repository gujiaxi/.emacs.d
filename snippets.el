;;; snippets.el --- my snippets for yasnippet

;;; Commentary:

;; These are my personal snippets for yasnippet.

;;; Code:

(yas-define-snippets 'fundamental-mode
                     '(("dd" "`(format-time-string \"%F\")`" "dd")
                       ("dt" "`(format-time-string \"%T\")`" "dt")
                       ("df" "`(format-time-string \"%F %T%z\")`" "df")
                       ("magic" "`(yas-trimmed-comment-start)` -*- $1 -*-" "magic")
                       ("lorem" "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat,sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet." "lorem")))

(yas-define-snippets 'makefile-mode
                     '(("check" "check-syntax:\n\tCC -Wall -Wextra -pedantic -std=c++14 -fsyntax-only $(CHK_SOURCES)" "check" nil nil ((yas-indent-line nil)))))

(yas-define-snippets 'org-mode
                     '(("bib" "#+BIBLIOGRAPHY: $0 plain limit:t option:-nobibsource option:-nokeywords" "bib")
                       ("doc" "#+TITLE: $1
#+STARTUP: hidestars indent content
#+OPTIONS: toc:nil num:nil html-style:nil html-scripts:nil html-postamble:t
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://jiaxi.sdf.org/css/org.css\"/>
$0" "doc")))

(yas-define-snippets 'python-mode
                     '(("plot" "plt.rcParams['font.size'] = 8
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.serif'] = ['DejaVu Serif']
plt.rcParams['font.sans-serif'] = ['DejaVu Sans']
plt.rcParams['font.monospace'] = ['DejaVu Sans Mono']
plt.rcParams['grid.linestyle'] = 'dashed'
plt.rcParams['grid.linewidth'] = 0.5
plt.rcParams['grid.alpha'] = 0.5
plt.rcParams['legend.fontsize'] = 8
plt.rcParams['axes.labelsize'] = 8
plt.rcParams['axes.titlesize'] = 8
plt.rcParams['xtick.labelsize'] = 8
plt.rcParams['ytick.labelsize'] = 8
plt.rcParams['savefig.dpi'] = 150
plt.rcParams['figure.figsize'] = [3.39, 2.10]" "plot")))


;;; snippets.el ends here
