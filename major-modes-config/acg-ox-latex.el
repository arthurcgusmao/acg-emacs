(with-eval-after-load 'ox-latex
  
  
  (add-to-list 'org-latex-classes
               '("article-1"                          ;class-name
                 "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=2in, right=2in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*a{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  
  
  
  (add-to-list 'org-latex-classes
               '("times-12"                          ;class-name
                 "\\documentclass[12pt]{article}
\\usepackage{times}
\\usepackage[top=1.5in, bottom=1.8in, left=1.5in, right=1.5in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*a{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

