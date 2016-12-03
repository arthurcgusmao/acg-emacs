(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(setq highlight-indent-guides-method 'character)
(set-face-foreground 'highlight-indent-guides-character-face "gainsboro")
