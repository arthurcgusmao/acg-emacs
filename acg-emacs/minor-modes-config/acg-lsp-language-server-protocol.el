(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable
        "~/.config/emacs/.cache/lsp/mspyls/Microsoft.Python.LanguageServer")
  (setq lsp-python-ms-auto-install-server t))
