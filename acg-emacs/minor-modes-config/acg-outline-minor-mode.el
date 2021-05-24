;; Outline library creates an abstraction layer to give structure to text files
;; by means of sections, similarly to how Org-mode headings operate.

;; Configs in this file have been greatly influenced by Protesilaos
;; configurations:
;; https://protesilaos.com/dotemacs/#h:a8e737b8-7c90-4c68-8814-acf2f637ffa1

(use-package outline
  :straight nil)

(use-package foldout
  :after outline
  :straight nil)

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ("M-a" . bicycle-cycle)
              ("M-A" . bicycle-cycle-global)))

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))
