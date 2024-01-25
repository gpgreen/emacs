;;; package --- My emacs configuration

;;; Commentary: my rust mode configuration

(use-package smartparens :ensure t
  :config (require 'smartparens-rust))

(defun sp1ff/rust/mode-hook ()
  "My rust-mode hook"

  (column-number-mode)
  (display-line-numbers-mode)
  (hs-minor-mode)
  (smartparens-mode)
  (define-key rust-mode-map "\C-ca" 'eglot-code-actions)
  (define-key rust-mode-map (kbd "C-<right>")   'sp-forward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-<left>")    'sp-forward-barf-sexp)
  (define-key rust-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)
  (define-key rust-mode-map "\C-c>" 'hs-show-all)
  (define-key rust-mode-map "\C-c<" 'hs-hide-all)
  (define-key rust-mode-map "\C-c;" 'hs-toggle-hiding)
  (define-key rust-mode-map "\C-c'" 'hs-hide-level)
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        fill-column 100))

(use-package rust-mode
  :hook (rust-mode . sp1ff/rust/mode-hook)
  :config
  (let ((dot-cargo-bin (expand-file-name "~/.cargo/bin/")))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-cargo-bin (concat dot-cargo-bin "cargo")
          rust-format-on-save t)))

(use-package cargo-mode
  :config
  (add-hook 'rust-ts-mode-hook 'cargo-minor-mode))

(use-package clippy-flymake
  :straight
  (clippy-flymake
   :type git
   :host sourcehut
   :repo "mgmarlow/clippy-flymake")
  :hook (rust-mode . clippy-flymake-setup-backend))

(defun clippy-flymake-manually-activate-flymake ()
  "Shim for working around eglot's tendency to suppress flymake backends."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

;; `eglot' by default will suppress all other flymake backends than its own
;; <https://github.com/joaotavora/eglot/issues/268> This workaround will
;; add `flymake-clippy'
(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (eglot-managed-mode . clippy-flymake-manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))

(provide 'init-rust-mode)
