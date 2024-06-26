;;; package --- My emacs configuration

;;; Commentary: my rust tree-sitter mode configuration
;;; This will be invoked if the environment var 'EMACS_USE_TREESITTER_MODES' is set

(use-package rust-mode
  :config
  (column-number-mode)
  (display-line-numbers-mode)
  ;(hs-minor-mode))
  (let ((dot-cargo-bin (ggreen-find-cargo-bin-dir)))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-format-on-save t)))

(use-package cargo-mode
  :hook (rust-ts-mode . cargo-minor-mode)
  :config
  (let ((dot-cargo-bin (ggreen-find-cargo-bin-dir)))
    (setq cargo-path-to-bin (concat dot-cargo-bin "cargo")
          compilation-scroll-output t
          cargo-mode-use-comint nil)))

(use-package flymake-clippy
  :hook (rust-ts-mode . flymake-clippy-setup-backend))

(defun flymake-clippy-manually-activate-flymake ()
  "Shim for working around eglot's tendency to suppress flymake backends."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

;; `eglot' by default will suppress all other flymake backends than its own
;; <https://github.com/joaotavora/eglot/issues/268> This workaround will
;; add `flymake-clippy'
(use-package eglot
  :ensure t
  :hook ((rust-ts-mode . eglot-ensure)
         (eglot-managed-mode . flymake-clippy-manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))

;; (use-package combobulate
;;   :straight
;;   (combobulate
;;    :type git
;;    :host github
;;    :repo "mickeynp/combobulate")
;;   :preface
;;   (setq combobulate-key-prefix "C-c o")
;;   :hook
;;   (rust-ts-mode . combobulate-mode))

(provide 'init-rust-ts-mode)
