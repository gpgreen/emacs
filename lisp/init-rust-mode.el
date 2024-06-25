;;; package --- My emacs configuration

;;; Commentary: my rust mode configuration

(use-package rust-mode
  :config
  (let ((dot-cargo-bin (expand-file-name "~/.cargo/bin/")))
    (setq rust-rustfmt-bin (concat dot-cargo-bin "rustfmt")
          rust-cargo-bin (concat dot-cargo-bin "cargo")
          rust-format-on-save t
          fill-column 100
          indent-tabs-mode nil))
  (column-number-mode)
  ; (hs-minor-mode))
  (display-line-numbers-mode))

;; cargo-mode has some good features
;; use prefix C-u before command to add options
(use-package cargo-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  ;; to change the keymap from C-c a
  ; (keymap-set cargo-minor-mode-map (kbd ...) 'cargo-mode-command-map)
  (setq compilation-scroll-output t
        cargo-mode-use-comint nil))

(use-package flymake-clippy
  :hook (rust-mode . flymake-clippy-setup-backend))

(defun flymake-clippy-manually-activate-flymake ()
  "Shim for working around eglot's tendency to suppress flymake backends."
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (flymake-mode 1))

;; `eglot' by default will suppress all other flymake backends than its own
;; <https://github.com/joaotavora/eglot/issues/268> This workaround will
;; add `flymake-clippy'
(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (eglot-managed-mode . flymake-clippy-manually-activate-flymake))
  :config
  (add-to-list 'eglot-stay-out-of 'flymake))

(provide 'init-rust-mode)
