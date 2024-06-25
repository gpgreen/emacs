;;; init-treesitter.el --- Initialization file for tree sitter

;;; Commentary:
;;

(defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash "https://github.com/tree-sitter/tree-sitter-bash")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (css "https://github.com/tree-sitter/tree-sitter-css")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (html "https://github.com/tree-sitter/tree-sitter-html")
               (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (make "https://github.com/alemuller/tree-sitter-make")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
               (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; only install 'grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))))

  ;; Optional but recommended. Tree-sitter enabled major-modes are distinct from their
  ;; ordinary counterparts.
  ;;
  ;; You can remap major modes with 'major-mode-remap-alist'. Note
  ;; that tihs does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(mp-setup-install-grammars)

(provide 'init-treesitter)
