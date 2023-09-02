;;; early-init --- Code to run at beginning of emacs init file loading

;;; Commentary:
;; https://github.com/raxod502/straight.el
;; prevent package.el loading packages prior to their init-file loading.

(setq package-enable-at-startup nil)
