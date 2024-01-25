;;; package --- My emacs configuration
;;; Summary: font switching using fontaine

(use-package fontaine
  :config

  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; (setq )upport or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; Iosevka Comfy            == monospaced, supports ligatures
  ;; Iosevka Comfy Fixed      == monospaced, no ligatures
  ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
  ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
  ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
  (setq fontaine-presets
        '((tiny
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 70)
          (small
           :default-family "Iosevka Comfy Fixed"
           :default-height 90)
          (regular
           :default-height 100)
          (medium
           :default-height 110)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (jumbo
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (thai
           :default-family "Garuda"
           :default-height 140)
          (source
           :default-family "SourceCodePro"
           :default-height 100)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Recover last preset or fall back to desired style from
  ;; `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; fontaine does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

(provide 'init-fontaine)
