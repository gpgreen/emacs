;;; gpg-python-template.el
;;;

(require 'tempo)

;;;
;;; Define some templates

;; A module template

(tempo-define-template "gpg-python-skeleton" 
  '("##############################################################################\n"
    "# \n"
    "# Copyright (c) 2017 Greg Green <ggreen@bit-builder.com>\n"
    "# \n"
    "# File: \n"
    "'''\n" (P "Module doc string: ") "\n'''\n"
    "\n\n")
  "moduleSkeleton"
  "Insert a skeleton for a Python module.")

;;
;; Functions to insert the templates

(defun gpg-python-insert-new-buffer-strings ()
  "Insert gpg-python-insert-new-buffer-strings."
  (interactive)
  (tempo-template-gpg-python-skeleton))
