;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gpg-python-stuff.el
;;
;; useful stuff for editing python code
;;
;; gpg-python-run-current-buffer ()
;;
;; gpg-python-find-module(py-module-name)
;; this method finds a given python module and then opens the module in
;; a buffer
;;
;; author: Greg Green <ggreen@zhonka.net>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; user definable variables
;;
(defcustom gpg-python-module-new-window t
  "*Open python modules in a new window."
  :type 'string
  :group 'python)

(defcustom gpg-python-buffer-cmd-args "-u"
  "Use these arguments when running python on the current buffer"
  :type 'string
  :group 'python)

;;;; gpg-python-run-current-buffer
;;;; run python using the current buffer
(defun gpg-python-run-current-buffer (args)
  "Run python using the current buffer"
  (interactive "sArgs:")
  (compile
   (format "python %s \"%s\" %s"
	   gpg-python-buffer-cmd-args
	   (buffer-file-name)
	   args)))

;; gpg-python-find-module
(defun gpg-python-find-module(py-module-name)
  "Find the python module source and open it in a new buffer;
The module is found using the current PYTHONPATH. The module name does not
need to have the .py suffix attached to it. It will be added if missing."
  (interactive "smodule name: ")
  (let ((pymodule (gpg-python-find-file (gpg-python-add-suffix py-module-name))))
    (if (equal pymodule nil) 
      (message (format "%s not found in python search path." py-module-name))
      (if (equal gpg-python-module-new-window t) 
	  (find-file-other-window pymodule)
	(find-file pymodule))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;
;;
(defun gpg-python-get-interp-path ()
  "Using the python interpreter, find the modules search path;
Prepends the current directory to the list. Returns a list of the directory 
paths."
  (let ((text (shell-command-to-string
               (format "python -c \"import sys; print '\\n'.join(sys.path)\""))))
   (cons "." (split-string (substring text 0 -1) "\n")))
)

;;
;;
(defun dots-to-slashes (module)
  "Given a string, convert all dots (.) to slashes (/)."
  (mapconcat 'identity (split-string module "\\.") "/")
)

;;
;;
(defun gpg-python-find-file(py-module-name)
  "Given a list of paths, try to find a python module in those paths;"
  (catch 'found
    (dolist (path (gpg-python-get-interp-path))
      (let ((filepath (concat path "/" py-module-name)))
	(if (file-exists-p filepath)
	    (throw 'found filepath))))))

;;
;;
(defun gpg-python-add-suffix(py-module-name)
  "Checks for the '.py' extension on py-module-name, if not there it adds it;"
  (if (string-match "\\.py$" py-module-name)
      py-module-name
    (concat (dots-to-slashes py-module-name) ".py"))
)

