;; This file is part of gpycomplete.

;; gpycomplete is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; gpycomplete is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with gpycomplete.  If not, see <http://www.gnu.org/licenses/>.

;; gpycomplete.el is written from scratch by Fabian Ezequiel Gallina
;; <fgallina at caffeinegroup dot com dot ar> but it is somehow based on
;; the original pycomplete package from the http://python-mode.sf.net.

;; gpycomplete allows inline completion and help for the python
;; programing language within GNU/Emacs

(require 'pymacs)
(require 'python-mode)

(pymacs-load "gpycomplete")


(defun gpy-reinitialize ()
  "Reinitializes gpy, this is just a debug helper"
  (interactive)
  (load-file "~/.emacs.d/gpycomplete.el")
  (pymacs-load "gpycomplete"))


(defun gpy-get-code ()
  "Gets all the code written in the current buffer"
  (buffer-substring (point-min) (point-max)))

(defun gpy-get-current-expression ()
  "Gets the current expression to complete"
  (save-restriction
    (save-excursion
      (beginning-of-line)
      (push-mark nil t)
      (end-of-line)
      (narrow-to-region (mark) (point)))
    (save-excursion
      (if (search-backward " " nil t)
	  (forward-char)
	(beginning-of-line))
      (push-mark nil t)
      (setq gpylbound (mark)))
    (buffer-substring gpylbound (point))))


(defun gpy-get-nearest-symbol (symbol-type)
  "Returns the name of the nearest symbol-type (function, method,
class, nested"
  (interactive "s")
  (save-excursion
    (cond
     ((equal symbol-type "function")
      (re-search-backward "^\\(\\)def +\\([a-zA-Z0-9_.]+\\).*" nil t))
     ((equal symbol-type "class")
      (re-search-backward "^\\(\\)class +\\([a-zA-Z0-9_.]+\\).*" nil t))
     ((equal symbol-type "method")
      (re-search-backward "^\\( +\\)def +\\([a-zA-Z0-9_.]+\\).*" nil t))
     ((equal symbol-type "nested")
      (re-search-backward "^\\( +\\)class +\\([a-zA-Z0-9_.]+\\).*" nil t)))
    (let ((gpy-nearest-symbol-point (point))
	  (gpy-nearest-symbol-indentation (match-string-no-properties 1))
	  (gpy-nearest-symbol (match-string-no-properties 2)))
      (if (equal gpy-nearest-symbol nil)
	  (progn
	    (setq gpy-nearest-symbol "")
	    (setq gpy-nearest-symbol-indentation "")
	    (setq gpy-nearest-symbol-point 0)))
      (if (equal (interactive-p) t)
	  (message (concat "simbol: " gpy-nearest-symbol "\n"
			   "point: " (number-to-string gpy-nearest-symbol-point) "\n"
			   "indentation: [" gpy-nearest-symbol-indentation "]"))
	(list gpy-nearest-symbol gpy-nearest-symbol-point gpy-nearest-symbol-indentation)))))


(defun gpy-get-subcontext ()
  "Gets the current function or class where the user could be into"
  (interactive)
  (let ((gpy-nearest-function-info (gpy-get-nearest-symbol "function"))
	(gpy-nearest-class-info (gpy-get-nearest-symbol "class"))
	(current-point (point)))
    (if (< (- current-point (car (nthcdr 1 gpy-nearest-class-info)))
	   (- current-point (car (nthcdr 1 gpy-nearest-function-info))))
	(car (nthcdr 0 gpy-nearest-class-info))
      (car (nthcdr 0 gpy-nearest-function-info)))))


(defun gpy-show (string)
  "Shows string in the *gpycomplete* buffer"
  (display-message-or-buffer string "*gpycomplete*"))


(defun gpy-refresh-context ()
  "Refreshes the python interpreter with the code of the buffer"
  (interactive)
  (gpycomplete-refresh-context (gpy-get-code)))


(defun gpy-check-errors ()
  "Checks if the current code of the buffer contains errors and
displays the result"
  (interactive)
  (gpy-show
   (gpycomplete-refresh-context (gpy-get-code))))


(defun gpy-complete ()
  "Returns all the available completions for the previous expression"
  (gpycomplete-complete (gpy-get-current-expression) (gpy-get-code) (gpy-get-subcontext)))


(defun gpy-electric-lparen ()
  "Displays the signature of the previous expresion when a left
paren is typed and inserts ("
  (interactive)
  (gpy-show
   (gpycomplete-get-signature (gpy-get-current-expression)))
  (insert "("))


(defun gpy-electric-comma ()
  "Displays the signature of the previous expresion when a comma
is typed and inserts ,"
  (interactive)
  (gpy-show
   (gpycomplete-get-signature (gpy-get-current-expression)))
  (insert ","))


(defun gpy-help-at-point ()
  "Displays the help string of the previous expression"
  (interactive)
  (gpy-show
   (gpycomplete-get-help (gpy-get-current-expression))))


(defun gpy-signature (obj)
  "Displays the signature for a user entered expression"
  (interactive "sPython signature on: ")
  (gpy-show (gpycomplete-get-signature obj)))


(defun gpy-help (obj)
  "Prints the help for a user entered expression"
  (interactive "sPython help on: ")
  (gpy-show (gpycomplete-get-help obj)))


(defun gpy-complete-and-indent ()
  "If a set of completions is available for the previous
expression prints the available completions, if only a single
completion is available then it is insertered on the buffer. If no
completions are available it indents"
  (interactive)
  (let ((completions (gpy-complete))
	(buffer (current-buffer)))
    (if (or (string-equal completions "No completions")
	    (equal completions nil))
	(indent-for-tab-command)
      (progn
	(get-buffer-create "*gpy-completions*")
	(set-buffer "*gpy-completions*")
	(if (not (equal (point-min) (point-max)))
	    (delete-region (point-min) (point-max)))
	(insert completions)
	(goto-char (point-min))
	(if (not (search-forward "," nil t))
	    (progn
	      (setq completion (buffer-substring (point-min) (point-max)))
	      (set-buffer buffer)
	      (backward-kill-word 1)
	      (insert completion))
	  (display-message-or-buffer (buffer-substring (point-min) (point-max)))
	  )))))

(defun gpy-refresh-and-dot ()
  "Refreshes the context to the python interpreter and inserts a ."
  (interactive)
  (gpy-refresh-context)
  (insert "."))


(defun gpy-refresh-and-newline ()
  "Refreshes the context to the python interpreter and inserts a newline"
  (interactive)
  (gpy-refresh-context)
  (newline))


(defun gpy-add-path (path)
  "Adds path to the pythonpath"
  (interactive
   (list
    (expand-file-name (read-directory-name "Add Python path: "))))
  (message
   (concat "Added: " (gpycomplete-add-path path))))


(defun gpy-set-django-project (path settings-module)
  "Sets the current django project to work on, where path is the
path of the project and settings-module is the name of the
settings file without the .py extension"
  (interactive
   (list
    (expand-file-name (read-directory-name "Django proyect path: "))
    (read-string "Django settings module: " "settings" nil "settings")))
  (message (concat "Current django proyect path: " path "\n"
		   "Django settings module: " settings-module))
  (gpycomplete-set-django-project path settings-module))


(define-key py-mode-map "\M-\C-i" 'gpy-complete)
(define-key py-mode-map "\t" 'gpy-complete-and-indent)
(define-key py-mode-map "(" 'gpy-electric-lparen)
(define-key py-mode-map "," 'gpy-electric-comma)
(define-key py-mode-map [f1] 'gpy-help-at-point)
(define-key py-mode-map [f2] 'gpy-signature)
(define-key py-mode-map [f3] 'gpy-help)
(define-key py-mode-map "." 'gpy-refresh-and-dot)
;; (define-key py-mode-map [return] 'gpy-refresh-and-newline)
;; (define-key py-mode-map [up] 'gpy-move-up)
;; (define-key py-mode-map [down] 'gpy-move-down)
;; (define-key py-mode-map [right] 'gpy-move-right)
;; (define-key py-mode-map [left] 'gpy-move-left)
;; (define-key py-mode-map "\C-p" 'gpy-move-up)
;; (define-key py-mode-map "\C-n" 'gpy-move-down)
;; (define-key py-mode-map "\C-f" 'gpy-move-right)
;; (define-key py-mode-map "\C-b" 'gpy-move-left)


;; This hook reloads the gpycomplete python package and refresh the
;; context when a python file is opened
(add-hook 'find-file-hook
	  (lambda ()
	    (if (string-match "\\.py$" (buffer-file-name))
		(progn
		  (pymacs-load "gpycomplete")
		  (gpy-refresh-context))))
	  t)

;; This hook reloads the gpycomplete python package and refresh the
;; context when a python file is saved
(add-hook 'after-save-hook
	  (lambda ()
	    (if (string-match "\\.py$" (buffer-file-name))
		(progn
		  (pymacs-load "gpycomplete")
		  (gpy-refresh-context))))
	  t)

(provide 'gpycomplete)