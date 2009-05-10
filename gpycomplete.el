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


(defgroup python nil
  "Inline autocompletion for the python language"
  :group 'languages
  :prefix "gpy-")


(defcustom gpy-completions-separator ", "
  "the separator string used to break each completion"
  :type 'string
  :group 'python)


(defcustom gpy-no-completions-error "No Completions!"
  "Error to display when no completions available"
  :type 'string
  :group 'python)


(defcustom gpy-file-contains-errors "This file contains errors!"
  "Message to display when the file contains errors"
  :type 'string
  :group 'python)


(defcustom gpy-file-contains-no-errors "This file contains NO errors!"
  "Message to display when the file does not contains errors"
  :type 'string
  :group 'python)


(defcustom gpy-max-completions 0
  "The number of completions to display, if 0 it means no limit"
  :type 'integer
  :group 'python)


(defcustom gpy-completions-fill-column 80
  "The number of characters per line to show in the completions buffer"
  :type 'integer
  :group 'python)


(defcustom gpy-completions-buffer-name "*gpy-completions*"
  "Name of the buffer which will contain all the available completions"
  :type 'string
  :group 'python)


(defun gpy-reload ()
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


(defun gpy-get-cursor-indentation ()
  "Gets the indentation of the cursor in the current line"
  (save-excursion
    (re-search-backward "^\\( *\\)" nil t)
    (match-string-no-properties 1)))


(defun gpy-get-nearest-definition (&optional prev)
  "Goes to the nearest definition and returns its indentation, its type, its name and its point.
An optional argument tells the function to search backwards"
  (if (not (equal nil prev))
      (re-search-backward "^\\( *\\)\\(def\\|class\\) +\\([a-zA-Z0-9_.]+\\).*" nil t)
    (re-search-forward "^\\( *\\)\\(def\\|class\\) +\\([a-zA-Z0-9_.]+\\).*" nil t))
  (let ((gpy-nearest-symbol-point (point))
	(gpy-nearest-symbol-indentation (match-string-no-properties 1))
	(gpy-nearest-symbol-type (match-string-no-properties 2))
	(gpy-nearest-symbol-name (match-string-no-properties 3)))
    (if (equal gpy-nearest-symbol-name nil)
	(progn
	  (setq gpy-nearest-symbol-indentation "")
	  (setq gpy-nearest-symbol-type "")
	  (setq gpy-nearest-symbol-name "")
	  (setq gpy-nearest-symbol-point 0)))
    (list gpy-nearest-symbol-indentation
	  gpy-nearest-symbol-type
	  gpy-nearest-symbol-name
	  gpy-nearest-symbol-point)))


(defun gpy-get-subcontext ()
  "Gets the current function or class where the user is into"
  (interactive)
  (save-excursion
    (let ((symbol nil)
	  (list-of-definitions nil))
      (while (not (equal "" (nth 0 (setq symbol (gpy-get-nearest-definition t)))))
	(setq list-of-definitions (cons symbol list-of-definitions)))
      (setq list-of-definitions (cons (gpy-get-nearest-definition) list-of-definitions)))))

(defun gpy-get-subcontext-debug ()
  "Gets the current function or class where the user is into"
  (interactive)
  (message (gpy-get-subcontext)))


(defun gpy-find-shortest-word (words)
  "given a list of words it returns the shortest"
  (let ((shortest)
	(max)
	(index 0))
    (setq shortest 0)
    (setq max (length words))
    (while (< index max)
      (if (< (length (nth index words))
	     (length (nth shortest words)))
	  (setq shortest index))
      (setq index (+ 1 index)))
    (nth shortest words)))


(defun gpy-find-equal-string-from-start (words)
  "given a list of words it returns the longest string which matches
with the beginning of all the words of the list.

For example, given the list \('calefon' 'calabaza' 'caliente') it
returns 'cal'"
  (let ((current-string (gpy-find-shortest-word words))
	(matches-with-all nil)
	(index 0)
	(max (length words)))
    (while (and (equal matches-with-all nil)
		(not (equal current-string "")))
      (setq matches-with-all t)
      (setq index 0)
      (while (and (< index max)
		  (equal matches-with-all t))
	(if (equal
	     (string-match (concat "^" current-string)
			   (nth index words)) nil)
	    (setq matches-with-all nil))
	(setq index (+ 1 index)))
      (if (equal matches-with-all nil)
	  (setq current-string (substring current-string 0 -1))))
    current-string))


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
   (if (gpycomplete-refresh-context (gpy-get-code))
       gpy-file-contains-no-errors
     gpy-file-contains-errors)))


(defun gpy-get-completions nil
  "Returns a list with all the the available completions for the
expression behind the pointer"
  (interactive)
  (gpycomplete-get-completions (gpy-get-current-expression)
			       (gpy-get-code)
			       (gpy-get-subcontext)
			       (gpy-get-cursor-indentation)))


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


(defun gpy-show-completions (string)
  "Shows the parsed completion list"
  (let ((buffer (current-buffer)))
    (get-buffer-create gpy-completions-buffer-name)
    (set-buffer gpy-completions-buffer-name)
    (erase-buffer)
    (insert string)
    (mark-whole-buffer)
    (set-fill-column gpy-completions-fill-column)
    (fill-region (point-min) (point-max))
    (message (buffer-substring (point-min) (point-max)))
    (set-buffer buffer)))


(defun gpy-parse-completions (completions)
  "parses the list of completions into a string and returns what
should be shown on the *gpy-completions* buffer"
  (interactive)
  (if (not (equal completions nil))
      (progn
	(let ((limit gpy-max-completions)
	      (i 0)
	      (parsed ""))
	  (if (equal limit 0)
	      (setq limit (length completions)))
	  (while (and (not (equal i limit))
		      (not (equal nil (nth i completions))))
	    (setq parsed
		  (concat parsed
			  (nth i completions)
			  gpy-completions-separator))
	    (setq i (+ 1 i)))
	  (setq parsed (substring parsed 0
				  (- (length parsed)
				     (length gpy-completions-separator))))
	  parsed))
    gpy-no-completions-error))


(defun gpy-complete-or-indent ()
  "If a set of completions is available for the previous
expression prints the available completions, if only a single
completion is available then it is insertered on the buffer. If no
completions are available it indents"
  (interactive)
  (let ((completions-list)
	(parsed-completions)
	(buffer (current-buffer))
	(completion))
    (setq completions-list (gpy-get-completions))
    (setq parsed-completions (gpy-parse-completions completions-list))
    (if (equal completions-list nil)
	(progn
	  (indent-for-tab-command)
	  (gpy-show-completions parsed-completions))
      (progn
	(gpy-show-completions parsed-completions)
	(setq completion (gpy-find-equal-string-from-start completions-list))
	(if (not (equal completion ""))
	    (progn
              (save-restriction
                (beginning-of-line)
                (push-mark nil t)
                (end-of-line)
                (narrow-to-region (mark) (point))
                (let ((end-point (point))
                      (start-point 0))
                  (if (search-backward "." nil t)
                      (progn
                        (forward-char)
                        (setq start-point (point))
                        (delete-region start-point end-point))
                    (backward-kill-word 1))
                  (insert completion)))))))))


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


(define-key py-mode-map "\t" 'gpy-complete-or-indent)
(define-key py-mode-map "(" 'gpy-electric-lparen)
(define-key py-mode-map "," 'gpy-electric-comma)
(define-key py-mode-map [f1] 'gpy-help-at-point)
(define-key py-mode-map [f2] 'gpy-signature)
(define-key py-mode-map [f3] 'gpy-help)
(define-key py-mode-map "." 'gpy-refresh-and-dot)
(define-key py-mode-map [return] 'gpy-refresh-and-newline)


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