;;; hugs-mode.el ---  Major mode for editing Hugs source.

;; Author: Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;; Maintainer: Chris Van Humbeeck <chris.vanhumbeeck@cs.kuleuven.ac.be>
;;             Ported to Emacs-20 by Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Keywords: Hugs, Haskell, languages

;; $Id: hugs-mode.el,v 1.1 1997/11/03 03:26:59 hwloidl Exp $

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; NOTE: Read the commentary below for the right way to submit bug reports!

;;; Commentary:

;; USAGE
;; =====

;; Emacs should enter `hugs-mode' when you find a Hugs source file.
;; When you have entered `hugs-mode', you may get more info by pressing
;; C-h m. You may also get online help describing various functions by:
;; C-h f <Name of function you want described>

;; If the prefix argument to C-c C-f or C-c C-a is non-nil, the Hugs
;; process changes to the current buffer's directory before loading the
;; file.

;; If the variable hugs-command is set then its value will be sent to
;; the Hugs process after the file is loaded.  This can be used for a
;; top-level expression to evaluate.

;; Just set hugs-tipsi-support to nil unless you have the patch.
;; This patch makes it possible to do some incremental type inference and
;; walk through (type) warnings.

;; You probably want to set HUGSPATH or hugs-script-file-path right 

;; in your .emacs :
;;
;; (autoload 'hugs-mode "hugs-mode" "Go into hugs mode" t)
;;
;; (setq auto-mode-alist (append auto-mode-alist
;;			   '(("\\.hs$" . hugs-mode))))

;; KNOWN BUGS / BUGREPORTS
;; =======================

;; Since this is still an Alpha version, there are bugs in the
;; current version of this package.

;; * Classes, instances and types are only recognized in easy cases.
;; * Font-Lock mode can go wrong when using ' and " (Prelude)
;; * No support for infix notation.
;; * ...

;; To submit bug reports, type "C-c C-b".  These will be sent to
;; hugs-mode@kotnet.org

;; HALL OF FAME
;; ============

;; Completion is based on pascal.el. So, thanks to Espen Skoglund who
;; wrote pascal.el.

;; Hugs-interface is based on gofer.el. So, thanks to Denis Howe who
;; wrote gofer.el.

;; Some source is based on python-mode.el, such as the use of temp-buffers.
;; So, thanks to the people who have contributed to it.

;; Thanks to Kris Aerts for some ideas.

;;; Changelog (after 0.6):
;;  $Log: hugs-mode.el,v $
;;  Revision 1.1  1997/11/03 03:26:59  hwloidl
;;  Mode for writing haskell programs and starting hugs sessions
;;  Ported to Emacs 20 (no other changes to official version 0.6)
;;
;;  Revision 1.2  1997/11/03 00:56:39  hwloidl
;;  Ported to emacs-20
;;

;;; Code:

(defconst hugs-version "0.7"
  "`hugs-mode' version number.")

(defconst hugs-bugs-address "hugs-mode@kotnet.org"
  "Address of the mailing list for `hugs-mode' bugs.")

;; User options (settable via M-x set-variable and M-x edit-options)

(defvar hugs-indent-offset 4
  "*Indentation of Hugs statements with respect to containing block.")

(defvar hugs-class-indent 4
  "*Indentation for class blocks.")

(defvar hugs-instance-indent 4
  "*Indentation for instance blocks.")

(defvar hugs-startup-message t
  "*Non-nil displays a startup message when `hugs-mode' is first called.")

(defvar hugs-program "Hugs" 
  "*Shell command used to start the Hugs interpreter.")

(defvar hugs-tipsi-support nil
  "*If not nil, Hugs interpreter has TIPSI support.
Without TIPSI support, warnings can't be shown.")

(defvar hugs-auto-load-file nil
  "*If not nil, load each Hugs source file into the Hugs interpreter when
visiting.")

(defvar hugs-script-file-path nil
  "*List of directories in which to look for script files.
If this is nil, uses the colon-separated path in $HUGSPATH instead.")

(defvar hugs-recursive-completions t
  "*Non-nil means should also try all `import-files' for possible completions")

(defvar hugs-toggle-completions nil
  "*Non-nil means \\<hugs-mode-map>\\[hugs-complete-word] should try all
possible completions one by one. 
Repeated use of \\[hugs-complete-word] will show you all of them.
Normally, when there is more than one possible completion,
it displays a list of all possible completions.")

(defvar hugs-command nil
  "A string sent to the Hugs process after a file is loaded.

If this string is non-nil it is sent to Hugs after each :load, :also
or :project command (\\[hugs-load], \\[hugs-also],
\\[hugs-save-project-and-go]).  If the value is not a string it is
assumed to be a symbol and its symbol name is sent so a single Hugs
symbol need not be put in double-quotes.  A newline is sent after the
string.")

;; No user definable variables beyond this point.

(defvar hugs-mode-hook nil
  "Hook run in each `hugs-mode' buffer and Hugs process buffer.")

;; Have to bind hugs-file-queue before installing the kill-emacs hook.
(defvar hugs-file-queue nil
  "Queue of Hugs temp files.
Currently-active file is at the head of the list.")

;; Define a mode-specific abbrev table for those who use such things.
(defvar hugs-mode-abbrev-table nil
  "Abbrev table in use in `hugs-mode' buffers.")

(define-abbrev-table 'hugs-mode-abbrev-table nil)

(defvar hugs-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x))))
    (or (funcall ok (getenv "TMPDIR"))
        (funcall ok "/tmp")
        (funcall ok "/var/tmp")
        (funcall ok "/usr/tmp")
        (funcall ok  ".")
        (error
         "Couldn't find a usable temp directory -- set hugs-temp-directory")))
  "*Directory used for temp files created by a Hugs process.
By default, the first directory from this list that exists and that you
can write into:  the value (if any) of the environment variable TMPDIR,
/tmp, /var/tmp, /usr/tmp, or the current directory.")

(defun hugs-script-file-path ()
  "Return a list of directories in which to look for script files."
  (or hugs-script-file-path
      (let ((env (concat ":" (getenv "HUGSPATH") ":."))
	    (path nil)
	    pos)
	(or env (error "No script file path specified."))
	(while (setq pos (string-match ":[^:]+$" env))
	  (setq path (cons (substring env (1+ pos)) path)
		env (substring env 0 pos)))
	path)))

(defun hugs-script-file-name (name)
  "Return the absolute filename of a script file, if found in
`hugs-script-file-path'. Otherwise, return nil."
  ;; First, append extension `hs' to the name if not already there.
  (or (string-match "\\.hs$" name)
      (setq name (concat name ".hs")))
  (if (file-name-absolute-p name)
      ;; It's an absolute file name, so don't need `hugs-script-file-path'.
      name
    ;; It's not an absolute file name, so we need `hugs-script-file-path'.
    (let ((path (hugs-script-file-path)))
      (while (and path
		  (not (file-exists-p
			(concat (file-name-as-directory (car path))
				name))))
	(setq path (cdr path)))
      (if path
	  ;; Found : return absolute filename.
	  (concat (file-name-as-directory (car path)) name)
	;; Not found : return nil.
	nil))))

(defconst hugs-xemacs
  (string-match "Lucid\\|XEmacs" emacs-version)
  "t when using XEmacs or Lucid.")

(defconst hugs-emacs-19 
  (and (not hugs-xemacs)
       (string-match "^19\\." emacs-version))
  "t when using Emacs-19")

;; HWL: port for emacs-20
(defconst hugs-emacs-20
  (and (not hugs-xemacs)
       (>= (string-to-number (substring emacs-version 0 2)) 20))
  "t when using Emacs-20 or higher")
	   
;;;###autoload
(eval-when-compile
  ;; Imenu isn't used in XEmacs, so just ignore load errors.
  (condition-case ()
      (progn
	(require 'cl)
	(require 'imenu))
    (error nil)))

(eval-and-compile
  ;; Not every Emacs supports buffer-substring-no-properties.
  (or (fboundp 'buffer-substring-no-properties)
      (defun buffer-substring-no-properties (beg end)
	(format "%s" (buffer-substring beg end)))))

;; Keymap

(defvar hugs-mode-map (make-sparse-keymap)
  "Keymap used in `hugs-mode'.")

(define-key hugs-mode-map "\r"       'electric-hugs-return)
(define-key hugs-mode-map "\t"       'electric-hugs-tab)
(define-key hugs-mode-map "\177"     'backward-delete-char-untabify)
(define-key hugs-mode-map "\M-?"     'hugs-show-completions)
(define-key hugs-mode-map "\M-a"     'hugs-beg-of-block)
(define-key hugs-mode-map "\M-e"     'hugs-end-of-block)
(define-key hugs-mode-map "\M-h"     'hugs-hide-process-buffer)
(define-key hugs-mode-map "\M-m"     'hugs-mark-block)
(define-key hugs-mode-map "\M-s"     'hugs-show-process-buffer)
(define-key hugs-mode-map "\M-q"     'hugs-kill-process)
(define-key hugs-mode-map "\M-\C-a"  'hugs-previous-block)
(define-key hugs-mode-map "\M-\C-e"  'hugs-next-block)
(define-key hugs-mode-map "\M-\r"    'hugs-start-newline)
(define-key hugs-mode-map "\M-\t"    'hugs-complete-word)
(define-key hugs-mode-map "\C-c?"    'hugs-describe-mode)
(define-key hugs-mode-map "\C-c-"    'hugs-comment-region)
(define-key hugs-mode-map "\C-c:"    'hugs-guess-indent-values)
(define-key hugs-mode-map "\C-c\C-a" 'hugs-also)
(define-key hugs-mode-map "\C-c\C-b" 'hugs-submit-bug-report)
(define-key hugs-mode-map "\C-c\C-c" 'hugs-interrupt)
(define-key hugs-mode-map "\C-c\C-d" 'hugs-show-definition)
(define-key hugs-mode-map "\C-c\C-e" 'hugs-show-error)
(define-key hugs-mode-map "\C-c\C-f" 'hugs-load-file)
(define-key hugs-mode-map "\C-c\C-i" 'hugs-get-info)
(define-key hugs-mode-map "\C-c\C-n" 'hugs-show-next-warning)
(define-key hugs-mode-map "\C-c\C-p" 'hugs-show-previous-warning)
(define-key hugs-mode-map "\C-c\C-r" 'hugs-load-region)
(define-key hugs-mode-map "\C-c\C-t" 'hugs-get-type)
(define-key hugs-mode-map "\C-c\C-u" 'hugs-uncomment-region)
(define-key hugs-mode-map "\C-c\C-v" 'hugs-version)

(define-key hugs-mode-map "\C-\\r" 'hugs-show-definition)

;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).

(defun hugs-add-hugs-menu ()
  "Adds the menu 'hugs' to the menu bar in `hugs-mode'."
  (if (condition-case nil
	  (require 'easymenu)
	(error nil))
      (easy-menu-define hugs-mode-menu hugs-mode-map "Menu keymap for `hugs-mode'."
			'("Hugs"
			  ["Mark current block" hugs-mark-block t]
			  ["Move to start of block" hugs-beg-of-block t]
			  ["Move to end of block" hugs-end-of-block t]
			  ["--------------------" nil nil]
			  ["Move to previous block" hugs-previous-block t]
			  ["Move to next block" hugs-next-block t]
			  ["-------------------" nil nil]
			  ["Indent current line (TAB)" hugs-indent-line t]
			  ["-------------------------" nil nil]
			  ["Comment Out  Region" hugs-comment-region (mark)]
			  ["Uncomment Region" hugs-uncomment-region (mark)]
			  ["----------------" nil nil]
			  ["Load file into Hugs" hugs-load-file t]
			  ["Load region into Hugs" hugs-load-region t]
			  ["Add file into Hugs" hugs-also t]
			  ["Show next warning" hugs-show-next-warning t]
			  ["Show previous warning" hugs-show-previous-warning t]
			  ["Show first error message" hugs-show-error t]
			  ["------------------------" nil nil]
			  ["Show `hugs-process' buffer" hugs-show-process-buffer t]
			  ["Hide `hugs-process' buffer" hugs-hide-process-buffer t]
			  ["Kill `hugs-process'" hugs-kill-process t]
			  ["------------------" nil nil]
			  ["Show definition" hugs-show-definition t]
			  ["Show info" hugs-get-type t]
			  ["Show type" hugs-get-type t]
			  ["---------" nil nil]
			  ["Describe mode" hugs-describe-mode t]))
    
    (if hugs-xemacs (progn
		      (easy-menu-add hugs-mode-menu)
		      (setq mode-popup-menu (cons "Hugs-mode" hugs-mode-menu))))))

;; Syntax table

(defvar hugs-syntax-table
  (let ((table (copy-syntax-table)))
    ;; HWL Orig:
    (modify-syntax-entry ?_   "w " table)   
    ;(modify-syntax-entry ?`   "w " table)
    (modify-syntax-entry ?\'  "w " table)
    ;; HWL: ?_ and ?\' are valid symbols in varid and conid so should be 
    ;;      in the _ class but imenu has problems with these (checks only w's?)
    ;(modify-syntax-entry ?_   "_ " table)   
    (modify-syntax-entry ?`   ". " table)
    ;(modify-syntax-entry ?\'  "_ " table)
    (modify-syntax-entry ?\(  "()" table)
    (modify-syntax-entry ?\)  ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?\\  "\\" table)
	  
    (if hugs-xemacs
	;; XEmacs specific syntax-table.
	(progn
	  (modify-syntax-entry ?-   ". 2356" table)  ;  --  starts a comment.
	  (modify-syntax-entry ?\n  "> b   " table)  ;  \n  ends a comment.
	  (modify-syntax-entry ?{   ". 1   " table)  ;  {-  starts a nested comment.
	  (modify-syntax-entry ?}   ". 4   " table)  ;  -}  ends a nested comment.
	  )
      ;; Emacs specific syntax-table.
        (modify-syntax-entry ?{  "(}1 " table)
	(modify-syntax-entry ?}  "){4 " table)
	(modify-syntax-entry ?-  "_ 23" table)
      ;; No alternative comment-style because they don't share the same
      ;; first character.
      )
    table)
  "Syntax table in use in `hugs-mode'")

(defconst hugs-word-constituents "a-zA-Z0-9_'`"
  "These symbols are part of Hugs words.
This should be compatible with the hugs-syntax-table.")

;; Hugs keywords

(defvar hugs-class-keywords
  '("Bounded" "Enum" "Eq" "Eval" "Floating" "Fractional" "Functor" 
    "Integral" "Ix" "Monad" "MonadPlus" "MonadZero"
    "Num" "Ord" "Show" "Read" "Real" "RealFloat" "RealFrac")
  "Classes defined in Prelude.hs used when completing a word in a declaration.
The classes defined within the Hugs program will be completed runtime,
and should not be added to this list.")

(defvar hugs-seperator-keywords
  '("case" "of" "if" "then" "else" "where" "let" "in" "deriving")
  "Keywords to complete when not standing at the first word of a line.
The functions defined within the Hugs program will be completed runtime,
and should not be added to this list.")

(defvar hugs-start-keywords
  '("infixl" "infixr" "infix" "instance" "class" "type" "data" "import"
    "module" "primitive")
  "Keywords to complete when standing at the first word of a line.
The functions defined within the Hugs program will be completed runtime,
and should not be added to this list.")

(defvar hugs-type-keywords
  '("Bool" "Char" "Double" "Either" "FilePath" "Float" 
    "Int" "IO" "IOError" "Integer" "Maybe" "Ordering" 
    "Ratio" "Rational" "ReadS" "ShowS" "String" "Void")
  "Types defined in Prelude.hs used when completing a word in a declaration.
The types defined within the Hugs program will be completed runtime,
and should not be added to this list.")

(defvar hugs-function-keywords nil
  "Functions defined in Prelude.hs used when completing a word in a declaration.
The functions defined within Prelude.hs will be completed when completion is
invoked the first time.
The functions defined within the Hugs program will be completed runtime.")

;; Regular expressions used to calculate indent and completion.

(defconst hugs-class-keywords-re
  ;; (make-regexp hugs-class-keywords)
  (concat "\\<\\("
	  "Bounded\\|E\\(num\\|q\\|val\\)\\|F\\(loating\\|ractional\\|unctor\\)\\|"
	  "I\\(ntegral\\|x\\)\\|Monad\\(\\|Plus\\|Zero\\)\\|Num\\|Ord\\|"
	  "Rea\\([dl]\\|lF\\(loat\\|rac\\)\\)\\|Show"
	  "\\)\\>")
  "Regexp describing classes defined in Prelude.hs")

(defconst hugs-seperator-keywords-re
  ;; (make-regexp hugs-seperator-keywords)
  (concat "\\<\\("
	  "case\\|deriving\\|else\\|i[fn]\\|let\\|of\\|then\\|where"
	  "\\)\\>")
  "Regexp describing keywords to complete when not standing at the first word
of a line.")

(defconst hugs-start-keywords-re
  ;; (make-regexp hugs-start-keywords)
  (concat "\\<\\("
	  "class\\|data\\|i\\(mport\\|n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|"
	  "module\\|primitive\\|type"
	  "\\)\\>")
  "Regexp describing keywords to complete when standing at the first word
of a line.")

(defconst hugs-type-keywords-re
  ;; (make-regexp hugs-type-keywords)
  (concat "\\<\\("
	  "Bool\\|Char\\|Double\\|Either\\|F\\(ilePath\\|loat\\)\\|"
	  "I\\(O\\(\\|Error\\)\\|nt\\(\\|eger\\)\\)\\|Maybe\\|Ordering\\|"
	  "R\\(atio\\(\\|nal\\)\\|eadS\\)\\|S\\(howS\\|tring\\)\\|Void"
	  "\\)\\>")
  "Regexp describing types defined in Prelude.hs")


(defvar hugs-font-lock-keywords-1
  (if hugs-xemacs
      ;; XEmacs
      (list
       ;; Fontify all builtin seperator keywords.
       (cons hugs-seperator-keywords-re 'font-lock-keyword-face)
       ;; Fontify all builtin start keywords (except data, import, module and type).
       (append (list (concat "\\<\\("
			     "class\\|i\\(n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|primitive"
			     "\\)\\>"))
	       '(1 font-lock-keyword-face))
       ;; Fontify all type specifiers.
       '("^\\(data\\|type\\)\\>"
	 1 font-lock-keyword-face)
       '("^\\(data\\|type\\)\\>[ \t]*\\(\\sw+\\)"
	 2 font-lock-type-face)
       ;; Fontify all import and module constructions.
       '("^\\(import\\|module\\)\\>"
	 1 font-lock-keyword-face)
       '("^\\(data\\|type\\)\\>[ \t]*\\(\\sw+\\)"
	 2 font-lock-reference-face)
       ;; Fontify type declarations.
       '("^\\sw+,\\(.+\\)[ \t]+::" 1 font-lock-doc-string-face)
       ;; Fontify anything at beginning of line as a definition.
       '("^\\(\\sw+\\)" 1 font-lock-doc-string-face))
    ;; Emacs
    (list
     ;; Not really ok :( 
     '("[^\"]\\('\\\\.'\\|'.'\\)" 1 font-lock-string-face)

     '("\".*\"" 0 font-lock-string-face)

     ; HWL: eliminated the overrride field (clashes with fakeinfo.el)
     '("--.*$" 0 font-lock-comment-face)

     '("\".*--.*\"" 0 font-lock-string-face)

     ;; Fontify all builtin seperator keywords.
     (cons hugs-seperator-keywords-re 'font-lock-keyword-face)
     ;; Fontify all builtin start keywords (except data, import, module and type).
     (cons (concat "\\<\\("
		   "class\\|i\\(n\\(fix\\(\\|[lr]\\)\\|stance\\)\\)\\|primitive"
		   "\\)\\>")
	   'font-lock-keyword-face)
     ;; Fontify all type specifiers.
     '("^\\(data\\|type\\)\\>[ \t]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
     ;; Fontify all import and module constructions.
     '("^\\(import\\|module\\)\\>[ \t]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-reference-face nil t))
     ;; Fontify type declarations.
     '("^\\sw+,\\(.+\\)[ \t]+::" 
       (1 font-lock-function-name-face nil t))
     ;; Fontify anything at beginning of line as a definition.
     '("^\\sw+" . font-lock-function-name-face))
    )
  "Subdued level highlighting for `hugs-mode'.")

(defconst hugs-font-lock-keywords-2
  (append hugs-font-lock-keywords-1
	  (list
	   (cons hugs-class-keywords-re 'font-lock-type-face)
	   (cons hugs-type-keywords-re 'font-lock-type-face)
	   ))
  "Gaudy level highlighting for `hugs-mode'.")

(defvar hugs-font-lock-keywords hugs-font-lock-keywords-2
  "Default expressions to highlight in `hugs-mode'.
See the doc to `font-lock-maximum-decoration' for user configuration.")

(defun hugs-previous-def ()
  "Move backward to the beginning of the previous definition when standing
at the beginning of a definition."
  (beginning-of-line 0)
  (if (or (looking-at "^\\sw+")
	  (re-search-backward "^\\sw+" (point-min) 'move))
      (let ((name (buffer-substring-no-properties (match-beginning 0)
						  (match-end 0))))
	(if (looking-at hugs-start-keywords-re)
	    ()
	  (while (and (or (looking-at (concat "^" name "\\>"))
			  (not (looking-at "^\\sw+")))
		      (/= -1 (forward-line -1))))
	  (re-search-forward (concat "^" name "\\>")))))
  (beginning-of-line))

(defun imenu-create-hugs-index ()
  "Function for finding imenu definitions in `hugs-mode'.
Finds all definitions (classes, functions, imports, instances,
primitifs or types) in a Hugs file for the imenu package."
  (let ((index-alist '())
	(index-class-alist '())   ;; Classes
	(index-fun-alist '())     ;; Functions
	(index-imp-alist '())     ;; Imports
	(index-inst-alist '())    ;; Instances
	(index-prim-alist '())    ;; Primitives
	(index-type-alist '())    ;; Types
	start end)
    (goto-char (point-max))
    (hugs-beg-of-block)
    (while (> (point) (point-min))
      (setq start (point)
	    end   (hugs-get-end-of-line))
      (cond ((and (not (looking-at hugs-start-keywords-re))
		  (looking-at "^\\(\\sw+\\)"))
	     (push (cons (buffer-substring-no-properties (match-beginning 1)
							 (match-end 1))
			 start) index-fun-alist))
	    ((re-search-forward "^\\(data\\|type\\)\\>" end t)
	     (re-search-forward "=>" end t)
	     (if (looking-at "[ \t]*\\(\\sw+\\)")
		 (push (cons (buffer-substring-no-properties (match-beginning 1)
							     (match-end 1))
			     start) index-type-alist)))
	    ((re-search-forward "^class\\>" end t)
	     (re-search-forward "=>" end t)
	     (if (looking-at "[ \t]*\\(\\sw+\\)")
		 (push (cons (buffer-substring-no-properties (match-beginning 1)
							     (match-end 1))
			     start) index-class-alist)))
	    ((looking-at "^import[ \t]+\\(\\sw+\\)")
	     (push (cons (buffer-substring-no-properties (match-beginning 1)
							 (match-end 1))
			 start) index-imp-alist))
	    ((re-search-forward "^instance[ \t]+" end t)
	     (re-search-forward "=>[ \t]+" end t)
	     (push (cons (buffer-substring-no-properties
			  (point)
			  (progn
			    (end-of-line)
			    (re-search-backward "where")
			    (skip-chars-backward " \t")
			    (point)))
			 start) index-inst-alist))
	    ((re-search-forward "^primitive[ \t]+" end t)
	     (save-excursion
	       (let ((end (save-excursion (re-search-forward "::") (point))))
		 (while (re-search-forward "\\sw+" end t)
		   (setq start (save-excursion (forward-word -1) (point)))
		   (push (cons (buffer-substring-no-properties (match-beginning 0)
							       (match-end 0))
			       start) index-prim-alist))))))
      ;; Goto beginning of previous definition.
      (hugs-previous-def))

    (and index-type-alist
	 (push (cons "Types"
		     (sort index-type-alist 'imenu-label-cmp))
	       index-alist))
    (and index-prim-alist
	 (push (cons "Primitives"
		     (sort index-prim-alist 'imenu-label-cmp))
	       index-alist))
    (and index-inst-alist
	 (push (cons "Instances"
		     (sort index-inst-alist 'imenu-label-cmp))
	       index-alist))
    (and index-imp-alist
	 (push (cons "Imports"
		     (sort index-imp-alist 'imenu-label-cmp))
	       index-alist))
    (and index-fun-alist
	 (push (cons "Functions"
		     (sort index-fun-alist 'imenu-label-cmp))
	       index-alist))
    (and index-class-alist
	 (push (cons "Classes"
		     (sort index-class-alist 'imenu-label-cmp))
	       index-alist))
    index-alist))

(defun imenu-label-cmp (el1 el2)
  "Predicate to compare labels in lists produced by imenu-create-hugs-index."
  (string< (car el1) (car el2)))

;; Macros

(defsubst hugs-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst hugs-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defsubst hugs-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (save-excursion (hugs-beg-of-block) (point))
			       (point)))))

(defsubst hugs-within-comment ()
  (save-excursion
    (or (nth 4 (parse-partial-sexp  (save-excursion (hugs-beg-of-block) (point))
				    (point)))
	(re-search-backward "--" (hugs-get-beg-of-line) t))))

(defsubst hugs-current-line ()
  (count-lines (point-min) (point)))

;;;###autoload
(defun hugs-mode ()
  "Major mode for editing Hugs source files.

To submit a problem report, enter `\\[hugs-submit-bug-report]' from a
`hugs-mode' buffer.  Do `\\[hugs-describe-mode]' for detailed
documentation.  To see what version of `hugs-mode' you are running,
enter `\\[hugs-version]'.

COMMANDS
\\{hugs-mode-map}\

TAB indents for Hugs code.  Delete converts tabs to spaces as it moves back.

\\[hugs-complete-word] completes the word around current point with respect \
to position in code
\\[hugs-show-completions] shows all possible completions at this point.

Variables controlling indentation/edit style:

 hugs-indent-offset      (default 4)
    Indentation of Hugs statements with respect to containing block.

See also the user variables hugs-type-keywords and hugs-start-keywords

Entry to this mode calls the value of \"hugs-mode-hook\" if that value
is non-nil.  See also the documentation for variable \"hugs-command\"."
  (interactive)
  ;; Set up local variables.
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (make-local-variable 'indent-line-function)

  (set-syntax-table hugs-syntax-table)
  (setq major-mode             'hugs-mode
	mode-name              "Hugs"
	local-abbrev-table     hugs-mode-abbrev-table
	comment-start          "{-"
	comment-start-skip     "{-[^a-zA-Z0-9]*"
	;; comment-end must be set because it may hold a wrong value if
	;; this buffer had been in another mode before.
	comment-end            ""
	indent-line-function   'hugs-indent-line)
  (use-local-map hugs-mode-map)

  ;; Setting up things for Font lock support.
  (if hugs-xemacs
      ;; XEmacs
      (progn
	(put 'hugs-mode 'font-lock-keywords-case-fold-search t)
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords hugs-font-lock-keywords))
    ;; Emacs
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults 
	  '((hugs-font-lock-keywords hugs-font-lock-keywords-1
				     hugs-font-lock-keywords-2)
	    nil t ((?\' . "/")))))

  ;; Install imenu.
  ;; In emacs-20's imenu we have to bind some functions first -- HWL
  (if (and hugs-emacs-20
	   (not (fboundp 'imenu-extract-index-name-function)))
    (setq imenu-extract-index-name-function 'imenu-create-hugs-index))

  (setq imenu-create-index-function
	(function imenu-create-hugs-index))
  (if (fboundp 'imenu-add-to-menubar)
      (imenu-add-to-menubar (format "Definitions")))

  ;; Add menu 'Hugs' to the menu bar.
  (hugs-add-hugs-menu)
  
  ;; Run the mode hook.
  (if hugs-mode-hook
      (run-hooks 'hugs-mode-hook)
    (run-hooks 'hugs-mode-hook))

  (add-hook 'find-file-hooks 'hugs-find-file-hook)
  
  ;; Show startup message.
  (if hugs-startup-message
      (message "Emacs `hugs-mode' version %s. Please report bugs to %s (type \"C-c C-b\")"
	       hugs-version hugs-bugs-address))
  (setq hugs-startup-message nil))

(defun hugs-find-file-hook ()
  "Find-file hook for `hugs-mode'. See the variable `hugs-auto-load-file'."
  (if (and (eq major-mode 'hugs-mode)
	   hugs-auto-load-file)
      (let ((buf (current-buffer)))
	(hugs-load-file nil)
	(switch-to-buffer buf))))

(defun hugs-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring-no-properties (progn
				      (skip-chars-backward hugs-word-constituents)
				      (point))
				    (progn
				      (skip-chars-forward hugs-word-constituents)
				      (point)))))

(defun hugs-keep-region-active ()
  ;; do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently and doesn't its policy doesn't
  ;; require us to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;;  Electric functions

(defun electric-hugs-tab ()
  "Function called when TAB is pressed in `hugs-mode'."
  (interactive)
  (if (hugs-within-string)
      ;; Do nothing special if within a string.
      (insert "\t")
    (hugs-indent-line)))

(defun electric-hugs-return ()
  "Function called when RET is pressed in `hugs-mode'."
  (interactive)
  ;; First, check if current line should be indented.
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at hugs-start-keywords-re)
	(hugs-indent-line)))
  ;; Indent next line.
  (newline)
  (indent-to (hugs-find-indent-col)))

(defun hugs-start-newline ()
  "Function called when Meta-RET is pressed in `hugs-mode'."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at "^\\([ \t]*--+[ \t]*\\)")
	 (let ((match (buffer-substring (match-beginning 1)
					(match-end 1))))
	   (end-of-line)
	   (newline)
	   (insert match)))
	((looking-at "^\\(class\\|instance\\)\\>")
	 (end-of-line)
	 (electric-hugs-return))
	((looking-at "\\(\\sw+\\)")
	 (let ((match (buffer-substring (match-beginning 1)
					(match-end 1))))
	   (end-of-line)
	   (newline)
	   (insert match " ")))
	(t
	 (end-of-line)
	 (electric-hugs-return))))
  
;; Indentation

(defun hugs-inside-class-block ()
  "Returns t if inside a class block."
  (save-excursion
    (hugs-beg-of-block)
    (looking-at "^class\\>")))

(defun hugs-inside-instance-block ()
  "Returns t if inside an instance block."
  (save-excursion
    (hugs-beg-of-block)
    (looking-at "^instance\\>")))

(defun hugs-guess-indent-values (&optional global)
  "Guess a good value for, and change if wanted, `hugs-class-indent'
and `hugs-instance-indent'.

By default (without a prefix arg), makes buffer-local copies of the new
values. This will not affect any other Hugs buffers.
With a prefix arg, changes the global values. This affects all Hugs
buffers (that don't have their own buffer-local copy), both those currently
existing and those created later in the Emacs session" 
  (interactive "P")
  (let ((new-class-indent    4)
	(new-instance-indent 4)
	(type (if global "Global" "Local")))
    (funcall (if global 'kill-local-variable 'make-local-variable)
	     'hugs-class-indent) 
    (funcall (if global 'kill-local-variable 'make-local-variable)
	     'hugs-instance-indent) 

    ;; Code to calculate the values.

    (if (y-or-n-p (concat "Set " type " value of hugs-class-indent to " 
			  (int-to-string new-class-indent) " ? "))
	(setq hugs-class-indent new-class-indent))
    (if (y-or-n-p (concat "Set " type " value of hugs-instance-indent to " 
			  (int-to-string new-instance-indent) " ? "))
	(setq hugs-instance-indent new-instance-indent))))

(defun hugs-find-indent-col ()
  "Find the indent column in `hugs-mode'"
  (save-excursion
    ;; Goto beginning of previous line.
    (beginning-of-line 0)
    (cond ((> 2 (point)) 0)
	  ((looking-at "^[ \t]*$") 0)
	  ((looking-at "^[ \t]*--") (hugs-find-indent-col))
	  ((and (hugs-inside-class-block)
		(re-search-forward "\\<where\\>$" (hugs-get-end-of-line) t))
	   hugs-class-indent)
	  ((and (hugs-inside-instance-block)
		(re-search-forward "\\<where\\>$" (hugs-get-end-of-line) t))
	   hugs-instance-indent)
	  ((looking-at "^[ \t]*\\(let\\|where\\)\\>")
	   (forward-word 1) (skip-chars-forward " \t") (current-column))
	  (t (back-to-indentation) (current-column)))))

(defun hugs-indent-line ()
  "Indent a line in `hugs-mode'"
  (interactive)
  (let ((orig-pos (point-marker))
	(previous (hugs-find-indent-col)))
    (save-excursion
      (beginning-of-line)
      (delete-region (point)
		     (progn (skip-chars-forward " \t")
			    (point)))
      (cond ((looking-at hugs-start-keywords-re)
	     (setq previous 0))
	    ((looking-at "where\\>")
	     (setq previous (+ previous hugs-indent-offset))))
      (indent-to previous))
    (back-to-indentation)
    (goto-char orig-pos)))

;; Hugs-interface

(require 'comint)
(require 'shell)

(defvar hugs-process nil
  "The active Hugs subprocess corresponding to current buffer.")

(defvar hugs-process-buffer nil
  "Buffer used for communication with Hugs subprocess for current buffer.")

(defvar hugs-warnings 0
  "Number of warnings found by Hugs.")

(defvar hugs-warning-nr 0
  "The warning we're currently looking at.")

(defvar hugs-last-loaded-file nil
  "The last file loaded into the Hugs process.")

(defun hugs-wait-for-output ()
  "Wait until output arrives and go to the last input."
  (while (progn			
	   (goto-char comint-last-input-end)
	   (not (re-search-forward comint-prompt-regexp nil t)))
    (accept-process-output hugs-process)))

(defun hugs-project-mode ()
  "Major mode for Hugs project files.
Type \\[hugs-save-project-and-go] to save the current buffer and load
it as a Hugs project."
  (interactive)
  (setq mode-name "Hugs-project")
  (local-set-key "\C-c\C-c" 'hugs-interrupt)
  (local-set-key "\C-c\C-e" 'hugs-show-error)
  (local-set-key "\C-c\C-f" 'hugs-save-project-and-go)
  (local-set-key "\C-c\C-n" 'hugs-show-next-warning)
  (local-set-key "\C-c\C-p" 'hugs-show-next-warning))

(defun hugs-start-process ()
  "Start a Hugs process."
  (interactive)
  (message "Starting `hugs-process' %s" hugs-program)
  (setq hugs-process-buffer (make-comint "hugs" hugs-program)
	hugs-process        (get-buffer-process hugs-process-buffer))
  ;; Select Hugs buffer temporarily
  (set-buffer hugs-process-buffer)
  (make-variable-buffer-local 'shell-cd-regexp)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-cd-regexp         ":cd"
	shell-dirtrackp         t
	comint-input-sentinel   'shell-directory-tracker
	;; Don't allow for multiple prompts on one line.
	comint-prompt-regexp    "^? "
	;; comint's history syntax conflicts with Haskell syntax, eg. !!
	comint-input-autoexpand nil)
  (run-hooks 'hugs-mode-hook)
  (message ""))

(defun hugs-go (load-command file cd)
  "Save the current buffer and load its file into the Hugs process.
The first argument LOAD-COMMAND specifies how the file should be
loaded: as a new file (\":load \"), as an additional file (\":also \")
or as a Hugs project file (\":project \").

If the third argument CD is non-nil, change the Hugs process to the
current buffer's directory before loading the file.

If the variable \"hugs-command\" is set then its value will be sent to
the Hugs process after the load command.  This can be used for a
top-level expression to evaluate."
  (hack-local-variables)		;; In case they've changed
  (if (not file)
      (progn
	(save-buffer)
	(setq file (buffer-file-name))))
  (setq hugs-last-loaded-file file)
  (let ((dir (expand-file-name default-directory))
	(cmd (and (boundp 'hugs-command)
		  hugs-command
		  (if (stringp hugs-command)
		      hugs-command
		    (symbol-name hugs-command)))))
    (if (and hugs-process-buffer
	     (eq (process-status hugs-process) 'run))
	  ;; Ensure the Hugs buffer is selected.
	  (set-buffer hugs-process-buffer)
      ;; Start Hugs process.
      (hugs-start-process))
 
    (if cd (hugs-send (concat ":cd " dir)))
    ;; Wait until output arrives and go to the last input.
    (hugs-wait-for-output)
    (hugs-send load-command file)
    ;; Error message search starts from last load command.
    (setq hugs-load-end (marker-position comint-last-input-end))
    (if cmd (hugs-send cmd))
    ;; Wait until output arrives and go to the last input.
    (hugs-wait-for-output)))

(defun hugs-send (&rest string)
  "Send hugs-process the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
  ;; Wait until output arrives and go to the last input.
  (hugs-wait-for-output)
  ;; Position for this input.
  (goto-char (point-max))		
  (apply 'insert string)
  (comint-send-input)
  (setq hugs-send-end (marker-position comint-last-input-end)))

(defun hugs-load-file (cd)
  "Save a hugs buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the Hugs
process to the current buffer's directory before loading the file."
  (interactive "P")
  (hugs-go ":load " nil cd)
  (hugs-load-message))

(defun hugs-load-file-if-necessary ()
  "Load file if necessary."
  (if (and hugs-process-buffer
	   (eq (process-status hugs-process) 'run))
      ;; Load file when buffer modified or other file last loaded.
      (if (or (buffer-modified-p)
	  (not (string= hugs-last-loaded-file (buffer-file-name))))
	  (hugs-load-file nil))
    ;; Load file when no Hugs process running.
    (hugs-load-file nil)))

(defun hugs-make-temp-name ()
   (make-temp-name
    (concat (file-name-as-directory hugs-temp-directory) "hugs")))

(defun hugs-load-region (start end)
  "Send the region between START and END to the Hugs interpreter."
  (interactive "r")
  (or (< start end) (error "Region is empty."))
  (let ((fname (hugs-make-temp-name)))
    (write-region start end fname nil 'no-msg)
    (setq hugs-file-queue (append hugs-file-queue (list fname)))
    (hugs-go ":load " fname nil)
    (hugs-load-message)
    )
  )

(defun hugs-delete-file-silently (fname)
  (condition-case nil
      (delete-file fname)
    (error nil)))

(defun hugs-kill-emacs-hook ()
  "Be nice to a poor man's bits, so delete temp files."
  (while hugs-file-queue
    (hugs-delete-file-silently (car hugs-file-queue))
    (setq hugs-file-queue (cdr hugs-file-queue)))
  (if (not (or hugs-emacs-19 hugs-emacs-20 hugs-xemacs))
      ;; Run the hook we inherited, if any.
      (and (boundp 'hugs-inherited-kill-emacs-hook)
	   hugs-inherited-kill-emacs-hook
           (funcall hugs-inherited-kill-emacs-hook))))

(defun hugs-also (cd)
  "Save a hugs source buffer and add its file to the file list.
If CD (prefix argument if interactive) is non-nil, change the Hugs
process to the current buffer's directory before loading the file."
  (interactive "P")
  (hugs-go ":also " nil cd)
  (hugs-load-message))

(defun hugs-save-project-and-go (cd)
  "Save a Hugs project buffer and run its file.
If CD (prefix argument if interactive) is non-nil, change the Hugs
process to the current buffer's directory before loading the file."
  (interactive "P")
  (hugs-go ":project " nil cd))

(defun hugs-show-error ()
  "Find the first error message in the Hugs process buffer.
Find the first error since the last input and then show the relevant
point in the errant file."
  (interactive)
  (hugs-load-file-if-necessary)
  ;; Ensure the Hugs buffer is selected.
  (set-buffer hugs-process-buffer)
  ;; Error message search starts from last load command.
  (goto-char hugs-load-end)
  (if (re-search-forward "^ERROR \"\\(.*\\)\" (line \\([0-9]*\\)" nil t)
      (let ((efile (buffer-substring (match-beginning 1)
				     (match-end 1)))
	    (eline (buffer-substring (match-beginning 2)
				     (match-end 2)))
	    (emesg (buffer-substring (1+ (point))
				     (save-excursion (end-of-line) (point)))))
	(goto-char (point-max))
	(if (string= (car hugs-file-queue) efile)
	    (message "Hugs error in region")
	  (find-file efile)
	  (message "Hugs error %s %s" efile emesg)
	  (goto-line (string-to-int eline))))
    (goto-char (point-max))
    (message "There were no errors.")))

(defun hugs-show-warning (next)
  "Find the next or previous warning message in the Hugs process buffer.
Find the next or previous warning and then show the relevant point in
the file."
  (if (not hugs-tipsi-support)
      (message "Tipsi-support is not set.")
    (hugs-load-file-if-necessary)
    (cond ((eq hugs-warnings 0)
	   (message "There were no warnings."))
	  (t
	   ;; Ensure the Hugs buffer is selected.
	   (set-buffer hugs-process-buffer)
	   (if next
	       ;; We're looking for the next warning.
	       (setq hugs-warning-nr (if (< hugs-warning-nr hugs-warnings)
					 (1+ hugs-warning-nr)
				       1))
	     ;; We're looking for the previous warning.
	     (setq hugs-warning-nr (if (> hugs-warning-nr 1)
				       (1- hugs-warning-nr)
				     hugs-warnings)))
	   ;; Warning messages search starts from last load command.
	   (goto-char hugs-load-end)		
	   (re-search-forward "^WARNING \"\\(.*\\)\" (line \\([0-9]*\\)" 
			      nil t hugs-warning-nr)
	   (let ((wfile (buffer-substring (match-beginning 1)
					  (match-end 1)))
		 (wline (buffer-substring (match-beginning 2)
					  (match-end 2)))
		 (wmesg (buffer-substring (+ 3 (point))
					  (save-excursion (end-of-line) (point)))))
	     (goto-char (point-max))
	     (find-file wfile)
	     (message "Hugs warning nr %d : %s" hugs-warning-nr wmesg)
	     (goto-line (string-to-int wline))
	     (string-match "\"\\(\\sw+\\)\"" wmesg)
	     (word-search-forward
	      (substring wmesg (match-beginning 1) (match-end 1)) nil t)
	     (forward-word -1))))))

(defun hugs-show-next-warning ()
  "Find the next warning message in the Hugs process buffer.
Find the next warning and then show the relevant point in the file."
  (interactive)
  (hugs-show-warning t))

(defun hugs-show-previous-warning ()
  "Find the previous warning message in the Hugs process buffer.
Find the previous warning and then show the relevant point in the file."
  (interactive)
  (hugs-show-warning nil))

(defun hugs-load-message ()
  (interactive)
  (set-buffer hugs-process-buffer)
  ;; Error message search starts from last load command.
  (goto-char hugs-load-end)
  ;; First, look for an error.
  (let ((mesg (if (re-search-forward "^ERROR \"\\(.*\\)\" (line \\([0-9]*\\)" nil t)
		  "There was an error"
		"There were no errors")))
    ;; Maybe there are warnings.
    (if hugs-tipsi-support
	(progn
	  (setq hugs-warnings   0
		hugs-warning-nr 0)
	  ;; Warning messages search starts from last load command.
	  (goto-char hugs-load-end)
	  (while (re-search-forward "^WARNING \"\\(.*\\)\" (line \\([0-9]*\\)" nil t)
	    (setq hugs-warnings (1+ hugs-warnings)))
	  (cond ((eq hugs-warnings 0)
		 (message "%s and no warnings." mesg))
		((eq hugs-warnings 1)
		 (message "%s and 1 warning." mesg))
		(t
		 (message "%s and %d warnings" mesg hugs-warnings))))
      (message "%s." mesg)))
  (goto-char (point-max)))

(defun hugs-interrupt ()
  "Interrupt the current Hugs process."
  (interactive)
  (set-buffer hugs-process-buffer)
  (comint-interrupt-subjob))

(defun hugs-get-type ()
  "Find the type of a variable."
  (interactive)
  (let ((buf (current-buffer))
	(symbol (hugs-get-default-symbol))
	symbol-type)
    (cond ((hugs-within-comment)
	   (message "Standing in a comment"))
	  ((hugs-within-string)
	   (message "Standing in a string"))
	  ((string= symbol "")
	   (message "Sorry, don't know what you mean."))
	  ((or (member symbol hugs-start-keywords)
	       (member symbol hugs-seperator-keywords))
	   (message "%s is a keyword" symbol))
	  (t
	   (hugs-load-file-if-necessary)
	   ;; Ensure the Hugs buffer is selected.
	   (set-buffer hugs-process-buffer)

	   (hugs-send (concat ":type " symbol))
	   ;; Wait until output arrives and go to the last input.
	   (hugs-wait-for-output)

	   (save-excursion
	     (goto-char hugs-send-end)
	     (setq symbol-type (buffer-substring-no-properties
				(save-excursion (beginning-of-line) (point))
				(save-excursion (end-of-line) (point)))))
	   (set-buffer buf)
	   (message symbol-type)))))

(defun hugs-get-info ()
  "Get some info about a variable."
  (interactive)
  (let ((buf (current-buffer))
	(symbol (hugs-get-default-symbol))
	symbol-info)
    (hugs-load-file-if-necessary)
    ;; Ensure the Hugs buffer is selected.
    (set-buffer hugs-process-buffer)
    
    (hugs-send (concat ":info " symbol))
    ;; Wait until output arrives and go to the last input.
    (hugs-wait-for-output)

    (save-excursion
      (goto-char hugs-send-end)
      (setq symbol-info (buffer-substring-no-properties
			 (save-excursion (beginning-of-line) (point))
			 (save-excursion
			   (re-search-forward comint-prompt-regexp nil t)
			   (end-of-line -1)
			   (point)))))
    
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Hugs Info*"
      (set-buffer "*Hugs Info*")
      (insert symbol-info)) 
    ;; Wait for a keypress. Then delete *Completion* window.
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Hugs Info*")))))

(defun hugs-kill-process ()
  "Kill hugs subprocess and its buffer."
  (interactive)
  (if (and hugs-process-buffer
	   (buffer-name hugs-process-buffer))
      (progn
	;; First, delete all windows that display `hugs-process-buffer'."
	(delete-windows-on hugs-process-buffer)
	(kill-buffer hugs-process-buffer)
	(message "`hugs-process' killed."))
    (message "There's no `hugs-process' to kill.")))

(defun hugs-show-process-buffer ()
  "Make sure `hugs-process-buffer' is being displayed."
  (interactive)
  (cond ((and hugs-process-buffer
	      (buffer-name hugs-process-buffer))
	 (display-buffer hugs-process-buffer))
	((y-or-n-p "There's no `hugs-process-buffer' to show. Start it? ")
	 ;; Start a `hugs-process' if the user wants it.
	 (hugs-start-process)
	 (display-buffer hugs-process-buffer)))
  (message ""))

(defun hugs-hide-process-buffer ()
  "Delete all windows that display `hugs-process-buffer'."
  (interactive)
  (if (and hugs-process-buffer
	   (buffer-name hugs-process-buffer))
      (delete-windows-on hugs-process-buffer)
    (message "There's no `hugs-process-buffer' to hide.")))

;; Interactive functions

(defun hugs-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double dash (`--') comment starter."
  (interactive "r\nP")
  (or (< beg end) (error "Region is empty"))
  (let ((comment-start "-- "))
    (comment-region beg end arg)))

(defun hugs-uncomment-region (beg end)
  "Delete `comment-start' at the beginning of a line in the region."
  (interactive "r")
  (or (< start end) (error "Region is empty"))
  (comment-region beg end -1))

(defun hugs-beg-of-block ()
  "Move backward to the beginning of the current block."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at "^[ \t]*--+")
	 ;; Find the beginning of comment block.
	 (re-search-backward "^[ \t]*\\sw+" (point-min) 'move)
	 (re-search-forward "^[ \t]*--+"))
	((or (looking-at "^\\sw+")
	     (re-search-backward "^\\sw+" (point-min) 'move))
	 ;; Find the beginning of non-comment block.
	 (let ((name (buffer-substring-no-properties (match-beginning 0)
						     (match-end 0))))
	   (if (looking-at "^\\(\\|class\\|instance\\)\\>")
	       ()
	     (if (looking-at "^infix\\(\\|l\\|r\\)\\>")
		 ;; I want one infix block.
		 (setq name "infix\\(\\|l\\|r\\)"))
	     (while (and (or (looking-at (concat "^" name "\\>"))
			     (not (looking-at "^\\sw+")))
			 (/= -1 (forward-line -1))))
	     (re-search-forward (concat "^" name "\\>"))))))
  (beginning-of-line))

(defun hugs-end-of-block ()
  "Move forward to the end of the current block."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at "^[ \t]*--+")
	 ;; Find the end of this comment block.
	 (re-search-forward "^[ \t]*\\sw+" (point-max) 'move)
	 (re-search-backward "^[ \t]*--+"))
	((or (looking-at "^\\sw+")
	     (re-search-backward "^\\sw+" (point-min) t))
	 ;; Find the end of this non-comment block.
	 (let ((name (buffer-substring-no-properties (match-beginning 0)
						     (match-end 0))))
	   (cond ((looking-at "^\\(\\|class\\|instance\\)\\>")
		  (re-search-forward "^\\sw+" (point-max) 'move 2))
		 ((looking-at "^infix\\(\\|l\\|r\\)\\>")
		  ;; I want one infix block.
		  (hugs-skip-forward-lines "infix\\(\\|l\\|r\\)"))
		 ((looking-at "^\\(data\\|import\\|type\\)\\>")
		  (hugs-skip-forward-lines name))
		 (t
		  (while (and (or (looking-at (concat "^" name "\\>"))
				  (not (looking-at "^\\sw+")))
			      (/= 1 (forward-line 1))))))
	   (forward-line -1)
	   (while (looking-at "^[ \t]*\\($\\|--+\\)")
	     (forward-line -1)))))
  (end-of-line))

(defun hugs-skip-forward-lines (name)
  "Skip forward blank lines, comment lines or lines starting with NAME."
  (while (and (or (looking-at (concat "^" name "\\>"))
		  (looking-at "^[ \t]*\\($\\|--+\\)"))
	      (/= 1 (forward-line 1)))))

(defun hugs-previous-block ()
  "Move backward to the beginning of the previous block."
  (interactive)
  (hugs-beg-of-block)
  (beginning-of-line 0)
  (hugs-beg-of-block))

(defun hugs-next-block ()
  "Move forward to the beginning of the next block."
  (interactive)
  (hugs-end-of-block)
  (beginning-of-line 2)
  (or (looking-at "^\\sw+")
      (re-search-forward "^\\sw+" (point-max) 'move))
  (beginning-of-line))

(defun hugs-mark-block ()
  "Mark the current hugs block.
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (hugs-end-of-block)
  (push-mark (point))
  (hugs-beg-of-block)
  (if (fboundp 'zmacs-activate-region)
      (zmacs-activate-region)))

;; Support functions for `definition search' and `completion'

(defun hugs-looking-at-starting-- ()
  "Returns t if the cursor is standing behind some starting -'s"
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*-+[ \t]*$")))

(defun hugs-looking-at-type (value)
  "Returns t if VALUE is type-name."
  (save-excursion
    (or	(member value hugs-type-keywords)
	(re-search-backward "::" (hugs-get-beg-of-line) t)
	(re-search-backward "^type\\>.+=" (hugs-get-beg-of-line) t))))

(defun hugs-looking-at-class (value)
  "Returns t if VALUE is a class-name."
  (save-excursion
    (member value hugs-class-keywords)))

(defun hugs-looking-at-first-word ()
  "Returns t if the cursor is standing on a word starting a line."
  (save-excursion
    (if (= 0 (current-column))
	t
      (let ((col (1- (current-column))))
	(beginning-of-line)
	(cond ((looking-at "^\\sw+")
	       (< col (progn (skip-chars-forward hugs-word-constituents)
			     (current-column))))
	      (t nil))))))

(defun hugs-looking-at-import ()
  "Returns t if the cursor is standing on an `import-file'."
  (save-excursion
    (re-search-backward "^import\\>" (hugs-get-beg-of-line) t)))

(defun hugs-looking-at (value)
  "Calculate what we are looking at or should be looking at."
  (save-excursion
    (cond ((member value hugs-start-keywords) 'start-keyword)
	  ((member value hugs-seperator-keywords) 'seperator-keyword)
	  ((hugs-looking-at-class value) 'class-keyword)
	  ((hugs-looking-at-type value) 'type-keyword)
	  ((hugs-looking-at-first-word) 'first-word)
	  ((hugs-looking-at-import) 'import-file)
	  ((hugs-looking-at-starting--) 'starting--))))

;; Definition search

(defun hugs-find-decl-class (name)
  "Find the definition of class NAME in the current buffer."
  (let ((oldpos (point)))
    (goto-char (point-min))
    (or (catch 'find
	  (while (re-search-forward "^class[ \t]+" nil t)
	    (re-search-forward "=>[ \t]*" (hugs-get-end-of-line) t)
	    (if (looking-at (concat name "\\>"))
		(throw 'find t))))
	(not (goto-char oldpos)))))

(defun hugs-find-decl-type (name)
  "Find the definition of type NAME in the current buffer."
  (let ((oldpos (point)))
    (goto-char (point-min))
    (or (catch 'find
	  (while (re-search-forward "^\\(data\\|type\\)[ \t]+" nil t)
	    (re-search-forward "=>[ \t]*" (hugs-get-end-of-line) t)
	    (if (looking-at (concat name "\\>"))
		(throw 'find t))))
	(not (goto-char oldpos)))))

(defun hugs-find-decl-function (name)
  "Find the definition of function NAME in the current buffer."
  (let ((oldpos (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" name "\\>") nil t)
	(forward-word -1)
      (not (goto-char oldpos)))))

(defun hugs-find-prelude (funtype name)
  "Find the definition of NAME in `Prelude.hs'."
  (let* ((fname (hugs-script-file-name "Prelude.hs"))
	 (oldbuf (current-buffer))
	 (newbuf (if fname (find-file-noselect fname))))
    (or fname (error "Couldn't find file Prelude.hs"))
    (set-buffer newbuf)
    (if (funcall funtype name)
	(switch-to-buffer newbuf)
      (set-buffer oldbuf)
      nil)))

(defun hugs-find-recursive (funtype name)
  "Find the definition of NAME in the current buffer or the import-files."
  (if (funcall funtype name)
      (switch-to-buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
	(while (re-search-forward "^import[ \t]+\\(\\sw+\\)" nil t)
	  (let* ((match (buffer-substring-no-properties (match-beginning 1)
							(match-end 1)))
		 (fname (hugs-script-file-name match))
		 (oldbuf (current-buffer))
		 (newbuf (if fname (find-file-noselect fname))))
	    (or fname (error "Couldn't find file %s.hs" match))
	    (set-buffer newbuf)
	    (if (hugs-find-recursive funtype name)
		(throw 'found t)
	      (set-buffer oldbuf))))))))

(defun hugs-show-definition ()
  "Show definition."
  (interactive)
  (let* ((value (hugs-get-default-symbol))
	 (what (hugs-looking-at value)))
    (cond ((eq what 'import-file)
	   (let ((fname (hugs-script-file-name value)))
	     (or fname (error "Couldn't find file %s" value))
	     (find-file fname)))
	  ((eq what 'class-keyword)
	   (if (member value hugs-class-keywords)
	       (hugs-find-prelude 'hugs-find-decl-class value)
	     (if (not (hugs-find-recursive 'hugs-find-decl-class value))
		 (error "Couldn't find the definition of class %s" value))))
	  ((eq what 'type-keyword)
	   (if (member value hugs-type-keywords)
	       (hugs-find-prelude 'hugs-find-decl-type value)
	     (if (not (hugs-find-recursive 'hugs-find-decl-type value))
		 (error "Couldn't find the definition of type %s" value))))
	  ((eq what 'start-keyword)
	   (message "%s is a Hugs `start-keyword'" value))
	  ((eq what 'seperator-keyword)
	   (message "%s is a Hugs `seperator-keyword'" value))
	  (t
	   (or (hugs-find-recursive 'hugs-find-decl-function value)
	       (hugs-find-prelude 'hugs-find-decl-function value)
	       (error "Couldn't find the definition of %s" value))))))

;; Completion

(defvar hugs-str nil)
(defvar hugs-all nil)
(defvar hugs-pred nil)
(defvar hugs-buffer-to-use nil)
(defvar hugs-flag nil)

(defun hugs-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

(defun hugs-complete-class ()
  "Calculate all possible completions for classes in the current buffer."
  (save-excursion
    ;; Traverse lines.
    (goto-char (point-min))
    (while (re-search-forward "^class[ \t]+" nil t)
      (re-search-forward "=>[ \t]*" (hugs-get-end-of-line) t)
      (if (looking-at (concat hugs-str "\\sw*\\>"))
	  (let ((match (buffer-substring-no-properties (match-beginning 0)
						       (match-end 0))))
		 (if (not (member match hugs-all))
		     (setq hugs-all (cons match hugs-all))))))))

(defun hugs-complete-type ()
  "Calculate all possible completions for types in the current buffer."
  (save-excursion
    ;; Traverse lines.
    (goto-char (point-min))
    (while (re-search-forward "^\\(data\\|type\\)[ \t]+" nil t)
      (re-search-forward "=>[ \t]*" (hugs-get-end-of-line) t)
      (if (looking-at (concat hugs-str "\\sw*\\>"))
	  (let ((match (buffer-substring-no-properties (match-beginning 0)
						       (match-end 0))))
		 (if (not (member match hugs-all))
		     (setq hugs-all (cons match hugs-all))))))))

(defun hugs-function-keywords ()
  "Find all functions defined in `Prelude.hs' and store them for speed."
  (let* ((fname (hugs-script-file-name "Prelude.hs"))
	 (oldbuf (current-buffer))
	 (newbuf (if fname (find-file-noselect fname))))
    (or fname (error "Couldn't find file Prelude.hs"))
    (set-buffer newbuf)
    (save-excursion
      ;; Traverse lines.
      (goto-char (point-min))
      (while (re-search-forward "^\\sw+" nil t)
	(let ((match (buffer-substring-no-properties (match-beginning 0)
						     (match-end 0))))
	  (if (not (or (member match hugs-start-keywords)
		       ;; Skip function already found. 
		       (member match hugs-function-keywords)))
	      (setq hugs-function-keywords (cons match hugs-function-keywords)))
	  ;; Skip the rest of this block.
	  (hugs-end-of-block))))
    (set-buffer oldbuf)))

(defun hugs-complete-function ()
  "Calculate all possible completions for functions when not standing at the
beginning of the line."
  (save-excursion
    ;; Traverse lines.
    (goto-char (point-min))
    (while (re-search-forward (concat "^\\(" hugs-str "\\sw*\\)") nil t)
      (let ((match (buffer-substring-no-properties (match-beginning 0)
						   (match-end 0))))
	(if (not (or (member match hugs-start-keywords)
		     ;; Skip function already found. 
		     (member match hugs-all)))
	    (setq hugs-all (cons match hugs-all)))
	    ;; Skip the rest of this block.
	(hugs-end-of-block)))))

(defun hugs-complete-function-definition ()
  "Calculate all possible completions for functions when standing at the beginning
of the line."
  (save-excursion
    ;; Maybe we're not starting a new block.
    (beginning-of-line 0)
    (hugs-beg-of-block)
    (if (looking-at (concat "^\\(" hugs-str "\\sw*\\)"))
	(let ((match (buffer-substring-no-properties (match-beginning 1)
						     (match-end 1))))
	  (if (not (or (member match hugs-start-keywords)
		       ;; Skip function already found. 
		       (member match hugs-all)))
	      (setq hugs-all (cons match hugs-all)))))
    ;; Traverse lines.
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (and (looking-at "^\\sw+.*::")
	       (not (looking-at hugs-start-keywords-re)))
	  ;; This is the type-definition of at least one function.
	  (let ((end (save-excursion (re-search-forward "::") (point))))
	    (while (re-search-forward "\\(\\sw+\\)[ \t]*,?[ \t]*" end t)
	      (let ((match (buffer-substring-no-properties (match-beginning 1)
							   (match-end 1))))
		(if (and (string-match (concat "^" hugs-str) match)
			 (not (or 
			       ;; Skip if function already found earlier. 
			       (member match hugs-all)
			       ;; Skip if function already defined elsewhere.
			       (save-excursion
				 (beginning-of-line 2)
				 (re-search-forward (concat "^" match "\\>") nil t )))))
		    (setq hugs-all (cons match hugs-all)))))
	    ;; Skip the rest of this block.
	    (hugs-end-of-block))
	;; Go to beginning of next line.
	(beginning-of-line 2)))))
  
(defun hugs-complete-recursive (funtype)
  "Calculate all possible completions in the current buffer or the import-files."
  (funcall funtype)
  (if hugs-recursive-completions
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^import[ \t]+\\(\\sw+\\)" nil t)
	  (let* ((match (buffer-substring-no-properties (match-beginning 1)
							(match-end 1)))
		 (fname (hugs-script-file-name match))
		 (oldbuf (current-buffer))
		 (newbuf (if fname (find-file-noselect fname))))
	    (or fname (error "Couldn't find file %s.hs" match))
	    (set-buffer newbuf)
	    (hugs-complete-recursive funtype)
	    (set-buffer oldbuf))))))

(defun hugs-complete-import ()
  "Calculate a possible completions for `import-files'"
  (let ((path (hugs-script-file-path))
	(scripts nil))
    (while path
      (setq scripts (append scripts
			    (directory-files (car path) nil 
					     (concat "^" hugs-str ".*\\.hs$")))
	    path (cdr path)))
    (while scripts
      (setq hugs-all (cons (substring (car scripts) 0 -3) hugs-all)
	    scripts (cdr scripts)))))

(defun hugs-complete-starting-- ()
  "Calculate a possible completions for a starting -"
  (save-excursion
    ;; Go to beginning of previous line.
    (beginning-of-line 0)
    (let (match)
      (cond ((looking-at "^[ \t]*\\(--+[ \t]*\\)")
	     (setq match (buffer-substring-no-properties (match-beginning 1)
							 (match-end 1))))
	    (t (setq match "--")))
      (setq hugs-all (cons match hugs-all)))))

(defun hugs-complete-keyword (keyword-list)
  "Give list of all possible completions of keywords in keyword-list."
  (mapcar '(lambda (s) 
	     (if (string-match (concat "\\<" hugs-str) s)
		 (if (or (null hugs-pred)
			 (funcall hugs-pred s))
		     (setq hugs-all (cons s hugs-all)))))
	  keyword-list))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defun hugs-completion (hugs-str hugs-pred hugs-flag)
  ;; Be sure to have something to complete.
  (if (string= hugs-str "")
      (setq hugs-str "\\sw+"))
  ;; Initialise function-keywords defined in `Prelude.hs' if necessary.
  (if (not hugs-function-keywords)
      (hugs-function-keywords))
  (save-excursion
    (let ((hugs-all nil)
	  (what (hugs-looking-at hugs-str)))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use hugs-completions
      (set-buffer hugs-buffer-to-use)
      ;; Call the right completion function.
      (cond ((eq what 'first-word)
	     (hugs-complete-keyword hugs-start-keywords)
	     (hugs-complete-function-definition))
	    ((eq what 'type-keyword)
	     (hugs-complete-keyword hugs-type-keywords)
	     (hugs-complete-recursive 'hugs-complete-type))
	    ((eq what 'class-keyword)
	     (hugs-complete-keyword hugs-class-keywords)
	     (hugs-complete-recursive 'hugs-complete-class))
	    ((eq what 'import-file)
	     (hugs-complete-import))
	    ((eq what 'starting--)
	     (hugs-complete-starting--))
	    (t
	     (hugs-complete-keyword hugs-seperator-keywords)
	     (hugs-complete-keyword hugs-function-keywords)
	     (hugs-complete-recursive 'hugs-complete-function)))
      ;; Sort the complete list.
      (setq hugs-all (sort hugs-all 'string<))

      ;; Now we have built a list of all matches. Give response to caller
      (hugs-completion-response))))

(defun hugs-completion-response ()
  (cond ((or (equal hugs-flag 'lambda) (null hugs-flag))
	 ;; This was not called by all-completions
	 (if (null hugs-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr hugs-all))
		  (match (car hugs-all))
		  (min (length match))
		  exact tmp)
	     (if (string= match hugs-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (hugs-string-diff match (car elm))) min)
		     (setq min   tmp
			   match (substring match 0 min)))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) hugs-str)
		     (setq match t
			   elm   nil)
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal hugs-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(hugs-flag
	 hugs-all)))

(defvar hugs-last-word-numb 0)
(defvar hugs-last-word-shown nil)
(defvar hugs-last-completions nil)

(defun hugs-complete-word ()
  "Complete word at current point.
\(See also `hugs-toggle-completions', `hugs-type-keywords',
`hugs-start-keywords' and `hugs-separator-keywords'.)"
  (interactive)
  (cond ((and (not (hugs-looking-at-starting--)) (hugs-within-comment))
	 ;; Don't complete if within a comment.
	 (message "(No completion in a comment)"))
	((hugs-within-string)
	 ;; Don't complete if within a string.
	 (message "(No completion in a string)"))
	(t
	 (let* ((b (save-excursion 
		     (skip-chars-backward (if (hugs-looking-at-starting--) 
					      "-" hugs-word-constituents)) 
		     (point)))
		(e (save-excursion
		     (skip-chars-forward hugs-word-constituents) 
		     (point)))
		(hugs-str (buffer-substring-no-properties b e))
		;; The following variable is used in hugs-completion
		(hugs-buffer-to-use (current-buffer))
		(allcomp (if (and hugs-toggle-completions
				  (string= hugs-last-word-shown hugs-str))
			     hugs-last-completions
			   (all-completions hugs-str 'hugs-completion)))
		(match (if hugs-toggle-completions
			   "" (try-completion
			       hugs-str (mapcar '(lambda (elm)
						   (cons elm 0)) allcomp)))))
	   ;; Delete old string
	   (delete-region b e)
	   
	   ;; Toggle-completions inserts whole labels
	   (if hugs-toggle-completions
	       (progn
		 ;; Update entry number in list
		 (setq hugs-last-completions allcomp
		       hugs-last-word-numb 
		       (if (>= hugs-last-word-numb (1- (length allcomp)))
			   0
			 (1+ hugs-last-word-numb)))
		 (setq hugs-last-word-shown (elt allcomp hugs-last-word-numb))
		 ;; Display next match or same string if no match was found
		 (if (not (null allcomp))
		     (insert "" hugs-last-word-shown)
		   (insert "" hugs-str)
		   (message "(No match)")))
	     ;; The other form of completion does not necessarily do that.

	     ;; Insert match if found, or the original string if no match
	     (if (or (null match) (equal match 't))
		 (progn (insert "" hugs-str)
			(message "(No match)"))
	       (insert "" match))
	     ;; Give message about current status of completion.
	     (cond ((equal match 't)
		    (if (not (null (cdr allcomp)))
			(message "(Complete but not unique)")
		      (message "(Sole completion)")))
		   ;; Display buffer if the current completion didn't help 
		   ;; on completing the label.
		   ((and (not (null (cdr allcomp))) (= (length hugs-str)
						       (length match)))
		    (with-output-to-temp-buffer "*Completions*"
		      (display-completion-list allcomp))
		    ;; Wait for a keypress. Then delete *Completion* window.
		    (momentary-string-display "" (point))
		    (delete-window (get-buffer-window (get-buffer "*Completions*")))
		    )))))))

(defun hugs-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward hugs-word-constituents) 
			    (point)))
	 (e (save-excursion (skip-chars-forward hugs-word-constituents) 
			    (point)))
	 (hugs-str (buffer-substring-no-properties b e))
	 ;; The following variable is used in hugs-completion.
	 (hugs-buffer-to-use (current-buffer))
	 (allcomp (if (and hugs-toggle-completions
			   (string= hugs-last-word-shown hugs-str))
		      hugs-last-completions
		    (all-completions hugs-str 'hugs-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a keypress. Then delete *Completion* window.
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))

(defun hugs-version ()
  "Echo the current version of `hugs-mode' in the minibuffer."
  (interactive)
  (message "Using `hugs-mode' version %s" hugs-version)
  (hugs-keep-region-active))

;; Get reporter-submit-bug-report when byte-compiling.
(eval-when-compile
  (require 'reporter))

(defun hugs-submit-bug-report (enhancement-p)
  "Submit via mail a bug report on `hugs-mode'.
With \\[universal-argument] just submit an enhancement request."
  (interactive
   (list (not (y-or-n-p
	       "Is this a bug report? (hit `n' to send other comments) "))))
  (let ((reporter-prompt-for-summary-p (if enhancement-p
					   "(Very) brief summary: "
					 t)))
    (reporter-submit-bug-report
     hugs-bugs-address	
     (concat "hugs-mode " hugs-version)
     ;; Varlist.
     (if enhancement-p nil
       '(hugs-indent-offset
	 hugs-class-indent
	 hugs-instance-indent
	 hugs-startup-message
	 hugs-program
	 hugs-tipsi-support
	 hugs-auto-load-file
	 hugs-script-file-path
	 hugs-recursive-completions
	 hugs-toggle-completions
	 hugs-command))
     nil				;; pre-hooks
     nil				;; post-hooks
     "Hi Chris,")                       ;; salutation
    (if enhancement-p nil
      (set-mark (point))
      (insert
       "Please replace this text with a sufficiently large code sample\n\
and an exact recipe so that I can reproduce your problem.  Failure\n\
to do so may mean a greater delay in fixing your bug.\n\n")
      (exchange-point-and-mark)
      (hugs-keep-region-active))))

(defun hugs-describe-mode ()
  "Dump long form of `hugs-mode' docs."
  (interactive))

;; Arrange to kill temp files when Emacs exists. Ripped from python-mode.
(if (or hugs-emacs-19 hugs-emacs-20 hugs-xemacs)
    (add-hook 'kill-emacs-hook 'hugs-kill-emacs-hook)
  ;; Have to trust that other people are as respectful of our hook
  ;; fiddling as we are of theirs
  (if (boundp 'hugs-inherited-kill-emacs-hook)
      ;; We were loaded before -- trust others not to have screwed us
      ;; in the meantime (no choice, really)
      nil
    ;; else arrange for our hook to run theirs
    (setq hugs-inherited-kill-emacs-hook kill-emacs-hook)
    (setq kill-emacs-hook 'hugs-kill-emacs-hook)))

;; Announce your existence to the world at large.
(provide 'hugs-mode)

;; "Why do we have to hide from the Borg, Captain?"
;; "Because we use emacs, Data. They use vi."

;;; End of hugs-mode.el


