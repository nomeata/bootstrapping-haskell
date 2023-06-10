;;; haskell-doc.el --- show function types in echo area

;; Time-stamp: <Mon Nov 03 1997 00:29:28 Stardate: [-30]0190.10 hwloidl>

;; Copyright (C) 1997 Hans-Wolfgang Loidl

;; Author: Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Maintainer: Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Keywords: extensions, minor mode, language mode, Haskell
;; Created: 1997-06-17
;; Revision: $Revision: 1.1 $
;; FTP archive: /ftp@ftp.dcs.gla.ac.uk:/pub/glasgow-fp/authors/Hans_Loidl/Elisp/haskell-doc.el
;; Status: Beta version

;; $Id: haskell-doc.el,v 1.1 1997/11/03 03:29:21 hwloidl Exp $

;;; Copyright:
;;  ==========

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  ===========

;; This program shows the type of the Haskell function under the cursor in the
;; minibuffer. It acts as a kind of "emacs background process", by regularly
;; checking the word under the cursor and matching it against a list of
;; prelude, library, local and global functions.

;; The preferred usage of this package is in combination with `hugs-mode'.
;; In that case `haskell-doc' checks an internal variable updated by `imenu'
;; to access the types of all local functions. In `haskell-mode' this is not 
;; possible. However, types of prelude functions are still shown.

;; To show types of global functions, i.e. functions defined in a module 
;; imported by the current module, call the function 
;; `turn-on-haskell-doc-global-types'. This automatically loads all modules
;; and builds `imenu' tables to get the types of all functions (again this 
;; currently requires `hugs-mode'). 
;; Note: The modules are loaded recursively, so you might pull in
;;       many modules by just turning on global function support.
;; This features is currently not very well supported.

;; This program was inspired by the `eldoc.el' package by Noah Friedman.

;;; Installation:
;;  =============

;; One useful way to enable this minor mode is to put the following in your
;; .emacs:
;;
;;      (autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)

;;   and depending on the major mode you use for your Haskell programs:
;;      (add-hook 'hugs-mode-hook 'turn-on-haskell-doc-mode)    ; hugs-mode
;;     or
;;      (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode) ; haskell-mode

;;; Customisation:
;;  ==============

;; You can control what exactly is shown by setting the following variables to
;; either t or nil:
;;  `haskell-doc-show-global-types' (default: nil)
;;  `haskell-doc-show-reserved'     (default: t)
;;  `haskell-doc-show-prelude'      (default: '1.4)
;;  `haskell-doc-show-strategy'     (default: t)
;;  `haskell-doc-show-user-defined' (default: t)

;; If you want to define your own strings for some identifiers define an
;; alist of (ID . STRING) and set `haskell-doc-show-user-defined' to t. 
;; E.g:
;;
;;   (setq haskell-doc-show-user-defined t)
;;   (setq haskell-doc-user-defined-ids 
;;	(list 
;;	   '("main" . "just another pathetic main function")
;;	   '("foo" . "a very dummy name")
;;	   '("bar" . "another dummy name")))

;;; Internals:
;;  ==========

;; `haskell-doc-mode' is implemented as a minor-mode. So, you can combine it
;; with any other mode. To enable it just type
;;   M-x turn-on-haskell-doc-mode

;; These are the names of the functions that can be called directly by the
;; user (with keybindings in `hugs-mode' and `haskell-mode'):
;;  `haskell-doc-mode' ... toggle haskell-doc-mode; with prefix turn it on
;;                        unconditionally if the prefix is greater 0 otherwise
;;                        turn it off
;;                        Key: CTRL-c CTRL-o (CTRL-u CTRL-c CTRL-o)
;;  `haskell-doc-ask-mouse-for-type' ... show the type of the id under the mouse
;;                                      Key: mouse-3
;;  `haskell-doc-show-reserved'     ... toggle echoing of reserved id's types
;;  `haskell-doc-show-prelude'      ... toggle echoing of prelude id's types
;;  `haskell-doc-show-strategy'     ... toggle echoing of strategy id's types
;;  `haskell-doc-show-user-defined' ... toggle echoing of user def id's types
;;  `haskell-doc-check-active' ... check whether haskell-doc is active via the 
;;                                `post-command-idle-hook' (for testing); 
;;                                 Key: ESC-/

;;; ToDo:
;;  =====

;;   - Fix byte-compile problems in `haskell-doc-prelude-types' for getArgs etc 
;;   - Write a parser for .hi files and make haskell-doc independent from
;;     hugs-mode. Read library interfaces via this parser.
;;   - Support both Haskell 1.4 and 1.2
;;   - Indicate kind of object with colours
;;   - Handle multi-line types
;;   - Encode i-am-fct info in the alist of ids and types.
;;   - Replace the usage of `post-command-idle-hook' with idle timers

;;; Bugs:
;;  =====

;;   - Some prelude fcts aren't displayed properly. This might be due to a 
;;     name clash of Haskell and Elisp functions (e.g. length) which
;;     confuses emacs when reading `haskell-doc-prelude-types'

;;; Changelog:
;;  ==========
;;  $Log: haskell-doc.el,v $
;;  Revision 1.1  1997/11/03 03:29:21  hwloidl
;;  Emacs minor mode for showing type of fct in the echo area
;;  First official release (V1.3)
;;
;;  Revision 1.3  1997/11/03 00:48:03  hwloidl
;;  Major revision for first release.
;;  Added alists for showing prelude fcts, haskell syntax, and strategies
;;  Added mouse interface to show type under mouse
;;  Fixed bug which causes demon to fall over
;;  Works now with hugs-mode and haskell-mode under emacs 19.34,20 and xemacs 19.15
;;

;;; Code:
;;  =====

;@menu
;* Constants and Variables::	
;* Install as minor mode::	
;* Menubar Support::		
;* Haskell Doc Mode::		
;* Switch it on or off::	
;* Check::			
;* Top level function::		
;* Mouse interface::		
;* Print fctsym::		
;* Movement::			
;* Bug Reports::		
;* Visit home site::		
;* Index::			
;* Token::			
;@end menu

;@node top, Constants and Variables, (dir), (dir)
;@top

;@node Constants and Variables, Install as minor mode, top, top
;@section Constants and Variables

;@menu
;* Emacs portability::		
;* Maintenance stuff::		
;* Mode Variable::		
;* Variables::			
;* Prelude types::		
;* Test membership::		
;@end menu

;@node Emacs portability, Maintenance stuff, Constants and Variables, Constants and Variables
;@subsection Emacs portability

(defconst haskell-doc-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version)
  "Running under XEmacs?")

(defconst haskell-doc-emacs-p (and (or (string-match "^19" emacs-version)
				       (string-match "^20" emacs-version))
				(not haskell-doc-xemacs-p))
  "Running under Emacs?")

;@node Maintenance stuff, Mode Variable, Emacs portability, Constants and Variables
;@subsection Maintenance stuff

(defconst haskell-doc-version "$Revision: 1.1 $"
 "Version of `haskell-doc' as RCS Revision.")

(defconst haskell-doc-maintainer "Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>"
  "Maintainer of `haskell-doc'.")

(defconst haskell-doc-ftp-site "/ftp@ftp.dcs.gla.ac.uk:/pub/glasgow-fp/authors/Hans_Loidl/Elisp/"
  "Main FTP site with latest version of `haskell-doc' and sample files.")

;@node Mode Variable, Variables, Maintenance stuff, Constants and Variables
;@subsection Mode Variable

;;;###autoload
(defvar haskell-doc-mode nil
  "*If non-nil, show the type of the function near point or a related comment.

If the identifier near point is a Haskell keyword and the variable
`haskell-doc-show-reserved' is non-nil show a one line summary
of the syntax.

If the identifier near point is a Prelude or one of the standard library 
functions and `haskell-doc-show-prelude' is non-nil show its type. Currently 
only Haskell 1.4 functions are supported. In future versions the 
`haskell-doc-show-prelude' variable should determine which prelude/library
to use for type lookup.

If the identifier near point is local \(i.e. defined in this module\) check
the `imenu' list of functions for the type. This obviously requires that
your language mode uses `imenu' \(`hugs-mode' 0.6 for example\).

If the identifier near point is global \(i.e. defined in an imported module\) 
and the variable `haskell-doc-show-global-types' is non-nil show the type of its 
function.

If the identifier near point is a standard strategy or a function, type related
related to strategies and `haskell-doc-show-strategy' is non-nil show the type
of the function. Strategies are special to the parallel execution of Haskell.
If you're not interested in that just turn it off.

If the identifier near point is a user defined function that occurs as key
in the alist `haskell-doc-user-defined-ids' and the variable 
`haskell-doc-show-user-defined' is non-nil show the type of the function.

This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-mode)

(defvar haskell-doc-mode-hook nil
 "Hook invoked when entering haskell-doc-mode.")

(defvar haskell-doc-index nil
 "Variable holding an alist matching file names to fct-type alists.
The function `haskell-doc-make-global-fct-index' rebuilds this variables \(similar to an
`imenu' rescan\).
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-index)

(defvar haskell-doc-show-global-types nil
 "*If non-nil, search for the types of global functions by loading the files.
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-show-global-types)

(defvar haskell-doc-show-reserved t
 "*If non-nil, show a documentation string for reserved ids.
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-show-reserved)

(defvar haskell-doc-show-prelude '1.4
 "*If non-nil, show a documentation string for prelude functions.
Possible values are Haskell versions. Currently, only `1.4' is supported.
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-show-prelude)

(defvar haskell-doc-show-strategy t
 "*If non-nil, show a documentation string for strategies.
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-show-strategy)

(defvar haskell-doc-show-user-defined t
 "*If non-nil, show a documentation string for user defined ids.
This variable is buffer-local.")
(make-variable-buffer-local 'haskell-doc-show-user-defined)

;@node Variables, Prelude types, Mode Variable, Constants and Variables
;@subsection Variables

(defvar haskell-doc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required.")

(defvar haskell-doc-argument-case 'identity ; 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.")

(defvar haskell-doc-mode-message-commands nil
  "*Obarray of command names where it is appropriate to print in the echo area.

This is not done for all commands since some print their own
messages in the echo area, and these functions would instantly overwrite
them.  But self-insert-command as well as most motion commands are good
candidates.

It is probably best to manipulate this data structure with the commands
`haskell-doc-add-command' and `haskell-doc-remove-command'.")

;(cond ((null haskell-doc-mode-message-commands)
;       ;; If you increase the number of buckets, keep it a prime number.
;       (setq haskell-doc-mode-message-commands (make-vector 31 0))
;       (let ((list '("self-insert-command"
;                     "next-"         "previous-"
;                     "forward-"      "backward-"
;                     "beginning-of-" "end-of-"
;                     "goto-"
;                     "recenter"
;                     "scroll-"))
;             (syms nil))
;         (while list
;           (setq syms (all-completions (car list) obarray 'fboundp))
;           (setq list (cdr list))
;           (while syms
;             (set (intern (car syms) haskell-doc-mode-message-commands) t)
;             (setq syms (cdr syms)))))))

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar haskell-doc-last-data '(nil . nil))

(defvar haskell-doc-minor-mode-string " Haskell-Doc"
  "*String to display in mode line when Haskell-Doc Mode is enabled.")

(defconst haskell-doc-varlist
  (list
   'haskell-doc-xemacs-p
   'haskell-doc-emacs-p
   'haskell-doc-version
   'haskell-doc-mode
   'haskell-doc-mode-hook
   'haskell-doc-index
   'haskell-doc-show-global-types
   'haskell-doc-show-reserved
   'haskell-doc-show-prelude
   'haskell-doc-show-strategy
   'haskell-doc-show-user-defined
   'haskell-doc-idle-delay
   'haskell-doc-argument-case
   'haskell-doc-mode-message-commands
  )
  "List of variables sent via `haskell-doc-submit-bug-report'.")

;@node Prelude types, Test membership, Variables, Constants and Variables
;@subsection Prelude types

;@cindex haskell-doc-reserved-ids

(defvar haskell-doc-reserved-ids
 (list
  '("case" . "case exp of { alts [;] }")
  '("class" . "class [context =>] simpleclass [where { cbody [;] }]")
  '("data" . "data [context =>] simpletype = constrs [deriving]")
  '("default" . "default (type1 , ... , typen)")
  '("deriving" . "deriving (dclass | (dclass1, ... , dclassn))") ; used with data or newtype
  '("do" . "do { stmts [;] }  stmts -> exp [; stmts] | pat <- exp ; stmts | let decllist ; stmts")
  '("else" . "if exp then exp else exp")
  '("if" . "if exp then exp else exp")
  '("import" . "import [qualified] modid [as modid] [impspec]")
  '("in" . "let decllist in exp")
  '("infix" . "infix [digit] ops")
  '("infixl" . "infixl [digit] ops")
  '("infixr" . "infixr [digit] ops")
  '("instance" . "instance [context =>] qtycls inst [where { valdefs [;] }]")
  '("let" . "let { decl; ...; decl [;] } in exp")
  '("module" . "module modid [exports] where body")
  '("newtype" . "newtype [context =>] simpletype = con atype [deriving]")
  '("of" . "case exp of { alts [;] }")
  '("then" . "if exp then exp else exp")
  '("type" . "type simpletype = type")
  '("where" . "exp where { decl; ...; decl [;] }") ; check that ; see also class, instance, module
  '("as" . "import [qualified] modid [as modid] [impspec]")
  '("qualified" . "import [qualified] modid [as modid] [impspec]")
  '("hiding" . "hiding ( import1 , ... , importn [ , ] )")
 )
 "An alist of reserved identifiers and a string describing the construct they are used in.")

;@cindex haskell-doc-prelude-types

(defvar haskell-doc-prelude-types
 (list
 ; Taken from the prelude of the 1.4 report
 ; ToDo: clean this up
 ; ToDo: add overloaded fcts at the beginning of the report.
; '("subtract"��������� . "(Num�a)�=>�a�->�a�->�a")
; '("odd"��������       . "(Integral�a)�=>�a�->�Bool")
; '("even"��������      . "(Integral�a)�=>�a�->�Bool")
; '("gcd"�������������� . "(Integral�a)�=>�a�->�a�->�a")
; '("lcm"�������������� . "(Integral�a)�=>�a�->�a�->�a")
; ;'("^"�������������� �. "(Num�a,�Integral�b)�=>�a�->�b�->�a")
; ;'("^^"������������� �. "(Fractional�a,�Integral�b)�=>�a�->�b�->�a")
; '("fromIntegral"����� . "(Integral�a,�Num�b)�=>�a�->�b")
; '("fromRealFrac"����� . "(RealFrac�a,�Fractional�b)�=>�a�->�b")
; '("atan2"������������ . "(RealFloat�a)�=>�a�->�a�->�a")
; '("map"�������������� . "(Functor�f) => (a�->�b)�->�f�a�->�f�b")
; ;'("(>>=)�������������. "(Monad�m) => m�a�->�(a�->�m�b)�->�m�b")
; ;'("(>>)��������������. "(Monad�m) => m�a�->�m�b�->�m�b")
; '("return"����������� . "(Monad�m) => a�->�m�a")
; '("zero"������������� . "(Monad�m) => m�a")
; ;'("(++)��������������. "(Monad�m) => m�a�->�m�a�->�m�a")
; '("accumulate"������� . "Monad�m�=>�[m�a]�->�m�[a]�")
; '("sequence"��������� . "Monad�m�=>�[m�a]�->�m�()�")
; '("mapM"������������� . "Monad�m�=>�(a�->�m�b)�->�[a]�->�m�[b]")
; '("mapM_"������������ . "Monad�m�=>�(a�->�m�b)�->�[a]�->�m�()")
; '("guard"������������ . "MonadZero�m�=>�Bool�->�m�()")
; '("filter"����������� . "MonadZero�m�=>�(a�->�Bool)�->�m�a�->�m�a")
; '("concat"����������� . "MonadPlus�m�=>�[m�a]�->�m�a")
; '("applyM"����������� . "Monad�m�=>�(a�->�m�b)�->�m�a�->�m�b")
; '("seq"�������������� . "(Eval�a) => a�->�b�->�b")
; '("strict"����������� . "(Eval�a) => (a�->�b)�->�a�->�b")
; '("id"��������������� . "a�->�a")
; '("const"������������ . "a�->�b�->�a")
; ; '("."���������������. "(b�->�c)�->�(a�->�b)�->�a�->�c")
; '("flip"������������� . "(a�->�b�->�c)�->�b�->�a�->�c")
; ;'("$"�������������� �. "(a�->�b)�->�a�->�b")
; ;'("&&"�������       �. "Bool�->�Bool�->�Bool")
; ;'("||"�������       �. "Bool�->�Bool�->�Bool")
; '("not"�������������� . "Bool�->�Bool")
; '("maybe"������������ . "b�->�(a�->�b)�->�Maybe�a�->�b")
; '("either"����������� . "(a�->�c)�->�(b�->�c)�->�Either�a�b�->�c")
; '("numericEnumFrom"��������� . "(Real�a)�=>�a�->�[a]")
; '("numericEnumFromThen"����� . "(Real�a)�=>�a�->�a�->�[a]")
; '("numericEnumFromTo"������� . "(Real�a)�=>�a�->�a�->�[a]")
; '("numericEnumFromThenTo"��� . "(Real�a)�=>�a�->�a�->�a�->�[a]")
; '("fst"�������������� . "(a,b)�->�a")
; '("snd"�������������� . "(a,b)�->�b")
; '("curry"������������ . "((a,�b)�->�c)�->�a�->�b�->�c")
; '("uncurry"���������� . "(a�->�b�->�c)�->�((a,�b)�->�c)")
; '("until"������������ . "(a�->�Bool)�->�(a�->�a)�->�a�->�a")
; '("asTypeOf"��������� . "a�->�a�->�a")
; '("error"������������ . "String�->�a")
; '("undefined"�������� . "a")
; ; List fcts
; '("head"������������� . "[a]�->�a")
; '("last"������������� . "[a]�->�a")
; '("tail"������������� . "[a]�->�[a]")
; '("init"������������� . "[a]�->�[a]")
; '("null"������������� . "[a]�->�Bool")
; '("length"����������� . "[a]�->�Int")
; ; '("!!"������������� . "[a]�->�Int�->�a")
; '("foldl"������������ . "(a�->�b�->�a)�->�a�->�[b]�->�a")
; '("foldl1"����������� . "(a�->�a�->�a)�->�[a]�->�a")
; '("scanl"������������ . "(a�->�b�->�a)�->�a�->�[b]�->�[a]")
; '("scanl1"����������� . "(a�->�a�->�a)�->�[a]�->�[a]")
; '("foldr"������������ . "(a�->�b�->�b)�->�b�->�[a]�->�b")
; '("foldr1"����������� . "(a�->�a�->�a)�->�[a]�->�a")
; '("scanr"������������ . "(a�->�b�->�b)�->�b�->�[a]�->�[b]")
; '("scanr1"����������� . "(a�->�a�->�a)�->�[a]�->�[a]")
; '("iterate"���������� . "(a�->�a)�->�a�->�[a]")
; '("repeat"����������� . "a�->�[a]")
; '("replicate"�������� . "Int�->�a�->�[a]")
; '("cycle"������������ . "[a]�->�[a]")
; '("take"������������� . "Int�->�[a]�->�[a]")
; '("drop"������������� . "Int�->�[a]�->�[a]")
; '("splitAt"���������� . "Int�->�[a]�->�([a],[a])")
; '("takeWhile"�������� . "(a�->�Bool)�->�[a]�->�[a]")
; '("dropWhile"�������� . "(a�->�Bool)�->�[a]�->�[a]")
; '("span"������������� . "(a�->�Bool)�->�[a]�->�([a],[a])")
; '("break"������������ . "(a�->�Bool)�->�[a]�->�([a],[a])")
; '("lines"������������ . "String�->�[String]")
; '("words"������������ . "String�->�[String]")
; '("unlines"���������� . "[String]�->�String")
; '("unwords"���������� . "[String]�->�String")
; '("reverse"���������� . "[a]�->�[a]")
; '("and"����������     . "[Bool]�->�Bool")
; '("or"����������      . "[Bool]�->�Bool")
; '("any"���������      . "(a�->�Bool)�->�[a]�->�Bool")
; '("all"���������      . "(a�->�Bool)�->�[a]�->�Bool")
; '("elem"����          . "(Eq�a)�=>�a�->�[a]�->�Bool")
; '("notElem"����       . "(Eq�a)�=>�a�->�[a]�->�Bool")
; '("lookup"����������� . "(Eq�a)�=>�a�->�[(a,b)]�->�Maybe�b")
; '("sum"�����          . "(Num�a)�=>�[a]�->�a")
; '("product"�����      . "(Num�a)�=>�[a]�->�a")
; '("maximum"�          . "(Ord�a)�=>�[a]�->�a")
; '("minimum"�          . "(Ord�a)�=>�[a]�->�a")
; '("concatMap"�������� . "(a�->�[b])�->�[a]�->�[b]")
; '("zip"�������������� . "[a]�->�[b]�->�[(a,b)]")
; '("zip3"������������� . "[a]�->�[b]�->�[c]�->�[(a,b,c)]")
; '("zipWith"���������� . "(a->b->c)�->�[a]->[b]->[c]")
; '("zipWith3"��������� . "(a->b->c->d)�->�[a]->[b]->[c]->[d]")
; '("unzip"������������ . "[(a,b)]�->�([a],[b])")
; '("unzip3"����������� . "[(a,b,c)]�->�([a],[b],[c])")
; '("readsPrec"�������� . "(Read�a) => Int�->�ReadS�a")
; '("readList"��������� . "(Read�a) => ReadS�[a]")
; '("showsPrec"�������� . "(Show�a) => Int�->�a�->�ShowS")
; '("showList"��������� . "(Show�a) => [a]�->�ShowS")
; '("reads"������������ . "(Read�a)�=>�ReadS�a")
; '("shows"������������ . "(Show�a)�=>�a�->�ShowS")
; '("read"������������� . "(Read�a)�=>�String�->�a")
; '("show"������������� . "(Show�a)�=>�a�->�String")
; '("showChar"��������� . "Char�->�ShowS")
; '("showString"������� . "String�->�ShowS")
; '("showParen"�������� . "Bool�->�ShowS�->�ShowS")
; '("readParen"�������� . "Bool�->�ReadS�a�->�ReadS�a")
; '("lex"�������������� . "ReadS�String")
; '("lexDigits"�������� . "ReadS�String�")
; '("nonnull"���������� . "(Char�->�Bool)�->�ReadS�String")
; '("lexLitChar"������� . "ReadS�String")
; ;'("showSigned"      �. "(Real�a)�=>�(a�->�ShowS)�->�Int�->�a�->�ShowS")
; ;'("readSigned"      �. "(Real�a)�=>�ReadS�a�->�ReadS�a")
; ;'("showInt"         �. "(Integral�a)�=>�a�->�ShowS")
; ;'("readInt"�        �. "(Integral�a)�=>�a�->�(Char�->�Bool)�->�(Char�->�Int)�->�ReadS�a")
; '("readDec"�          . "(Integral�a)�=>�ReadS�a")
; '("readOct"�          . "(Integral�a)�=>�ReadS�a")
; '("readHex"�          . "(Integral�a)�=>�ReadS�a")
; ; IO fcts
; '("fail"������������� . "IOError�->�IO�a")
; '("userError"�������� . "String�->�IOError")
; '("catch"������������ . "IO�a�->�(IOError�->�IO�a)�->�IO�a�")
; '("putChar"���������� . "Char�->�IO�()")
; '("putStr"����������� . "String�->�IO�()")
; '("putStrLn"��������� . "String�->�IO�()")
; '("print"������������ . "Show�a�=>�a�->�rIO�()")
; '("getChar"���������� . "IO�Char")
; '("getLine"���������� . "IO�String")
; '("getContents"������ . "IO�String")
; '("interact"��������� . "(String�->�String)�->�IO�()")
; '("readFile"��������� . "FilePath�->�IO�String")
; '("writeFile"�������� . "FilePath�->�String�->�IO�()")
; '("appendFile"������� . "FilePath�->�String�->�IO�()")
; '("readIO"����������� . "Read�a�=>�String�->�IO�a")
; '("readLn"����������� . "Read�a�=>�IO�a")
 ;; ---------------------------------------------------------------------------
 ;; taken from Prelude Index of the Haskell 1.4 report
 '("!!"�������������.�"[a]�->�Int�->�a�")
 '("$"��������������.�"(a�->�b)�->�a�->�b�")
 '("&&"�������������.�"Bool�->�Bool�->�Bool�")
 '("||"�������������.�"Bool�->�Bool�->�Bool�")
 '("*"��������������.�"Num�a�=>�a�->�a�->�a�")
 '("**"�������������.�"Floating�a�=>�a�->�a�->�a�")
 '("+"��������������.�"Num�a�=>�a�->�a�->�a")
 '("++"�������������.�"MonadPlus�m�=>�m�a�->�m�a�->�m�a�")
 '("-"��������������.�"Num�a�=>�a�->�a�->�a�")
 '("."��������������.�"(b�->�c)�->�(a�->�b)�->�a�->�c�")
 '("/"��������������.�"Fractional�a�=>�a�->�a�->�a�")
 '("/="�������������.�"Eq�a�=>�a�->�a�->�Bool�")
 '("<"��������������.�"Ord�a�=>�a�->�a�->�Bool�")
 '("<="�������������.�"Ord�a�=>�a�->�a�->�Bool�")
 '("=="�������������.�"Eq�a�=>�a�->�a�->�Bool�")
 '(">"��������������.�"Ord�a�=>�a�->�a�->�Bool�")
 '(">="�������������.�"Ord�a�=>�a�->�a�->�Bool�")
 '(">>"�������������.�"m�a�->�m�b�->�m�b�")
 '(">>="������������.�"Monad�m�=>�m�a�->�(a�->�m�b)�->�m�b�")
 '("^"��������������.�"(Num�a,�Integral�b)�=>�a�->�b�->�a�")
 '("^^"�������������.�"(Fractional�a,�Integral�b)�=>�a�->�b�->�a�")
 '("abs"��������������. "Num�a�=>�a�->�a�")
 '("accumulate"�������.�"Monad�m�=>�[m�a]�->�m�[a]�")
 '("acos"�������������.�"Floating�a�=>�a�->�a�")
 '("acosh"������������.�"Floating�a�=>�a�->�a�")
 '("all"��������������.�"(a�->�Bool)�->�[a]�->�Bool�")
 '("and"��������������.�"[Bool]�->�Bool�")
 '("any"��������������.�"(a�->�Bool)�->�[a]�->�Bool�")
 '("appendFile"�������.�"FilePath�->�String�->�IO�()")
 '("applyM"�����������.�"Monad�m�=>�(a�->�m�b)�->�m�a�->�m�b")
 '("asTypeOf"���������.�"a�->�a�->�a�")
 '("asin"�������������.�"Floating�a�=>�a�->�a�")
 '("asinh"������������.�"Floating�a�=>�a�->�a�")
 '("atan"�������������.�"Floating�a�=>�a�->�a�")
 '("atan2"������������.�"RealFrac�a�=>�a�->�a�")
 '("atanh"������������.�"Floating�a�=>�a�->�a�")
 '("break"������������.�"(a�->�Bool)�->�[a]�->�([a],�[a])�")
 '("catch"������������.�"IO�a�->�(IOError�->�IO�a)�->�IO�a�")
 '("ceiling"����������.�"(RealFrac�a,�Integral�b)�=>�a�->�b�")
 '("compare"����������.�"Ord�a�=>�a�->�a�->�Ordering�")
 '("concat"�����������.�"MonadPlus�m�=>�[m�a]�->�m�a�")
 '("concatMap"��������.�"(a�->�[b])�->�[a]�->�[b]")
 '("const"������������.�"a�->�b�->�a")
 '("cos"��������������.�"Floating�a�=>�a�->�a�")
 '("cosh"�������������.�"Floating�a�=>�a�->�a�")
 '("curry"������������.�"((a,�b)�->�c)�->�a�->�b�->�c")
 '("cycle"������������.�"[a]�->�[a]�")
 '("decodeFloat"������.�"RealFloat�a�=>�a�->�(Integer,�Int)�")
 '("div"��������������.�"Integral�a�=>�a�->�a�->�a�")
 '("divMod"�����������.�"Integral�a�=>�a�->�a�->�(a,�a)�")
 '("drop"�������������.�"Int�->�[a]�->�[a]�")
 '("dropWhile"��������.�"(a�->�Bool)�->�[a]�->�[a]�")
 '("elem"�������������.�"Eq�a�=>�a�->�[a]�->�Bool�")
 '("encodeFloat"������.�"RealFloat�a�=>�Integer�->�Int�->�a�")
 '("enumFrom"���������.�"Enum�a�=>�a�->�[a]�")
 '("enumFromThen"�����.�"Enum�a�=>�a�->�a�->�[a]�")
 '("enumFromThenTo"���.�"Enum�a�=>�a�->�a�->�a�->�[a]�")
 '("enumFromTo"�������.�"Enum�a�=>�a�->�a�->�[a]�")
 '("error"������������.�"String�->�a�")
 '("even"�������������.�"Integral�a�=>�a�->�Bool")
 '("exp"��������������.�"Floating�a�=>�a�->�a�")
 '("exponent"���������.�"RealFloat�a�=>�a�->�Int�")
 '("fail"�������������.�"IOError�->�IO�a�")
 '("filter"�����������.�"MonadZero�m�=>�(a�->�Bool)�->�m�a�->�m�a�")
 '("flip"�������������.�"(a�->�b�->�c)�->�(b�->�a�->�c)")
 '("floatDigits"������.�"RealFloat�a�=>�a�->�Int�")
 '("floatRadix"�������.�"RealFloat�a�=>�a�->�Integer�")
 '("floatRange"�������.�"RealFloat�a�=>�a�->�(Int,�Int)�")
 '("floor"������������.�"(RealFrac�a,�Integral�b)�=>�a�->�b�")
 '("foldl"������������.�"(a�->�b�->�a)�->�a�->�[b]�->�a�")
 '("foldl1"�����������.�"(a�->�a�->�a)�->�[a]�->�a�")
 '("foldr"������������.�"(a�->�b�->�b)�->�b�->�[a]�->�b�")
 '("foldr1"�����������.�"(a�->�a�->�a)�->�[a]�->�a�")
 '("fromEnum"���������.�"Enum�a�=>�a�->�Int�")
 '("fromInteger"������.�"Num�a�=>�Integer�->�a�")
 '("fromIntegral"�����.�"(Integral�a,�Num�b)�=>�a�->�b")
 '("fromRational"�����.�"Fractional�a�=>�Rational�->�a�")
 '("fromRealFrac"�����.�"(RealFrac�a,�Fractional�b)�=>�a�->�b")
 '("fst"��������������.�"(a,�b)�->�a")
 '("gcd"��������������.�"(Integral�a)�=>�a�->�a�->�a")
 '("getChar"����������.�"IO�Char�")
 '("getContents"������.�"IO�String")
 '("getLine"����������.�"IO�Char�")
 '("guard"������������.�"MonadZero�m�=>�Bool�->�m�()")
 '("head"�������������.�"[a]�->�a")
 '("id"���������������.�"a�->�a")
 '("init"�������������.�"[a]�->�[a]")
 '("interact"���������.�"(String�->�String)�->�IO�()")
 '("isDenormalized"���.�"RealFloat�a�=>�a�->�Bool�")
 '("isIEEE"�����������.�"RealFloat�a�=>�a�->�Bool�")
 '("isInfinite"�������.�"RealFloat�a�=>�a�->�Bool�")
 '("isNaN"������������.�"RealFloat�a�=>�a�->�Bool�")
 '("isNegativeZero"���.�"RealFloat�a�=>�a�->�Bool�")
 '("iterate"����������.�"(a�->�a)�->�a�->�[a]�")
 '("last"�������������.�"[a]�->�a�")
 '("lcm"��������������.�"Integral�a�=>�a�->�a�->�a")
 '("length"�����������.�"[a]�->�Int")
 '("lex"��������������.�"ReadS�String�")
 '("lines"������������.�"String�->�[String]")
 '("log"��������������.�"Floating�a�=>�a�->�a�")
 '("logBase"����������.�"Floating�a�=>�a�->�a�->�a�")
 '("lookup"�����������.�"Eq�a�=>�a�->�[(a,�b)]�->�Maybe�b")
 '("map"��������������.�"Functor�f�=>�(a�->�b)�->�f�a�->�f�b�")
 '("mapM"�������������.�"Monad�m�=>�(a�->�m�b)�->�[a]�->�m�[b]")
 '("mapM_"������������.�"Monad�m�=>�(a�->�m�b)�->�[a]�->�m�()")
 '("max"��������������.�"Ord�a�=>�a�->�a�->�a�")
 '("maxBound"���������.�"Bounded�a�=>�a�")
 '("maximum"����������.�"Ord�a�=>�[a]�->�a")
 '("maybe"������������.�"b�->�(a�->�b)�->�Maybe�a�->�b�")
 '("min"��������������.�"Ord�a�=>�a�->�a�->�a�")
 '("minBound"���������.�"Bounded�a�=>�a�")
 '("minimum"����������.�"Ord�a�=>�[a]�->�a")
 '("mod"��������������.�"Integral�a�=>�a�->�a�->�a�")
 '("negate"�����������.�"Num�a�=>�a�->�a�")
 '("not"��������������.�"Bool�->�Bool")
 '("notElem"����������.�"Eq�a�=>�a�->�[a]�->�Bool")
 '("null"�������������.�"[a]�->�Bool")
 '("odd"��������������.�"Integral�a�=>�a�->�Bool")
 '("or"���������������.�"[Bool]�->�Bool")
 '("otherwise"��������.�"Bool")
 '("pi"���������������.�"Floating�a�=>�a�")
 '("pred"�������������.�"Enum�a�=>�a�->�a�")
 '("print"������������.�"Show�a�=>�IO�()�")
 '("product"����������.�"Num�a�=>�[a]�->�a")
 '("properFraction"���.�"(RealFrac�a,�Integral�b)�=>�a�->�(b,�a)�")
 '("putChar"����������.�"Char�->�IO�()")
 '("putStr"�����������.�"String�->�IO�()")
 '("putStrLn"���������.�"String�->�IO�()�")
 '("quot"�������������.�"Integral�a�=>�a�->�a�->�a�")
 '("quotRem"����������.�"Integral�a�=>�a�->�a�->�(a,�a)�")
 '("read"�������������.�"Read�a�=>�String�->�a")
 '("readFile"���������.�"FilePath�->�IO�String")
 '("readIO"�����������.�"Read�a�=>�String�->�IO�a�")
 '("readList"���������.�"Read�a�=>�ReadS�[a]")
 '("readLn"�����������.�"Read�a�=>�IO�a")
 '("readParen"��������.�"Bool�->�ReadS�a�->�ReadS�a")
 '("reads"������������.�"Read�a�=>�ReadS�a�")
 '("readsPrec"��������.�"Read�a�=>�Int�->�ReadS�a")
 '("recip"������������.�"Fractional�a�=>�a�->�a�")
 '("rem"��������������.�"Integral�a�=>�a�->�a�->�a�")
 '("repeat"�����������.�"a�->�[a]�")
 '("replicate"��������.�"Int�->�a�->�[a]�")
 '("return"�����������.�"Monad�m�=>�a�->�m�a�")
 '("reverse"����������.�"[a]�->�[a]�")
 '("round"������������.�"(RealFrac�a,�Integral�b)�=>�a�->�b�")
 '("scaleFloat"�������.�"RealFloat�a�=>�Int�->�a�->�a�")
 '("scanl"������������.�"(a�->�b�->�a)�->�a�->�[b]�->�[a]�")
 '("scanl1"�����������.�"(a�->�a�->�a)�->�[a]�->�[a]�")
 '("scanr"������������.�"(a�->�b�->�b)�->�b�->�[a]�->�[b]�")
 '("scanr1"�����������.�"(a�->�a�->�a)�->�[a]�->�[a]�")
 '("seq"��������������.�"Eval�a�=>�a�->�a�->�b�")
 '("sequence"���������.�"Monad�m�=>�[m�a]�->�m�()�")
 '("show"�������������.�"Show�a�=>�a�->�String�")
 '("showChar"���������.�"Char�->�ShowS")
 '("showList"���������.�"Show�a�=>�[a]�->�ShowS")
 '("showParen"��������.�"Bool�->�ShowS�->�ShowS")
 '("showString"�������.�"String�->�ShowS")
 '("shows"������������.�"Show�a�=>�a�->�ShowS�")
 '("showsPrec"��������.�"Show�a�=>�Int�->�a�->�ShowS�")
 '("significand"������.�"RealFloat�a�=>�a�->�a�")
 '("signum"�����������.�"Num�a�=>�a�->�a�")
 '("sin"��������������.�"Floating�a�=>�a�->�a�")
 '("sinh"�������������.�"Floating�a�=>�a�->�a�")
 '("snd"��������������.�"(a,�b)�->�b")
 '("span"�������������.�"(a�->�Bool)�->�[a]�->�([a],�[a])�")
 '("splitAt"����������.�"Int�->�[a]�->�([a],�[a])�")
 '("sqrt"�������������.�"Floating�a�=>�a�->�a�")
 '("strict"�����������.�"Eval�a�=>�(a�->�b)�->�(a�->�b)�")
 '("subtract"���������.�"Num�a�=>�a�->�a�->�a")
 '("succ"�������������.�"Enum�a�=>�a�->�a�")
 '("sum"��������������.�"Num�a�=>�[a]�->�a�")
 '("tail"�������������.�"[a]�->�[a]�")
 '("take"�������������.�"Int�->�[a]�->�[a]�")
 '("takeWhile"��������.�"(a�->�Bool)�->�[a]�->�[a]�")
 '("tan"��������������.�"Floating�a�=>�a�->�a�")
 '("tanh"�������������.�"Floating�a�=>�a�->�a�")
 '("toEnum"�����������.�"Enum�a�=>�Int�->�a�")
 '("toInteger"��������.�"Integral�a�=>�a�->�Integer�")
 '("toRational"�������.�"Real�a�=>�a�->�Rational�")
 '("truncate"���������.�"(RealFrac�a,�Integral�b)�=>�a�->�b�")
 '("uncurry"����������.�"(a�->�b�->�c)�->�((a,�b)�->�c)")
 '("undefined"��������.�"a�")
 '("unlines"����������.�"[String]�->�String")
 '("until"������������.�"(a�->�Bool)�->�(a�->�a)�->�a�->�a�")
 '("unwords"����������.�"[String]�->�String")
 '("unzip"������������.�"[(a,�b)]�->�([a],�[b])�")
 '("unzip3"�����������.�"[(a,�b,�c)]�->�([a],�[b],�[c])")
 '("userError"��������.�"String��->�IOError")
 '("words"������������.�"String�->�[String]�")
 '("writeFile"��������.�"FilePath�->�String�->�IO�()")
 '("zero"�������������.�"MonadZero�m�=>�m�a�")
 '("zip"��������������.�"[a]�->�[b]�->�[(a,�b)]�")
 '("zip3"�������������.�"[a]�->�[b]�->�[c]�->�[(a,�b,�c)]")
 '("zipWith"����������.�"(a�->�b�->�c)�->�[a]�->�[b]�->�[c]�")
 '("zipWith3"���������.�"(a�->�b�->�c�->�d)�->�[a]�->�[b]�->�[c]�->�[d]")
 ;; ---------------------------------------------------------------------------
 ;; The following functions are from the 1.4 Library Report (headers/ dir)
 ;; headers/Ratio.hs
 '("numerator"               . "(Integral a) => Ratio a -> a")
 '("denominator"             . "(Integral a) => Ratio a -> a")
 '("approxRational"          . "(RealFrac a) => a -> a -> Rational")
 ;; headers/Complex.hs
 '("realPart" . "(RealFloat a) => Complex a -> a")
 '("imagPart" . "(RealFloat a) => Complex a -> a")
 '("conjugate"	 . "(RealFloat a) => Complex a -> Complex a")
 '("mkPolar"		 . "(RealFloat a) => a -> a -> Complex a")
 '("cis"		 . "(RealFloat a) => a -> Complex a")
 '("polar"		 . "(RealFloat a) => Complex a -> (a,a)")
 '("magnitude" . "(RealFloat a) => Complex a -> a")
 '("phase" . "(RealFloat a) => Complex a -> a")
 ;; headers/Numeric.hs
 '("fromRat" . "(RealFloat a) => Rational -> a")
 '("fromRat'" . "(RealFloat a) => Rational -> a")
 '("scaleRat" . "Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)")
 '("minExpt" . "Int")
 '("maxExpt" . "Int")
 '("expt" . "Integer -> Int -> Integer")
 '("expts" . "Array Int Integer")
 '("integerLogBase" . "Integer -> Integer -> Int")
 '("showSigned"    . "Real a => (a -> ShowS) -> Int -> a -> ShowS")
 '("showInt"    . "Integral a => a -> ShowS")
 '("readSigned" . "(Real a) => ReadS a -> ReadS a")
 '("readInt" . "(Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a")
 '("readDec" . "(Integral a) => ReadS a")
 '("readOct" . "(Integral a) => ReadS a")
 '("readHex" . "(Integral a) => ReadS a")
 '("showEFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showFFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showGFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showFloat"      . "(RealFloat a) => a -> ShowS")
 '("formatRealFloat" . "(RealFloat a) => FFFormat -> Maybe Int -> a -> String")
 '("roundTo" . "Int -> Int -> [Int] -> (Int, [Int])")
 '("floatToDigits" . "(RealFloat a) => Integer -> a -> ([Int], Int)")
 '("readFloat"     . "(RealFloat a) => ReadS a")
 '("lexDigits"        . "ReadS String ")
 '("nonnull"          . "(Char -> Bool) -> ReadS String")
 ;; headers/Ix.hs
 '("rangeSize" . "Ix a => (a,a) -> Int")
 ;; headers/Array.hs
 '("array"           . "(Ix a) => (a,a) -> [(a,b)] -> Array a b")
 '("listArray"       . "(Ix a) => (a,a) -> [b] -> Array a b")
 ; '("(!)             :: "(Ix a) => Array a b -> a -> b")
 '("bounds"          . "(Ix a) => Array a b -> (a,a)")
 '("indices"         . "(Ix a) => Array a b -> [a]")
 '("elems"           . "(Ix a) => Array a b -> [b]")
 '("assocs"          . "(Ix a) => Array a b -> [(a,b)]")
 '("accumArray"      . "(Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b")
 ; (//)            :: (Ix a) => Array a b -> [(a,b)] -> Array a b
 '("accum"           . "(Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b")
 '("ixmap"           . "(Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c")
 ;; headers/List.hs (omitted; see 1.2 List module above)
 ;; headers/Maybe.hs
 '("isJust"                 . "Maybe a -> Bool")
 '("fromJust"               . "Maybe a -> a")
 '("fromMaybe"              . "a -> Maybe a -> a")
 '("maybeToList"            . "Maybe a -> [a]")
 '("listToMaybe"            . "[a] -> Maybe a")
 '("catMaybes"              . "[Maybe a] -> [a]")
 '("mapMaybe"               . "(a -> Maybe b) -> [a] -> [b]")
 '("unfoldr"                . "([a] -> Maybe ([a], a)) -> [a] -> ([a],[a])")
 ;; headers/Char.hs
 '("isAscii" . "Char -> Bool")
 '("isControl" . "Char -> Bool")
 '("isPrint" . "Char -> Bool")
 '("isSpace" . "Char -> Bool")
 '("isUpper" . "Char -> Bool")
 '("isLower" . "Char -> Bool")
 '("isAlpha" . "Char -> Bool")
 '("isDigit" . "Char -> Bool")
 '("isOctDigit" . "Char -> Bool")
 '("isHexDigit" . "Char -> Bool")
 '("isAlphanum" . "Char -> Bool")
 '("digitToInt" . "Char -> Int")
 '("intToDigit" . "Int -> Char")
 '("toUpper"                  . "Char -> Char")
 '("toLower"                  . "Char -> Char")
 '("ord"                     . "Char -> Int")
 '("chr"                     . "Int  -> Char")
 '("readLitChar"             . "ReadS Char")
 '("sshowLitChar"               . "Char -> ShowS")
 '("lexLitChar"          . "ReadS String")
 ;; headers/Monad.hs
 '("unless"           . "(Monad m) => Bool -> m () -> m ()")
 '("ap"               . "(Monad m) => m (a -> b) -> m a -> m b")
 '("liftM"            . "(Monad m) => (a -> b) -> (m a -> m b)")
 '("liftM2"           . "(Monad m) => (a -> b -> c) -> (m a -> m b -> m c)")
 '("liftM3"           . "(Monad m) => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)")
 '("liftM4"           . "(Monad m) => (a -> b -> c -> d -> e) -> (m a -> m b -> m c -> m d -> m e)")
 '("liftM5"           . "(Monad m) => (a -> b -> c -> d -> e -> f) -> (m a -> m b -> m c -> m d -> m e -> m f)")
 ;; headers/IO.hs
; '("try"            . "IO a -> IO (Either IOError a)")
; '("bracket"        . "IO a -> (a -> IO b) -> (a -> IO c) -> IO c")
; '("bracket_"        . "IO a -> (a -> IO b) -> IO c -> IO c")
 ;; Directory
; '("createDirectory"� .�"FilePath�->�IO�()")
; '("removeDirectory"� .�"FilePath�->�IO�()")
; '("removeFile"� .�"FilePath�->�IO�()")
; '("renameDirectory"� .�"FilePath�->�FilePath�->�IO�()")
; '("renameFile"� .�"FilePath�->�FilePath�->�IO�()")
; '("getDirectoryContents"� .�"FilePath�->�IO�[FilePath]")
; '("getCurrentDirectory"� .�"IO�FilePath")
; '("setCurrentDirectory"� .�"FilePath�->�IO�()")
; '("doesFileExist" .�"FilePath�->�IO�Bool")
; '("doesDirectoryExist" .�"FilePath�->�IO�Bool")
; '("getPermissions" .�"FilePath�->�IO�Permissions")
; '("setPermissions" .�"FilePath�->�Permissions�->�IO�()")
; '("getModificationTime" .�"FilePath�->�IO�ClockTime")
 ;; System
; '("getArgs"� .�"IO�[String]")
; '("getProgName"� .�"IO�String")
; '("getEnv"�������� .�"String�->�IO�String")
; '("system"�������� .�"String�->�IO�ExitCode")
; '("exitWith"��� .�"ExitCode�->�IO�a")
 ;; headers/Time.hs
 '("getClockTime"            . "IO ClockTime")
 '("addToClockTime"          . "TimeDiff     -> ClockTime -> ClockTime")
 '("diffClockTimes"          . "ClockTime    -> ClockTime -> TimeDiff")
 '("toCalendarTime"          . "ClockTime    -> IO CalendarTime")
 '("toUTCTime"               . "ClockTime    -> CalendarTime")
 '("toClockTime"             . "CalendarTime -> ClockTime")
 '("calendarTimeToString"    . "CalendarTime -> String")
 '("formatCalendarTime" . "TimeLocale -> String -> CalendarTime -> String")
 '("show2" . "Int -> String")
 '("show2'" . "Int -> String")
 '("show3" . "Int -> String")
 ;; headers/Locale.hs
 '("defaultTimeLocale" . "TimeLocale ")
 ;; headers/Random.hs
 '("random"    . "(Integer,Integer) -> Integer -> [Integer]")
 '("randomIO"  . "(Integer,Integer) -> IO [Integer]")
 )
"alist of prelude functions and their types.")

;@cindex haskell-doc-strategy-ids

(defvar haskell-doc-strategy-ids
 (list
  '("par"  . "Done -> Done -> Done ; [infixr 0]")
  '("seq"  . "Done -> Done -> Done ; [infixr 1]")

  '("using"      . "a -> Strategy a -> a ; [infixl 0]")
  '("demanding"  . "a -> Done -> a ; [infixl 0]")
  '("sparking"   . "a -> Done -> a ; [infixl 0]")

  '(">||" . "Done -> Done -> Done ; [infixr 2]")
  '(">|" .  "Done -> Done -> Done ; [infixr 3]")
  '("$||" . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
  '("$|"  . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
  '(".|"  . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
  '(".||" . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
  '("-|"  . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")
  '("-||" . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")

  '("Done" . "type Done = ()")
  '("Strategy" . "type Strategy a = a -> Done")

  '("r0"    . "Strategy a")
  '("rwhnf" . "Eval a => Strategy a")
  '("rnf" . "Strategy a")
  '("NFData" . "class Eval a => NFData a where rnf :: Strategy a")
  '("NFDataIntegral" ."class (NFData a, Integral a) => NFDataIntegral a")
  '("NFDataOrd" . "class (NFData a, Ord a) => NFDataOrd a")

  '("markStrat" . "Int -> Strategy a -> Strategy a")

  '("seqPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
  '("parPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
  '("seqTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")
  '("parTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")

  '("parList"  . "Strategy a -> Strategy [a]")
  '("parListN"  . "(Integral b) => b -> Strategy a -> Strategy [a]")
  '("parListNth"  . "Int -> Strategy a -> Strategy [a]")
  '("parListChunk"  . "Int -> Strategy a -> Strategy [a]")
  '("parMap"  . "Strategy b -> (a -> b) -> [a] -> [b]")
  '("parFlatMap"  . "Strategy [b] -> (a -> [b]) -> [a] -> [b]")
  '("parZipWith"  . "Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]")
  '("seqList"  . "Strategy a -> Strategy [a]")
  '("seqListN"  . "(Integral a) => a -> Strategy b -> Strategy [b]")
  '("seqListNth"  . "Int -> Strategy b -> Strategy [b]")

  '("parBuffer"  . "Int -> Strategy a -> [a] -> [a]")

  '("seqArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")
  '("parArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")

  '("fstPairFstList"  . "(NFData a) => Strategy [(a,b)]")
  '("force"  . "(NFData a) => a -> a ")
  '("sforce"  . "(NFData a) => a -> b -> b")
  )
"alist of strategy functions and their types as defined in Strategies.lhs.")

(defvar haskell-doc-user-defined-ids nil
 "alist of functions and strings defined by the user.")

;@node Test membership,  , Prelude types, Constants and Variables
;@subsection Test membership

;@cindex haskell-doc-is-of
(defsubst haskell-doc-is-of (fn types)
  "Check whether FN is one of the functions in the alist TYPES and return the type."
  (assoc fn types) )

;@node Install as minor mode, Menubar Support, Constants and Variables, top
;@section Install as minor mode

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'haskell-doc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((haskell-doc-mode haskell-doc-minor-mode-string)))))

;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages are
;; recorded in a log.  Do not put haskell-doc messages in that log since
;; they are Legion.

;@cindex haskell-doc-message

(defmacro haskell-doc-message (&rest args)
  (if (fboundp 'display-message)
      ;; XEmacs 19.13 way of preventing log messages.
      ;(list 'display-message '(quote no-log) (apply 'list 'format args))
      ;; XEmacs 19.15 seems to be a bit different
      (list 'display-message '(quote message) (apply 'list 'format args))
    (list 'let (list (list 'message-log-max 'nil))
          (apply 'list 'message args))))

;(haskell-doc-message "hi there")
;(display-message 'no-log "bonzo")

;@node Menubar Support, Haskell Doc Mode, Install as minor mode, top
;@section Menubar Support

; a dummy definition needed for xemacs (I know, it's horrible :-(
(if (not (functionp 'define-key-after))
  (defun define-key-after (map seq con name)))

;@cindex haskell-doc-install-keymap

(defun haskell-doc-install-keymap ()
  "Install a menu for `haskell-doc' as a submenu of `Hugs'."
 (interactive)
 ; define a keymap `haskell-doc-keymap' for the derive menu
 (if nil ; (keymapp haskell-doc-keymap)
       nil
     (setq haskell-doc-keymap (make-sparse-keymap))
     (define-key haskell-doc-keymap [visit]
       '("Visit FTP home site" . haskell-doc-visit-home))
     (define-key haskell-doc-keymap [submit]
       '("Submit bug report" . haskell-doc-submit-bug-report))
     (define-key haskell-doc-keymap [dummy]
       '("---" . nil))
     (define-key haskell-doc-keymap [make-index]
       '("Make global fct index" . haskell-doc-make-global-fct-index))
     (define-key haskell-doc-keymap [global-types-on]
       '("Toggle display of global types" . haskell-doc-show-global-types))
     (define-key haskell-doc-keymap [strategy-on]
       '("Toggle display of strategy ids" . haskell-doc-show-strategy))
     (define-key haskell-doc-keymap [user-defined-on]
       '("Toggle display of user defined ids" . haskell-doc-show-user-defined))
     (define-key haskell-doc-keymap [prelude-on]
       '("Toggle display of prelude functions" . haskell-doc-show-prelude))
     (define-key haskell-doc-keymap [reserved-ids-on]
       '("Toggle display of reserved ids" . haskell-doc-show-reserved))
     (define-key haskell-doc-keymap [haskell-doc-on]
       '("Toggle haskell-doc mode" . haskell-doc-mode))
  )

 ; add the menu to the hugs menu as last entry
 (cond 
  ((eq major-mode 'hugs-mode)
   (let ((hugsmap (lookup-key hugs-mode-map [menu-bar Hugs])))
	 (if (and (not haskell-doc-xemacs-p) ; XEmacs has problems here
		  (not (lookup-key hugsmap [haskell-doc])))
	     (define-key-after hugsmap [haskell-doc] (cons "Haskell-doc" haskell-doc-keymap)
	       [Haskell-doc mode]))
     ; add shortcuts for these commands
     (define-key hugs-mode-map "\e/" 'haskell-doc-check-active) ; for testing 
     (define-key hugs-mode-map "\C-c\C-o" 'haskell-doc-mode) 
     (define-key hugs-mode-map [mouse-3] 'haskell-doc-ask-mouse-for-type)))
  ((eq major-mode 'haskell-mode)
   ; add shortcuts for these commands
   (define-key haskell-mode-map "\e/" 'haskell-doc-check-active) ; for testing 
   (define-key haskell-mode-map "\C-c\C-o" 'haskell-doc-mode) 
   (define-key haskell-mode-map [mouse-3] 'haskell-doc-ask-mouse-for-type)) ) )


;@node Haskell Doc Mode, Switch it on or off, Menubar Support, top
;@section Haskell Doc Mode

;@cindex haskell-doc-mode

;;;###autoload
(defun haskell-doc-mode (&optional prefix)
  "*If non-nil, then enable haskell-doc-mode (see variable docstring)."
  (interactive "P")

  ;; Make sure it's on the post-command-idle-hook if defined, otherwise put
  ;; it on post-command-hook.  The former first appeared in Emacs 19.30.
  (setq haskell-doc-mode
  	 (if prefix
  	     (or (listp prefix);; C-u alone
  		 (> (prefix-numeric-value prefix) 0))
  	   (not haskell-doc-mode)))

  (and haskell-doc-mode-hook
       haskell-doc-mode
       (run-hooks haskell-doc-mode-hook))

  ;; ToDo: replace binding of `post-command-idle-hook' by `run-with-idle-timer'
  (and haskell-doc-mode
       (not (memq 'haskell-doc-mode-print-current-symbol-info 
		  (if (boundp 'post-command-idle-hook)
		     post-command-idle-hook
		   post-command-hook)))
       (add-hook (if (boundp 'post-command-idle-hook)
		     'post-command-idle-hook
		   'post-command-hook)
		 'haskell-doc-mode-print-current-symbol-info))

  (and (not haskell-doc-mode)
       (memq 'haskell-doc-mode-print-current-symbol-info 
	     (if (boundp 'post-command-idle-hook)
			post-command-idle-hook
		   post-command-hook))
       (remove-hook (if (boundp 'post-command-idle-hook)
			'post-command-idle-hook
		   'post-command-hook)
		 'haskell-doc-mode-print-current-symbol-info))

  (and haskell-doc-mode
       haskell-doc-show-global-types
       (progn
	 (setq haskell-doc-minor-mode-string " Haskell-DOC")
	 (haskell-doc-make-global-fct-index))  ; build type index for global fcts
       (setq haskell-doc-minor-mode-string " Haskell-Doc"))

  (if haskell-doc-mode
      (haskell-doc-install-keymap))

  (and (interactive-p)
       (if haskell-doc-mode
           (message "haskell-doc-mode is enabled")
         (message "haskell-doc-mode is disabled")))
  haskell-doc-mode)

;;@cindex haskell-doc-show-global-types

;;;;###autoload
;(defun haskell-doc-show-global-types (&optional prefix)
;  "*If non-nil, then enable display of global types in `haskell-doc-mode'."
;  (interactive "P")
;  ;; toggle mode or set it based on prefix value
;  (setq haskell-doc-show-global-types
;	(if prefix
;	    (>= (prefix-numeric-value prefix) 0)
;	  (not haskell-doc-show-global-types)))

;  (cond (haskell-doc-show-global-types
;	 ;; set mode string to reflect value of `haskell-doc-show-global-types'
;	 (setq haskell-doc-minor-mode-string " Haskell-DOC")
;	 ;; build index (note: this can be quite expensive)
;	 (haskell-doc-make-global-fct-index))
;	(t
;	 (setq haskell-doc-minor-mode-string " Haskell-Doc")) ) )


(defmacro haskell-doc-toggle-var (id prefix)
  ;; toggle variable or set it based on prefix value
  (setq id
	(if prefix
	    (>= (prefix-numeric-value prefix) 0)
	  (not id))) )

;@cindex haskell-doc-show-global-types
(defun haskell-doc-show-global-types (&optional prefix)
  "Turn on global types information in `haskell-doc-mode'."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-global-types prefix)
  (if haskell-doc-show-global-types
      (setq haskell-doc-minor-mode-string " Haskell-DOC")
    (setq haskell-doc-minor-mode-string " Haskell-Doc")) )

;@cindex haskell-doc-show-reserved
(defun haskell-doc-show-reserved (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-reserved prefix))

;@cindex haskell-doc-show-prelude
(defun haskell-doc-show-prelude (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-prelude prefix))

;@cindex haskell-doc-show-strategy
(defun haskell-doc-show-strategy (&optional prefix)
  "Toggle the automatic display of a doc string for strategy ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-strategy prefix))

;@cindex haskell-doc-show-user-defined
(defun haskell-doc-show-user-defined (&optional prefix)
  "Toggle the automatic display of a doc string for user defined ids."
  (interactive "P")
  (haskell-doc-toggle-var haskell-doc-show-user-defined prefix))

;@node Switch it on or off, Check, Haskell Doc Mode, top
;@section Switch it on or off

;@cindex turn-on-haskell-doc-mode

;;;###autoload
(defun turn-on-haskell-doc-mode ()
  "Unequivocally turn on haskell-doc-mode (see variable documentation)."
  (interactive)
  (haskell-doc-mode 1))

;@cindex  turn-off-haskell-doc-mode

;;;###autoload
(defun turn-off-haskell-doc-mode ()
  "Unequivocally turn off haskell-doc-mode (see variable documentation)."
  (interactive)
  (haskell-doc-mode 0))

;@node Check, Top level function, Switch it on or off, top
;@section Check

;@cindex haskell-doc-check-active

(defun haskell-doc-check-active ()
 "Check whether the print function is hooked in. 
Should be the same as the value of `haskell-doc-mode' but alas currently it 
is not."
 (interactive)
 (message 
  (if (memq 'haskell-doc-mode-print-current-symbol-info 
	    (if (boundp 'post-command-idle-hook)
		post-command-idle-hook
	      post-command-hook))
      "haskell-doc is ACTIVE"
    "haskell-doc is not ACTIVE \(Use C-u C-c C-o to turn it on\)")))

;@node Top level function, Mouse interface, Check, top
;@section Top level function

;@cindex haskell-doc-mode-print-current-symbol-info
;; This is the function hooked into the elisp command engine
(defun haskell-doc-mode-print-current-symbol-info ()
 "Print the type of the symbol under the cursor. 

This function is hooked into the `post-command-idle-hook' to print the type
automatically if `haskell-doc-mode' is turned on. It can also be called 
directly to ask for the type of a function."
  (interactive)
  (and haskell-doc-mode
       (not executing-kbd-macro)
       ;; Having this mode operate in the minibuffer makes it impossible to
       ;; see what you're doing.
       (not (eq (selected-window) (minibuffer-window)))
       ; take a nap
       (sit-for haskell-doc-idle-delay)
       ; good morning! read the word under the cursor for breakfast
       (let ((current-symbol (haskell-doc-get-current-word)) ); (haskell-doc-current-symbol)) )
             ; (current-fnsym  (haskell-doc-fnsym-in-current-sexp)))
	 (haskell-doc-show-type current-symbol)) ))

;	 ; ToDo: find surrounding fct
;         (cond ((eq current-symbol current-fnsym)
;                (haskell-doc-show-type current-fnsym))
;               (t
;                (or nil ; (haskell-doc-print-var-docstring current-symbol)
;                    (haskell-doc-show-type current-fnsym)))))))


;@node Mouse interface, Print fctsym, Top level function, top
;@section Mouse interface for interactive query

;@cindex haskell-doc-ask-mouse-for-type
(defun haskell-doc-ask-mouse-for-type (event)
 "Read the identifier under the mouse and echo its type.
This uses the same underlying function `haskell-doc-show-type' as the hooked
function. Only the user interface is different."
 (interactive "e")
 (save-excursion
   (select-window (posn-window (event-end event)))
   (goto-char (posn-point (event-end event)))
   (haskell-doc-show-type )))
 

;@node Print fctsym, Movement, Mouse interface, top
;@section Print fctsym

;@menu
;* Show type::			
;* Aux::			
;* Global fct type::		
;* Local fct type::		
;@end menu

;@node Show type, Aux, Print fctsym, Print fctsym
;@subsection Show type

;@cindex haskell-doc-show-type

;;;###autoload
(defun haskell-doc-show-type (&optional symbol)
  "Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `haskell-doc-prelude-types' alist of prelude functions and their types, or from the local functions in the current buffer."
  (interactive)
  (let* ((sym (or symbol (haskell-doc-get-current-word))) 
	; (haskell-doc-current-symbol))); (haskell-doc-fnsym-in-current-sexp)))
        (printit t)
        (i-am-prelude nil)
        (i-am-fct nil)
        (type nil)
	(is-reserved (haskell-doc-is-of sym haskell-doc-reserved-ids))
	(is-prelude  (haskell-doc-is-of sym haskell-doc-prelude-types))
	(is-strategy (haskell-doc-is-of sym haskell-doc-strategy-ids))
	(is-user-defined (haskell-doc-is-of sym haskell-doc-user-defined-ids))
	(is-prelude  (haskell-doc-is-of sym haskell-doc-prelude-types)))
   (cond
	  ;; if printed before do not print it again
          ((string= sym (car haskell-doc-last-data))
           (setq printit nil)
           (setq type (cdr haskell-doc-last-data)))
	  ;; if reserved id (i.e. Haskell keyword
	  ((and haskell-doc-show-reserved
	       is-reserved)
	   (setq type (cdr is-reserved))
           (setcdr haskell-doc-last-data type))
	  ;; if built-in function get type from docstring
          ((and haskell-doc-show-prelude
		is-prelude)
           (setq type (cdr is-prelude)) ; (cdr (assoc sym haskell-doc-prelude-types)))
	   (if (= 2 (length type)) ; horrible hack to remove bad formatting
	       (setq type (cadr type)))
	   (setq i-am-prelude t)
	   (setq i-am-fct t)
           (setcdr haskell-doc-last-data type))
	  ((and haskell-doc-show-strategy
	       is-strategy)
	   (setq i-am-fct t)
	   (setq type (cdr is-strategy))
           (setcdr haskell-doc-last-data type))
	  ((and haskell-doc-show-user-defined
	       is-user-defined)
	   ; (setq i-am-fct t)
	   (setq type (cdr is-user-defined))
           (setcdr haskell-doc-last-data type))
          (t
	   (let ( (x (haskell-doc-get-and-format-fct-type sym)) )
	     (if (null x)
		 (setcdr haskell-doc-last-data nil) ; if not found reset last data
	       (setq type (car x))
	       (setq i-am-fct (string= "Functions" (cdr x)))
	       (if (and haskell-doc-show-global-types (null type))
		   (setq type (haskell-doc-get-global-fct-type sym)))
	       (setcdr haskell-doc-last-data type)))) )
    ;; ToDo: encode i-am-fct info into alist of types
    (and type
         printit
	 ; drop `::' if it's not a fct
	 (let ( (str (cond (i-am-fct
			    (format "%s :: %s" sym type))
			   (t 
			    (format "%s" type)))) )
	   (if i-am-prelude
	       (add-text-properties 0 (1- (length str)) '(face bold) str))
	   (haskell-doc-message str)))) )


;; ToDo: define your own notion of `near' to find surrounding fct
;(defun haskell-doc-fnsym-in-current-sexp ()
;  (let* ((p (point))
;         (sym (progn
;		(forward-word -1)
;;                (while (and (forward-word -1) ; (haskell-doc-forward-sexp-safe -1)
;;                            (> (point) (point-min))))
;                (cond ((or (= (point) (point-min))
;                           (memq (or (char-after (point)) 0)
;                                 '(?\( ?\"))
;                           ;; If we hit a quotation mark before a paren, we
;                           ;; are inside a specific string, not a list of
;                           ;; symbols.
;                           (eq (or (char-after (1- (point))) 0) ?\"))
;                       nil)
;                      (t (condition-case nil
;                             (read (current-buffer))
;                           (error nil)))))))
;    (goto-char p)
;    (if sym
;	(format "%s" sym)
;      sym) ) )

;;    (and (symbolp sym)
;;         sym)))

;@node Aux, Global fct type, Show type, Print fctsym
;@subsection Aux

;; ToDo: handle multi-line types and bring them into a concise form

;@cindex haskell-doc-grab-line
(defun haskell-doc-grab-line (fct-and-pos)
 "Get the type of an \(FCT POSITION\) pair from the current buffer."
; (if (null fct-and-pos)
;     "" ; fn is not a local fct
   (goto-char (cdr fct-and-pos))
   (beginning-of-line)
   (buffer-substring-no-properties (point) (progn (end-of-line) (point))) )

;@cindex haskell-doc-get-imenu-info

(defun haskell-doc-get-imenu-info (obj kind)
  "Returns a string describing OBJ of KIND \(Functions, Types, Data\)."
  (cond ((eq major-mode 'hugs-mode)
	 (let* ( (imenu-info-alist (cdr (assoc kind imenu--index-alist)))
		 ; (names (mapcar (lambda (x) (car x)) imenu-info-alist))
		 (x (assoc obj imenu-info-alist))
	       )
	     (if x
		 (haskell-doc-grab-line x)
	       nil)) )
	  (t
	   ; (error "Cannot get local functions in %s mode, sorry" major-mode))) )
	   nil)))

;@node Global fct type, Local fct type, Aux, Print fctsym
;@subsection Global fct type

;; ToDo:
;;  - modular way of defining a mapping of module name to file
;;  - use a path to search for file (not just current directory)

;@cindex haskell-doc-imported-list

(defun haskell-doc-imported-list (outer-file)
  "Return a list of the imported modules in OUTER-FILE."
  (interactive "fName of outer `include' file: ") ;  (buffer-file-name))
  (let ((imported-file-list (list outer-file))
        start)
    (save-excursion
      (switch-to-buffer (find-file-noselect outer-file))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*import\\s-+" nil t)
        (skip-chars-forward " \t")
        (setq start (point))
        (end-of-line)
        (skip-chars-backward " \t")
	(let ( (file (concat (buffer-substring start (point)) ".hs")) )
	  (if (file-exists-p file)
	      (setq imported-file-list
		    (cons file imported-file-list))))
	(let ( (file (concat (buffer-substring start (point)) ".lhs")) )
	  (if (file-exists-p file)
	      (setq imported-file-list
		    (cons file imported-file-list))))
      )
      (nreverse imported-file-list)
      ;(message imported-file-list)
)))

;; ToDo: generalise this to "Types" etc (not just "Functions")

;@cindex haskell-doc-rescan-files

(defun haskell-doc-rescan-files (filelist)
 "Does an `imenu' rescan on every file in FILELIST and returns the fct-list.
This function switches to and potentially loads many buffers."
   (mapcar (lambda (f)
	     (switch-to-buffer (find-file-noselect f))
	     (imenu--make-index-alist)
	     (let ( (fn-alist (cdr (assoc "Functions" imenu--index-alist)) ) )
	       (cons f
		     (mapcar (lambda (x)
			       `(,(car x) . ,(haskell-doc-grab-line x)) )
			     fn-alist)) ) )
   filelist ) )

;@cindex haskell-doc-make-global-fct-index

(defun haskell-doc-make-global-fct-index ()
 "Scan imported files for types of global fcts and update `haskell-doc-index'."
 (interactive)
 (let* ( (this-buffer (current-buffer))
	 (this-file (buffer-file-name))
	 (x (haskell-doc-rescan-files (haskell-doc-imported-list this-file) )) )
   (switch-to-buffer this-buffer)
   ;; haskell-doc-index is buffer local => switch-buffer before setq
   (setq haskell-doc-index x) ) )

;; ToDo: use a separate munge-type function to format type concisely

;@cindex haskell-doc-get-global-fct-type

(defun haskell-doc-get-global-fct-type (&optional sym)
 "Get type for function symbol SYM by examining `haskell-doc-index'."
  (interactive) ;  "fName of outer `include' file: \nsFct:")
  (save-excursion
  ; (switch-to-buffer "*scratch*")
  ; (goto-char (point-max))
  ;; Produces a list of fct-type alists
;  (if (null sym)
;      (setq sym (progn (forward-word -1) (read (current-buffer)))))
  (or sym
      (current-word))
  (let* ( (fn sym) ; (format "%s" sym))
	  (fal haskell-doc-index)
	  (res "") )
    (while (not (null fal))
      (let* ( (l (car fal))
	      (f (car l))
	      (x (assoc fn (cdr l))) )
	(if (not (null x))
	    (let* ( (ty (cdr x)) ; the type as string
		    (idx (string-match "::" ty))
		    (str (if (null idx)
			     ty
			   (substring ty (+ idx 2)))) )
	      (setq res (format "[%s] %s" f str))))
	  (setq fal (cdr fal))))
    res))) ; (message res)) )

;@node Local fct type,  , Global fct type, Print fctsym
;@subsection Local fct type

;@cindex haskell-doc-get-and-format-fct-type

(defun haskell-doc-get-and-format-fct-type (fn)
 "Get the type and kind of FN by checking local and global functions."
 (save-excursion
   (save-match-data
     (let ((docstring "")
	   (doc nil)
	   )
       ; is it a local function?
       (setq docstring (haskell-doc-get-imenu-info fn "Functions"))
       (if (and (not (null docstring))
		(string-match (format "^%s\\s-+::\\s-+\\(.*\\)$" fn) docstring))
	   (setq doc `(,(match-string 1 docstring) . "Functions") ))
       ; is it a type declaration?
       (setq docstring (haskell-doc-get-imenu-info fn "Types"))
       (if (and (not (null docstring))
		(string-match (format "^\\s-*type\\s-+%s.*$" fn) docstring))
	     (setq doc `(,(match-string 0 docstring) . "Types")) )
       (if (and (not (null docstring))
		(string-match (format "^\\s-*data.*%s.*$" fn) docstring))
	 (setq doc `(,(match-string 0 docstring) . "Data")) )
       ; return the result
       doc ))))


;@node Movement, Bug Reports, Print fctsym, top
;@section Movement
; Functions for moving in text and extracting the current word under the cursor

; prbly nukable

;; forward-sexp calls scan-sexps, which returns an error if it hits the
;; beginning or end of the sexp.  This returns nil instead.
(defun haskell-doc-forward-sexp-safe (&optional count)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -COUNT means
move backward across COUNT balanced expressions.
Return distance in buffer moved, or nil."
  (or count (setq count 1))
  (condition-case err
      (- (- (point) (progn
                      (let ((parse-sexp-ignore-comments t))
                        (forward-sexp count))
                      (point))))
    (error nil)))

;; Do indirect function resolution if possible.
;(defun haskell-doc-symbol-function (fsym)
;  (let ((defn (and (fboundp fsym)
;                   (symbol-function fsym))))
;    (and (symbolp defn)
;         (condition-case err
;             (setq defn (indirect-function fsym))
;           (error (setq defn nil))))
;    defn))

;; HWL: currently unused; this is taken from eldoc

(defun haskell-doc-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (current-word))))

;; HWL: my attempt at more efficient (current-word)

;@cindex haskell-doc-is-id-char-at
(defsubst haskell-doc-is-id-char-at (x)
 (let ( (c (char-syntax (char-after x))) )
   (or (eq c ?w) (eq c ?_))) )

;; NB: this function is called from within the hooked print function;
;;     therefore this function must not fail, otherwise the function will
;;     be de-installed;
;;     if no word under the cursor return an empty string
;@cindex haskell-doc-get-current-word
(defun haskell-doc-get-current-word ()
 "Return the word under the cursor, or empty string if no word found."
 ; (interactive)
 (if (bobp)
     ""
 (let ((x (1- (point)))
       (beg)
       (end)
       )
 ; go back to first non-word char 
 (while (and (> x (point-min)) (haskell-doc-is-id-char-at x))  ; (not (bobp))
   (setq x (1- x)) )
 (if (= x (point-min))
     (setq beg x)
   (setq beg (1+ x)))
 (setq x (1+ x))
 (while (and (< x (point-max)) (haskell-doc-is-id-char-at x)) ; (not (eobp))
   (setq x (1+ x)) )
 (setq end x)
 (buffer-substring-no-properties beg end))))

;@node Bug Reports, Visit home site, Movement, top
;@section Bug Reports

;@cindex haskell-doc-submit-bug-report
; send a bug report
(defun haskell-doc-submit-bug-report ()
  "Send email to the maintainer of `haskell-doc'."
  (interactive)
  ;; In case we can't find reporter...
  (condition-case err
      (progn
        (require 'reporter)
	(and (y-or-n-p "Do you really want to submit a bug report? ")
        (reporter-submit-bug-report
	 haskell-doc-maintainer                               ; address
	 (concat "haskell-doc.el " haskell-doc-version)       ; package
	 haskell-doc-varlist                                  ; varlist
         nil nil                                        ; pre-/post-hooks
        "I have detected the following strange behaviour/bug in `haskell-doc':\n")))
    ;; ...fail gracefully.
    (error
     (beep)
     (message "Sorry, reporter.el not found."))))

;@node Visit home site, Index, Bug Reports, top
;@section Visit home site

;@cindex haskell-doc-visit-home

(defun haskell-doc-visit-home ()
 "Jump to the main FTP site for `haskell-doc'."
 (interactive)
 (if haskell-doc-xemacs-p
    (require 'efs)
  (require 'ange-ftp))
 (require 'dired)
 (dired-other-window haskell-doc-ftp-site))

;@appendix

;@node Index, Token, Visit home site, top
;@section Index

;@index
;* haskell-doc-ask-mouse-for-type::
;* haskell-doc-check-active::
;* haskell-doc-get-and-format-fct-type::
;* haskell-doc-get-current-word::
;* haskell-doc-get-global-fct-type::
;* haskell-doc-get-imenu-info::
;* haskell-doc-grab-line::
;* haskell-doc-imported-list::
;* haskell-doc-install-keymap::
;* haskell-doc-is-id-char-at::
;* haskell-doc-is-of::
;* haskell-doc-make-global-fct-index::
;* haskell-doc-message::
;* haskell-doc-mode::
;* haskell-doc-mode-print-current-symbol-info::
;* haskell-doc-prelude-types::
;* haskell-doc-rescan-files::
;* haskell-doc-reserved-ids::
;* haskell-doc-show-global-types::
;* haskell-doc-show-prelude::
;* haskell-doc-show-reserved::
;* haskell-doc-show-strategy::
;* haskell-doc-show-type::
;* haskell-doc-show-user-defined::
;* haskell-doc-strategy-ids::
;* haskell-doc-submit-bug-report::
;* haskell-doc-visit-home::
;* turn-off-haskell-doc-mode::
;* turn-on-haskell-doc-mode::
;@end index

;@node Token,  , Index, top
;@section Token

(provide 'haskell-doc)

;;; haskell-doc.el ends here
