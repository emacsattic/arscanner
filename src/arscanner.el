;; Copyright 2006-2007 Thierry Aime
;; Copyright 2002 Carsten Dominik

;; This is a scanner which explore a directory for extracting copyright 
;; and licence note from sources files. Result are displayed in an emacs 
;; buffer; an emacs mode help to broswe 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation, 
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA



(require 'outline)

;; Visibility cycling' cycles through the most important
;; states of an outline buffer. It is bound to the TAB key. 
;; This piece of code is provided by Carsten Dominik :
;; http://staff.science.uva.nl/~dominik/Tools/outline-magic.el


;;; Visibility cycling

(defcustom outline-cycle-emulate-tab nil
  "Where should `outline-cycle' emulate TAB.
nil    Never
white  Only in completely white lines
t      Everywhere except in headlines"
  :group 'outlines
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Everywhere except in headlines" t)
		 ))
		 
(defvar outline-promotion-headings nil
  "A sorted list of headings used for promotion/demotion commands.
Set this to a list of headings as they are matched by `outline-regexp',
top-level heading first.  If a mode or document needs several sets of 
outline headings (for example numbered and unnumbered sections), list
them set by set, separated by a nil element.  See the example for
`texinfo-mode' in the file commentary.") 
(make-variable-buffer-local 'outline-promotion-headings)

(defun outline-cycle (&optional arg)
  "Visibility cycling for outline(-minor)-mode.

- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
  (interactive "P")
  (setq deactivate-mark t)
  (cond

   ((equal arg '(4))
    ; Run `outline-cycle' as if at the top of the buffer.
    (save-excursion
      (goto-char (point-min))
      (outline-cycle nil)))

   (t
    (cond
     ((bobp) ;; Beginning of buffer: Global cycling

      (cond
       ((eq last-command 'outline-cycle-overview)
	;; We just created the overview - now do table of contents
	;; This can be slow in very large buffers, so indicate action
	(message "CONTENTS...") 
	(save-excursion
	  ;; Visit all headings and show their offspring
	  (goto-char (point-max))
	  (catch 'exit
	    (while (and (progn (condition-case nil
				   (outline-previous-visible-heading 1)
				 (error (goto-char (point-min))))
			       t)
			(looking-at outline-regexp))
	      (show-branches)
	      (if (bobp) (throw 'exit nil))))
	  (message "CONTENTS...done"))
	(setq this-command 'outline-cycle-toc))
       ((eq last-command 'outline-cycle-toc)
	;; We just showed the table of contents - now show everything
	(show-all)
	(message "SHOW ALL")
	(setq this-command 'outline-cycle-showall))
       (t
	;; Default action: go to overview
	(hide-sublevels 1)
	(message "OVERVIEW")
	(setq this-command 'outline-cycle-overview))))

     ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
      ;; At a heading: rotate between three different views
      (outline-back-to-heading)
      (let ((goal-column 0) beg eoh eol eos)
	;; First, some boundaries
	(save-excursion
	  (outline-back-to-heading)           (setq beg (point))
	  (save-excursion (outline-next-line) (setq eol (point)))
	  (outline-end-of-heading)            (setq eoh (point))
	  (outline-end-of-subtree)            (setq eos (point)))
	;; Find out what to do next and set `this-command'
	(cond
	 ((= eos eoh) 
	  ;; Nothing is hidden behind this heading
	  (message "EMPTY ENTRY"))
	 ((>= eol eos)
	  ;; Entire subtree is hidden in one line: open it
	  (show-entry)
	  (show-children)
	  (message "CHILDREN")
	  (setq this-command 'outline-cycle-children))
	 ((eq last-command 'outline-cycle-children)
	  ;; We just showed the children, now show everything.
	  (show-subtree)
	  (message "SUBTREE"))
	 (t 
	  ;; Default action: hide the subtree.
	  (hide-subtree)
	  (message "FOLDED")))))

     ;; TAB emulation
     ((outline-cycle-emulate-tab)
      (indent-relative))

     (t
      ;; Not at a headline: Do indent-relative
      (outline-back-to-heading))))))

(defun outline-cycle-emulate-tab ()
  "Check if TAB should be emulated at the current position."
  ;; This is called after the check for point in a headline,
  ;; so we can assume we are not in a headline
  (if (and (eq outline-cycle-emulate-tab 'white)
	   (save-excursion
	     (beginning-of-line 1) (looking-at "[ \t]+$")))
      t
    outline-cycle-emulate-tab))

(defun outline-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
	      (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))



(defconst scanner "arscanner ")

(defvar root-directory nil)

(defvar exclude-extension '("xml"))
(defvar include-extension '("txt"))


(defun file-source-p (file-name)
	"Returns true if file is recognized like a source file"
	(let ((file-extension (file-name-extension file-name)))
		(if (member file-extension exclude-extension) nil
			(if (member file-extension include-extension) t
				(let ((file-type (shell-command-to-string (concat "file -ib " file-name))))
					(if (< (length file-type) 4) nil
						(if (string-equal (substring file-type 0 4) "text") t
							nil)))))))


(defun tag-file (file-name)
	"Put the content of the yank buffer to the head of the file named file-name"
	(find-file file-name)
	(goto-line 2)
	(yank)
	(save-buffer 0)
	(kill-buffer nil))


(defun tag-files (directory)
	"Browses recursively directory, and for each source file, indicate the copyright owner and licence"
	(let 
			((contents (directory-files directory))
			 dirs)
		(while contents
			(let ((item (car contents)))
				;; The lower-case variants of RCS and CVS are for DOS/Windows.
				(unless (member item '("." ".." "RCS" "CVS" "rcs" "cvs" ".svn"))
					(let ((file-name (expand-file-name item directory)))
						(if (file-directory-p file-name) 
								(setq dirs (nconc dirs (list file-name)))
							(if (member (file-name-extension file-name) include-extension)
									(tag-file file-name)))))
				(setq contents (cdr contents))))
		(while dirs
			(tag-files (car dirs))
			(setq dirs (cdr dirs)))))



(defun scanne-directory (directory)
	"Browses recursively directory, and for each source file, indicate the copyright owner and licence"
	(terpri)
	(princ "Directory   >> ") 
	(princ (file-relative-name directory root-directory))
	(terpri)
	(let 
			((contents (directory-files directory))
			 dirs)
		(while contents
			(let ((item (car contents)))
				;; The lower-case variants of RCS and CVS are for DOS/Windows.
				(unless (member item '("." ".." "RCS" "CVS" "rcs" "cvs" ".svn"))
					(let ((file-name (expand-file-name item directory)))
						(if (file-directory-p file-name) 
								(setq dirs (nconc dirs (list file-name)))
							(let ((relative-file-name (file-relative-name file-name root-directory)))
								(if (file-source-p relative-file-name)
										(princ (shell-command-to-string (concat scanner relative-file-name)))
									(progn (princ "  ----      -> ")
												 (princ (file-relative-name file-name root-directory))
												 (terpri)))))))
				(setq contents (cdr contents))))
		(while dirs
			(scanne-directory (car dirs))
			(setq dirs (cdr dirs)))))


(defun arscanner (directory)
  "Displays in a special buffer named *arscanner* the list of files in directory 
with the copyright notice and the licence"
  
	(interactive 
	 (list (read-file-name "List directory (brief): "
												 nil default-directory nil)))
   
	(or directory (setq directory default-directory))
	(setq root-directory (expand-file-name directory))
	(with-output-to-temp-buffer "*arscanner*"
		(buffer-disable-undo standard-output)
		
		(princ "Arscanner ")
		(princ root-directory)
		(terpri)
		(save-excursion
			(set-buffer "*arscanner*")
			(arscanner-mode)
			(outline-minor-mode)
			(set-variable 'outline-regexp "Directory   >> \\(.*/\\)*")
			(setq default-directory
						(if (file-directory-p root-directory)
								(file-name-as-directory root-directory)
							(file-name-directory root-directory)))
			(scanne-directory root-directory)
			(toggle-read-only))))

(provide 'arscanner)


(defun ars-set-exclude-extension (ext-list)
	"Set the list of non source file extension"
	(interactive "xlist of non source file extension, ex. \(\"exe\" \"lib\"\): ")
	(set-variable 'exclude-extension ext-list))

(defun ars-set-include-extension (inc-list)
	"Set the list of source file extension"
	(interactive "xlist of source file extension, ex. \(\"c\" \"h\"\): ")
	(set-variable 'include-extension inc-list))

(defun ars-view-file ()
	"open in view mode the file named at the end of the line"
	(interactive)
	(let ((current-line (buffer-substring 
											 (save-excursion (move-to-column 12) (point))
											 (save-excursion (end-of-line) (point)))))
		(princ root-directory)
		(if (string-equal "-> " (substring current-line 0 3))
				(view-file (concat root-directory "/" (substring current-line 3))))))

(defun ars-edit-file ()
	"open in edit mode the file named at the end of the line"
	(interactive)
	(let ((current-line (buffer-substring 
											 (save-excursion (move-to-column 12) (point))
											 (save-excursion (end-of-line) (point)))))
		(princ root-directory)
		(if (string-equal "-> " (substring current-line 0 3))
				(find-file (concat root-directory "/" (substring current-line 3))))))

(defun ars-tag-files ()
	"Put the content of the yank buffer to the head of the files"
	(interactive)
	(let ((current-line (buffer-substring 
											 (save-excursion (move-to-column 12) (point))
											 (save-excursion (end-of-line) (point)))))
		(princ root-directory)
		(if (string-equal "-> " (substring current-line 0 3))
				(tag-file (concat root-directory "/" (substring current-line 3)))
			(if (string-equal ">> " (substring current-line 0 3))
					(tag-files (concat root-directory "/" (substring current-line 3)))))))




;; try to write a clean major mode for emacs
(defvar arscanner-mode-hook nil)

(defvar arscanner-mode-map
  (let ((arscanner-mode-map (make-keymap)))
    (define-key arscanner-mode-map [return] 'ars-view-file)
    (define-key arscanner-mode-map "v" 'ars-view-file)
    (define-key arscanner-mode-map "e" 'ars-edit-file)
    (define-key arscanner-mode-map "t" 'ars-tag-files)
		(define-key arscanner-mode-map [(tab)] 'outline-cycle)
    arscanner-mode-map)
  "Keymap for arscanner major mode")

;; it may be usefull if a arscanner buffer is save and reload
(add-to-list 'auto-mode-alist '("\\.ars\\'" . arscanner-mode))


(defvar arscanner-font-lock-keywords 
	(list
	 '("\\(Authors rights.*$\\)" .   font-lock-keyword-face)
	 '("\\(Directory.*$\\)" . font-lock-variable-name-face)
	 '("\\(UNKNOWN.*$\\)" . font-lock-builtin-face)
	 '("\\(----.*$\\)" . font-lock-comment-face)
  "Default highlighting expressions for arscanner mode"))


(defun arscanner-mode ()
  "Major mode for browsing authors rights and licence in source files"
  (interactive)
	(kill-all-local-variables)
  (use-local-map arscanner-mode-map)
	(set (make-local-variable 'font-lock-defaults) '(arscanner-font-lock-keywords))
  (setq major-mode 'arscanner-mode)
  (setq mode-name "arscanner")
	(run-hooks 'arscanner-mode-hook))

(provide 'arscanner-mode)


