;; Copyright 2006-2007 Thierry Aime

;; This is a scanner which explore a directory for extracting copyright 
;; and licence note from sources files. Result are displayed in an emacs 
;; buffer; an emacs mode help to broswe 

;; This software is governed by the CeCILL license under French law and
;; abiding by the rules of distribution of free software.  You can  use, 
;; modify and/ or redistribute the software under the terms of the CeCILL
;; license as circulated by CEA, CNRS and INRIA at the following URL
;; "http://www.cecill.info". 

;; As a counterpart to the access to the source code and  rights to copy,
;; modify and redistribute granted by the license, users are provided only
;; with a limited warranty  and the software's author,  the holder of the
;; economic rights,  and the successive licensors  have only  limited
;; liability. 

;; In this respect, the user's attention is drawn to the risks associated
;; with loading,  using,  modifying and/or developing or reproducing the
;; software by the user in light of its specific status of free software,
;; that may mean  that it is complicated to manipulate,  and  that  also
;; therefore means  that it is reserved for developers  and  experienced
;; professionals having in-depth computer knowledge. Users are therefore
;; encouraged to load and test the software's suitability as regards their
;; requirements in conditions enabling the security of their systems and/or 
;; data to be ensured and,  more generally, to use and operate it in the 
;; same conditions as regards security. 

;; The fact that you are presently reading this means that you have had
;; knowledge of the CeCILL license and that you accept its terms.


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


