;;; verbiste.el --- Emacs interface to verbiste
;;
;; $Id: verbiste.el 5 2007-02-06 13:18:51Z intrigeri $
;; $URL: https://intrigeri.boum.org/svn/pub/verbiste/verbiste.el $
;; Heavily inspired from dict.el, by Max Vasin.
;;

;; Copyright (c) 2002, 2003 Max Vasin
;; Copyright (c) 2006 by Ben Voui <intrigeri@boum.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Code:
(defgroup Verbiste nil
  "Interface to the verbiste French conjugation system."
  :prefix "verbiste-"
  :group 'external)

(defcustom verbiste-always-quote-terms nil
  "If t verbiste will always quote terms."
  :type 'boolean
  :group 'Verbiste)

;;;;
;;;; Key binding customisation variables and helper functions
;;;;

(defun verbiste-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun verbiste-set-enable-key-bindings (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun verbiste-mode-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defcustom verbiste-conjugate-key-binding "\\C-cvc"
  "Specifies a key binding to run french-conjugator and display
the results in the Emacs buffer."
  :type 'string
  :group 'Verbiste
  :set 'verbiste-set-key-binding
  :require 'verbiste)

(defcustom verbiste-deconjugate-key-binding "\\C-cvd"
  "Specifies a key binding to run french-deconjugator and display
the results in the Emacs buffer."
  :type 'string
  :group 'Verbiste
  :set 'verbiste-set-key-binding
  :require 'verbiste)

(defcustom verbiste-enable-key-bindings nil
  "Enables key bindings for verbiste.el commands."
  :type 'boolean
  :group 'Verbiste
  :set 'verbiste-set-enable-key-bindings
  :require 'verbiste)

(defgroup Verbiste-Mode nil
  "Verbiste-mode key bindings"
  :tag "Verbiste-mode"
  :prefix "verbiste-mode-"
  :group 'Verbiste)

(defcustom verbiste-mode-conjugate-key-binding "c"
  "Specifies a key binding to run french-conjugator and display
the results in the Emacs buffer."
  :tag "Conjugate"
  :type 'string
  :group 'Verbiste-Mode
  :set 'verbiste-mode-set-key-binding
  :require 'verbiste)

(defcustom verbiste-mode-deconjugate-key-binding "d"
  "Specifies a key binding to run french-deconjugator and display
the results in the Emacs buffer."
  :tag "Deconjugate"
  :type 'string
  :group 'Verbiste-Mode
  :set 'verbiste-mode-set-key-binding
  :require 'verbiste)

(defcustom verbiste-buffer-coding-system 'latin-1
  "Specifies coding system to use in verbiste buffer.
At least until Versite 0.1.10, only latin-1 is supported."
  :tag "Input coding system for Verbiste buffer"
  :type 'sexp
  :group 'Verbiste-Mode)

(defun verbiste-quote (word)
  "Quote WORD if necessary."
  (if verbiste-always-quote-terms
      (if (or (and (eq (aref word 0) ?\")
		   (eq (aref word (- (length word) 1)) ?\"))
	      (and (eq (aref word 0) ?\')
		   (eq (aref word (- (length word) 1)) ?\')))
	  word
	(concat "'" word "'"))
    word))

(defun verbiste-get-answer (what deconj)
  "Receive the conjugation of WHAT from the verbiste and insert
in the buffer.
If DECONJ is non-nil, deconjugate instead"
  (let* ((word what)
	 (buffer-name (concat "*VERBISTE " word "*"))
	 (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))
	 (coding-system-for-read verbiste-buffer-coding-system)
	 (coding-system-for-write verbiste-buffer-coding-system))
    (save-excursion
      (set-buffer buffer)
      (kill-region (point-min) (point-max))
      (verbiste-mode))
    (message "%s %s in the background"
	     (if deconj "Deconjugating" "Conjugating")
	     word)
    (set-process-sentinel
     (start-process "verbiste" buffer "sh" "-c"
		    (format "%s %s"
			    (if deconj "french-deconjugator" "french-conjugator -p")
			    (verbiste-quote word)))
     'verbiste-bgproc-sentinel)))

(defun verbiste-bgproc-sentinel (process msg)
  "Verbiste background PROCESS sentinel (handling MSG)."
  (let ((buffer (process-buffer process)))
    (cond
     ((string= "finished\n" msg)
      (save-excursion (set-buffer buffer)
		      (goto-char (point-min))
		      (display-buffer buffer)))
     ((string-match "exited abnormally with code" msg)
      (message msg)))))

(defsubst verbiste-default-verbiste-entry ()
  "Make a guess at a default verbiste entry.
This guess is based on the text surrounding the cursor."
  (let ((word (or (current-word)
                  "")))
    (if (string-match "[._]+$" word)
        (substring word 0 (match-beginning 0))
      word)))

;;;;
;;;; Lookup functions
;;;;

(defun verbiste-conjugate (verb)
  "Conjugate a VERB using Verbiste."
  (interactive (list (let* ((default-entry (verbiste-default-verbiste-entry))
	     (input (read-string
		     (format "Verb%s: "
			     (if (string= default-entry "")
				 ""
			       (format " (default %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No verbiste args given") default-entry) input))))
  (verbiste-get-answer verb nil))

(defun verbiste-deconjugate (word)
  "Deconjugate a WORD using Verbiste."
  (interactive (list (let* ((default-entry (verbiste-default-verbiste-entry))
	     (input (read-string
		     (format "Word%s: "
			     (if (string= default-entry "")
				 ""
			       (format " (default %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No verbiste args given") default-entry) input))))
  (verbiste-get-answer word t))

(defun verbiste-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE."
  (let ((result (set-default key value)))
    (set-default 'verbiste-enable-key-bindings t)
    (verbiste-update-key-bindings)
    result))

(defun verbiste-set-enable-key-bindings (key value)
  "Set KEY to VALUE and update verbiste key bindings."
  (let ((result (set-default key value)))
    (verbiste-update-key-bindings)
    result))

(defun verbiste-process-key-binding (string)
  "Process a STRING representing a key binding to allow easy key
  binding customisation."
  (read (concat "\"" string "\"")))

(defvar verbiste-mode-keymap (make-sparse-keymap))

(defun verbiste-mode-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE (for Verbiste-mode)."
  (let ((result (set-default key value)))
    (verbiste-mode-update-key-bindings)
    result))

(defun verbiste-mode()
  (interactive)
  (use-local-map verbiste-mode-keymap)
  (setq mode-name "Verbiste")
  (setq major-mode 'verbiste-mode))

(defun verbiste-update-key-bindings ()
  "Update verbiste key bindings."
  (when verbiste-enable-key-bindings
    ;; Setup global key binding `C-c v c' for running french-conjugator...
    (global-set-key (verbiste-process-key-binding
		     verbiste-conjugate-key-binding)
		    'verbiste-conjugate)
    ;; ... `C-c v d' for running french-deconjugator...
    (global-set-key (verbiste-process-key-binding
		     verbiste-deconjugate-key-binding)
		    'verbiste-deconjugate)))

(defun verbiste-mode-update-key-bindings ()
  "Update verbiste key bindings for Verbiste-mode."
  ;; Setup Verbiste-mode key binding `c' for running french-conjugator...
  (define-key verbiste-mode-keymap (verbiste-process-key-binding
				    verbiste-mode-conjugate-key-binding)
                                   'verbiste-conjugate)
  ;; ... `d' for running french-deconjugator...
  (define-key verbiste-mode-keymap (verbiste-process-key-binding
				    verbiste-mode-deconjugate-key-binding)
                                   'verbiste-deconjugate))

;;;;
;;;; Informational functions
;;;;

(defun verbiste-version ()
  "Display verbiste version information."
  (interactive)
  (shell-command "french-conjugator --version"))

(defconst verbiste-version
  "$Rev: 5 $"
  "Version number for 'verbiste' package.")

(defun verbiste-version-number ()
  "Return 'verbiste' version number."
  (string-match "[0123456789.]+" verbiste-version)
  (match-string 0 verbiste-version))

(defun verbiste-display-version ()
  "Display 'verbiste' version."
  (interactive)
  (message "Emacs Verbiste client version <%s>." (verbiste-version-number)))

(verbiste-update-key-bindings)
(verbiste-mode-update-key-bindings)

(provide 'verbiste)

;;; verbiste.el ends here

