;;; org-appear.el --- Auto-toggle Org fragments -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode fragment visibility upon entering and leaving
;; Homepage: https://github.com/awth13/org-appear

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package enabes automatic visibility toggling of various Org fragments depending on cursor position.
;; Automatic toggling of fragments may be enabled by setting `org-appear-autoemphasis'
;; and `org-appear-autosubmarkers' custom variables to non-nil.
;; By default, only `org-appear-autoemphasis' is enabled.
;; If Org mode custom variables that control visibility of emphasis markers, links,
;; or sub/superscripts are configured to show hidden parts,
;; the respective `org-appear' settings do not have an effect.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-appear nil
  "Auto-toggle Org fragments"
  :group 'org)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim fragments.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript markers.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

;; TODO: Link support is currently broken
;; (defcustom org-appear-autolinks nil
;;   "Non-nil enables automatic toggling of links.
;; Does not have an effect if `org-link-descriptive' is nil."
;;   :type 'boolean
;;   :group 'org-appear)

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles fragments in Org mode."
  nil nil nil

  (cond
   (org-appear-mode
    (org-appear--set-fragments)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t))
   (t
    ;; Clean up the current fragment when disabling the mode
    (let ((current-frag (org-appear--current-frag)))
      (when current-frag
	(org-appear--toggle-lock-and-flush current-frag)))
    (remove-hook 'post-command-hook #'org-appear--post-cmd t))))

(defvar org-appear-fragments nil
  "List of Org fragments to toggle.")

(defun org-appear--set-fragments ()
  "Add designated fragments to toggle to `org-appear-fragments'."
  (let ((emph-fragments '(bold
			  italic
			  underline
			  strike-through
			  verbatim
			  code))
	(subscript-fragments '(subscript
			       superscript)))

    ;; FIXME: There must be a better way to do this
    (setq-local org-appear--prev-frag nil)
    (setq org-appear-fragments nil)	; reset
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-fragments (append org-appear-fragments emph-fragments)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-fragments (append org-appear-fragments subscript-fragments)))))

(defvar-local org-appear--prev-frag nil
  "Previous fragment that surrounded the cursor, or nil if the cursor was not
on a fragment. This is used to track when the cursor leaves a fragment.")

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling fragments depending on whether the cursor entered or exited them."
  (let* ((prev-frag org-appear--prev-frag)
	 (prev-frag-start (org-element-property :begin prev-frag))
	 (current-frag (org-appear--current-frag))
	 (current-frag-start (org-element-property :begin current-frag)))

    ;; If the start position of current and previous fragments are not equal,
    ;; we're in a new fragment
    (when (not (equal prev-frag-start current-frag-start))

      ;; Unless we are still in an active fragment
      (unless (and prev-frag current-frag)
	(org-appear--toggle-lock-and-flush prev-frag))

      ;; We need to reevaluate `org-element-context' before disabling/enabling
      ;; since it is possible to modify the fragment while it is enabled
      (setq org-appear--prev-frag current-frag)
      (when prev-frag
	(save-excursion
	  (goto-char prev-frag-start)
	  (org-appear--hide-invisible (org-element-context))))
      (when current-frag
	(save-excursion
	  (goto-char current-frag-start)
	  (org-appear--show-invisible (org-element-context)))))))

(defun org-appear--current-frag ()
  "Return element list of fragment at point.
Return nil if element is not supported by `org-appear-mode'."
  (let ((elem (org-element-context)))
    (if (member (car elem) org-appear-fragments)
	elem
      nil)))

(defun org-appear--parse-elem (elem)
  "Return start and end positions and parent type of element ELEM."
  (let ((elem-start (org-element-property :begin elem))
	(elem-end (org-element-property :end elem))
	(elem-parent (org-element-property :parent elem)))
    (list 'start elem-start
	  'end elem-end
	  'parent (car elem-parent))))

(defun org-appear--nestedp (elem)
  "Return nil if ELEM is not inside another emphasised fragment."
  (member (plist-get elem 'parent) '(bold
				     italic
				     underline
				     strike-through
				     verbatim
				     code)))

(defun org-appear--toggle-lock-and-flush (frag)
  "Disable `jit-lock-mode' if it was enabled.
Enable it otherwise, flushing previous fragment FRAG."
  (if jit-lock-mode
      (setq-local jit-lock-mode nil)
    (setq-local jit-lock-mode t)
    ;; Flushing is necessary to make sure previous FRAG
    ;; is refontified if it was just destroyed
    (when frag
      (save-excursion
	(font-lock-fontify-region (max (org-element-property :begin frag) (point-min))
				  (min (org-element-property :end frag) (point-max)))))))

(defun org-appear--show-invisible (frag)
  "Silently remove invisible property from invisible elements inside fragment FRAG."
  (let ((elem-at-frag (org-appear--parse-elem frag)))
    (when elem-at-frag			    ; Exit immediately if not valid FRAG
      (let ((start (plist-get elem-at-frag 'start))
	    (end (plist-get elem-at-frag 'end))
	    (org-hide-emphasis-markers)
	    (org-pretty-entities)
	    (font-lock-extend-region-functions
	     (if (org-appear--nestedp elem-at-frag)
		 font-lock-extend-region-functions
	       nil)))
	;; HACK: (1- start) makes sure that font-lock is aware that
	;;       it is looking at an emphasised fragment
	(font-lock-fontify-region (1- start) end)))))

(defun org-appear--hide-invisible (frag)
  "Silently add invisible property to invisible elements inside fragment FRAG."
  (let ((elem-at-frag (org-appear--parse-elem frag)))
    (when elem-at-frag		  ; Exit immediately if not valid FRAG
      (let ((start (plist-get elem-at-frag 'start))
	    (end (plist-get elem-at-frag 'end)))
	(font-lock-fontify-region start end)))))

(provide 'org-appear)
;;; org-appear.el ends here
