;;; org-appear.el --- Auto-toggle Org elements -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-elemtog https://github.com/io12/org-elemtog
;; org-elemtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Description: Toggle Org mode element visibility upon entering and leaving
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

;; This package enabes automatic visibility toggling of Org elements depending on cursor position.
;; `org-appear-autoemphasis', `org-appear-autolinks' and `org-appear-autosubmarkers'
;; custom variables enable automatic toggling of respective elements when set to non-nil.
;; By default, only `org-appear-autoemphasis' is enabled. If Org mode custom variables
;; that control visibility of emphasis markers, links, or sub/superscripts are configured
;; to show hidden parts, the respective `org-appear' settings do not have an effect.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-appear nil
  "Auto-toggle Org elements"
  :group 'org)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim elements.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript elements.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolinks nil
  "Non-nil enables automatic toggling of links.
Does not have an effect if `org-link-descriptive' is nil."
  :type 'boolean
  :group 'org-appear)

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles visibility of elements in Org mode."
  nil nil nil

  (cond
   (org-appear-mode
    (org-appear--set-elements)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t))
   (t
    ;; Clean up the current element when disabling the mode
    (let ((current-elem (org-appear--current-elem)))
      (when current-elem
	(org-appear--toggle-lock-and-fontify current-elem)))
    (remove-hook 'post-command-hook #'org-appear--post-cmd t))))

(defvar org-appear-elements nil
  "List of Org elements to toggle.")

(defun org-appear--set-elements ()
  "Add designated elements to toggle to `org-appear-elements'."
  (let ((emphasis-elements '(bold
			     italic
			     underline
			     strike-through
			     verbatim
			     code))
	(script-elements '(subscript
			   superscript))
	(link-elements '(link)))

    ;; FIXME: There must be a better way to do this
    (setq-local org-appear--prev-elem nil)
    (setq org-appear-elements nil)	; reset
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-elements (append org-appear-elements emphasis-elements)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-elements (append org-appear-elements script-elements)))
    (when (and org-link-descriptive org-appear-autolinks)
      (setq org-appear-elements (append org-appear-elements link-elements)))))

(defvar-local org-appear--prev-elem nil
  "Previous element that surrounded the cursor.
Nil if the cursor was not inside an element.")

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling element depending on whether the cursor entered or exited them."
  (let* ((prev-elem org-appear--prev-elem)
	 (prev-elem-start (org-element-property :begin prev-elem))
	 (current-elem (org-appear--current-elem))
	 (current-elem-start (org-element-property :begin current-elem)))

    ;; If the start position of current and previous elements are not equal,
    ;; we're in a new element
    (when (not (equal prev-elem-start current-elem-start))

      ;; Unless we are still inside an active element
      (unless (and prev-elem current-elem)
	(org-appear--toggle-lock-and-fontify prev-elem))

      ;; We need to reevaluate `org-element-context' before disabling/enabling
      ;; since it is possible to modify an element while it is enabled
      (setq org-appear--prev-elem current-elem)
      (when prev-elem
	(save-excursion
	  (goto-char prev-elem-start)
	  (org-appear--hide-invisible (org-element-context))))
      (when current-elem
	(save-excursion
	  (goto-char current-elem-start)
	  (org-appear--show-invisible (org-element-context)))))))

(defun org-appear--current-elem ()
  "Return element list of element at point.
Return nil if element is not supported by `org-appear-mode'."
  (let ((elem (org-element-context)))
    (if (member (car elem) org-appear-elements)
	elem
      nil)))

(defun org-appear--nestedp (elem)
  "Return nil if ELEM is not inside another emphasised element."
  (member (car (org-element-property :parent elem)) '(bold
						italic
						underline
						strike-through
						verbatim
						code)))

(defun org-appear--toggle-lock-and-fontify (elem)
  "Disable `jit-lock-mode' if it was enabled.
Enable it otherwise, fontifying element ELEM."
  (if jit-lock-mode
      (setq-local jit-lock-mode nil)
    (setq-local jit-lock-mode t)
    ;; Forced fontification is necessary to make sure previous ELEM
    ;; is refontified if it was just destroyed
    (when elem
      (save-excursion
	(font-lock-fontify-region (org-element-property :begin elem)
				  (org-element-property :end elem))))))

(defun org-appear--show-invisible (elem)
  "Make invisible parts of element ELEM appear visible."
  (let ((link-type (org-element-property :type elem)))
    (if link-type
	(unless (string= link-type "cite")
	  (org-appear--expand-link elem))
      (let ((start (org-element-property :begin elem))
	    (end (org-element-property :end elem))
	    (org-hide-emphasis-markers)
	    (org-pretty-entities)
	    (font-lock-extend-region-functions
	     (if (org-appear--nestedp elem)
		 font-lock-extend-region-functions
	       nil)))
	;; HACK: (1- start) makes font-lock aware that
	;;       it is looking at an emphasised element
	(font-lock-fontify-region (1- start) end)))))

(defun org-appear--hide-invisible (elem)
  "Refontify element ELEM, hiding invisible parts."
  (let ((start (org-element-property :begin elem))
	(end (org-element-property :end elem)))
    (font-lock-fontify-region start end)))

(defun org-appear--expand-link (elem)
  "Make invisible parts of link inside element ELEM appear visible.
Special handler is needed for links 
because `org-link-descriptive' affects the entire buffer."
  (if (featurep 'org-fold)
      (org-fold-show-set-visibility 'local)
    (let ((start (org-element-property :begin elem))
	  (end (org-element-property :end elem)))
      (with-silent-modifications
	(remove-text-properties start end '(invisible org-link))))))

(provide 'org-appear)
;;; org-appear.el ends here
