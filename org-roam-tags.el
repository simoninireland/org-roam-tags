;;; org-roam-tags.el --- Manage content tags in org-roam notes -*- lexical-binding: t -*-

;; Copyright (c) 2023 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Version: 0.1.1
;; Keywords: hypermedia, multimedia
;; Homepage: https://github.com/simoninireland/org-roam-tags
;; Package-Requires: ((emacs "27.2") (org "8.0") (org-roam)

;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-roam adds a database layer to Org documents, which (amongst
;; other things) provides access to backlinks to documents. This
;; in turn lets us treat some documents like tags, that exist purely
;; as targets for backlinks. This style is more flexible than normal
;; Org tags as they can be added anywhere in a note, not simply to
;; headings. We refer to thes emore flexible tags as "content" tags.
;;
;; org-roam-tags provides three user-level operations:
;;
;; - functions to add new content tags, either inline or at file level
;; - a function to open a backlinks buffer for a content tag
;; - a change to how links to content tags behave when followed,
;;   making them open the backlinks buffer (instead of the tag's note)
;;
;; and two programming operations:
;;
;; - to find all the tags in a buffer
;; - to find all the file tags in a buffer, which are those on a line
;;   starting "+ tags ::" by default

;;; Code:

(require 'dash)
(require 'org)
(require 'org-roam)


;; ---------- Tags ----------

(defcustom org-roam-tags-tag-sql "# #"
  "SQL LIKE expression for recognising tags.

Returns all 'tag-like' note titles using a database query. The
actual tags must match this SQL expression and also the regexp
defined in `org-roam-tags-tag-regexp'.

The default matches all single words, without spaces. There's
probably no need to change this."
  :tag "Org Roam Tags content tag SQL LIKE expression"
  :type 'string
  :group 'org-roam)


(defcustom org-roam-tags--file-tags-line-marker "+ tags ::"
  "String used by org-roam-tags package to denote the start of the file tags line."
  :tag "Org Roam Tags file tags line marker"
  :group 'org-roam
  :type 'string)


(defcustom org-roam-tags-tag-regexp (rx (seq bol (one-or-more (in lower digit "-" ":")) eol))
  "Regular expression for recognising tags.

Tags must always match `org-roam-tags-tag-sql'. This regexp further
constrains the patterns used. The default is single words composed
of lowercase letters, digits, dashes, and colons."
  :tag "Org Roam Tags content tag regexp"
  :type 'regexp
  :group 'org-roam)


(defun org-roam-tags-tag-p (taglike)
  "Test whether TAGLIKE is a tag.

Tags are selected by matching `org-roam-tags-tag-regexp', case-sensitively."
  (let ((case-fold-search nil))
    (string-match org-roam-tags-tag-regexp taglike)))


(defun org-roam-tags--only-tags (taglike)
  "Filters a list of TAGLIKE strings to remove those that are not tags."
  (-filter #'org-roam-tags-tag-p taglike))


;; ---------- Database interaction----------

;; We always go stright to the database with queries, rather than
;; construct a Lisp data structure.
;;
;; Tags arew filtered twice, once in SQL and once in Emacs. The first
;; sift should take out most non-tag notes; the second refines that selection
;; ready to be presented for choice.
;;
;; The SQL queries are in the form used by emacsql.

(defun org-roam-tags--tags ()
  "Return the list of tags.

The tags are returned in alphabetical order."
  (let ((taglike (org-roam-db-query [:select title :from nodes
				     :where  (not (like title $s1))]
				    org-roam-tags-tag-sql)))
    (sort (org-roam-tags--only-tags (mapcar #'car taglike)) #'string<)))


(defun org-roam-tags--id-for-tag (tag)
  "Return the id associated with TAG."
  (if (org-roam-tags-tag-p tag)
      (let ((id (org-roam-db-query [:select id :from nodes
					    :where (= title $s1)]
				   tag)))
	(if (not (null id))
	    (caar id)))))


(defun org-roam-tags--tag-for-id (id)
  "Return the tag associated with ID."
  (let ((tag (org-roam-db-query [:select title :from nodes
					 :where (= id $s1)]
				id)))
    (if (and (not (null tag))
	     (org-roam-tags-tag-p (caar tag)))
	(caar tag))))


(defun org-roam-tags--file-for-id (id)
  "Return the file where ID is defined."
  (let ((files (org-roam-db-query [:select file :from nodes
					   :where (= id $s1)]
				  id)))
    (if (not (null files))
	(caar files))))


(defun org-roam-tags--tag-exists-p (tag)
  "Test whether TAG exists as a tag."
  (not (null (org-roam-tags--id-for-tag tag))))


(defun org-roam-tags--tags-for-element (id)
  "Return the tags associated with the element ID."
  (let ((links (mapcar #'car (org-roam-db-query [:select dest :from links
						 :where (= source $s1)]
						id))))
    (-filter #'org-not-nil (mapcar #'org-roam-tags--tag-for-id links))))


;; ---------- Tagging ----------

(defun org-roam-tags--link-for-tag (tag)
  "Return an org-mode link string for TAG.

The target of the link is the id associated with TAG."
  (let ((id (org-roam-tags--id-for-tag tag)))
    (if id
	(format "[[id:%s][%s]]" id tag)
      (error (format "No tag %s" tag)))))


(defun org-roam-tags--insert-link-for-tag (tag)
  "Insert a link at point to TAG."
  (let ((link (org-roam-tags--link-for-tag tag)))
    (if link
	(insert link))))


(defun org-roam-tags--tag-filename (tag)
  "Return the filename for a note associated with TAG."
  (concat (expand-file-name org-roam-directory) "/" tag ".org"))


(defun org-roam-tags--create-tag (tag)
  "Create a new org mode page for TAG.

The tag page isn't intended to be informative and so gets
very little content. You can of course add more content,
for example an explanation of what the tag does.

Return the id used as the target for this tag."
  (let ((fn (org-roam-tags--tag-filename tag))
	(id (org-id-new)))
    (if (file-exists-p fn)
	(error (format "Document for tag %s exists already (%s)" tag fn)))

    ;; populate the file
    (with-temp-file fn
      (insert
       ":PROPERTIES:\n"
       ":ID: " id "\n"
       ":END:\n"
       "#+TITLE: " tag "\n"
       "\n"
       "* " tag "\n"))

    ;; save the new id's location in the org database
    (org-id-add-location id fn)

    ;; update org-roam's database
    (org-roam-db-update-file fn)

    ;; record tag creation
    (message (format "New tag %s created" tag))

    ;; return the new tag's id
    id))


(defun org-roam-tags--ensure-tag-exists (tag)
  "Ensure there is a tag page for TAG.

This is a no-op if the tag already exists, and creates it
if not, in either case returning TAG. Returns nil if the
user rejects creating the tag."
  (if (org-roam-tags--tag-exists-p tag)
      tag
    (if (yes-or-no-p (format "Create new tag %s? " tag))
	(progn
	  (org-roam-tags--create-tag tag)
	  tag)
      (message "Tag creation aborted"))))


(defun org-roam-tags--find-file-tags-line ()
  "Find a file-level line to hold tags, moving point there.

The tag line is of the form '+ tags ::' at the bottom of the
file. Point is left immediately after the tags marker."
  (goto-char (point-max))
  (if (re-search-backward (concat "^" org-roam-tags--file-tags-line-marker) nil t)
      ;; line found
      (progn
	;; move to after the marker
	(forward-char (length org-roam-tags--file-tags-line-marker)))

    ;; line not found, add one
    (progn
      (goto-char (point-max))
      (insert "\n\n"
	      org-roam-tags--file-tags-line-marker))))


(defun org-roam-tags--find-file-tags-line-and-append ()
  "Move point ready to receive a new tag."
  (org-roam-tags--find-file-tags-line)

  ;; delete trailing whitespace
  (save-excursion
    (let ((s (progn
	       (beginning-of-line)
	       (point)))
	  (e (progn
	       (end-of-line)
	       (point))))
      (delete-trailing-whitespace s e)))

  ;; prepare to insert
  (end-of-line)
  (insert " "))


(defun org-roam-tags--find-file-tags-line-and-clear ()
  "Ensure there is an empty tags line in the buffer."
  (org-roam-tags--find-file-tags-line)
  (delete-region (point) (point-max)))


(defun org-roam-tags--insert-file-tag (tag)
  "Insert TAG into the file tag line."
  (save-excursion
    (org-roam-tags--find-file-tags-line-and-append)
    (if (org-roam-tags--ensure-tag-exists tag)
	(org-roam-tags--insert-link-for-tag tag))))


(defun org-roam-tags--insert-tag (tag)
  "Insert TAG into the file at point.

Spaces are inserted if required between the link and the surrounding
text."
  ;; add a space before the link if necessary
  (let ((syn (char-syntax (char-before))))
    (if (not (or (bolp)
		 (eq syn ?\ )
		 (eq syn ?-)))
	(insert " ")))

  ;; add the tag link
  (org-roam-tags--ensure-tag-exists tag)
  (org-roam-tags--insert-link-for-tag tag)

  ;; add a space after the link if necessary
  (let ((syn (char-syntax (char-after))))
    (if (not (or (eolp)
		 (eq syn ?\ )
		 (eq syn ?-)))
	(insert " "))))


;; ---------- Opening tag links ----------

(defun org-roam-tags--open-tag (tag)
  "Open TAG, which shows the backlinks to the tag page."
  (let* ((id (org-roam-tags--id-for-tag tag))
	 (node (org-roam-node-from-id id)))
    (org-roam-buffer-display-dedicated node)))


(defun org-roam-tags--open-tag-link-at-point ()
  "Check for a tag link at point, and open it.

This is intended to be hung on `org-open-at-point-functions' to
recognise links to tag pages, and then use `org-roam-tags--open-tag'
to open the tag.

If the link clicked is not a tag (because it's not an id: link,
or because the id isn't of a page whose title is an acceptable tag),
the function returns 'nil' which causes `org-open-at-point' to
drop-through to the rest of the handlers."
  (let ((context (org-element-context)))
    (when (and (eq (org-element-type context) 'link)
	       (equal (org-element-property :type context) "id"))
      (let* ((id (org-element-property :path context))
	     (tag (org-roam-tags--tag-for-id id)))
	(when tag
	  ;; open the tag
	  (org-roam-tags--open-tag tag)

	  ;; return t to inhibit further processing
	  t)))))


;; ---------- Tag selection ----------

(defun org-roam-tags--select-tag-and-go (f)
  "Select a content tag and pass it to F."
  (let ((tag (completing-read "Content tag: " (org-roam-tags--tags) nil nil)))
    (if tag
	(funcall f tag))))


;; ---------- Public interface ----------

;;;###autoload
(defun org-roam-tags-tag-note (fromscratch)
  "Tag the current note by adding a tag to its tag line.

The tag is selected interactively. A tag line is added if needed.
If the tag is new, a tag note is created for it.

If executed with prefix argument FROMSCRATCH, the existing tags
are deleted before selection is offered."
  (interactive "P")

  (if fromscratch
      (org-roam-tags--find-file-tags-line-and-clear))
  (org-roam-tags--select-tag-and-go #'org-roam-tags--insert-file-tag))


;;;###autoload
(defun org-roam-tags-tag-note-at-point ()
  "Tag the current note by inserting a tag at point.

The tag is selected interactively and if the tag is new, a
tag note is created for it."
  (interactive)
  (org-roam-tags--select-tag-and-go #'org-roam-tags--insert-tag))


;;;###autoload
(defun org-roam-tags-open-tag ()
  "Select a tag and open it."
  (interactive)
  (org-roam-tags--select-tag-and-go #'org-roam-tags--open-tag))


;; ---------- Public programming interface ----------

;;;###autoload
(defun org-roam-tags-all-tags-in-buffer ()
  "Return a list of strings representing all the tags found in this buffer."
  (let* ((p (org-element-parse-buffer))
	 (ids (org-element-map p 'link
		(lambda (l)
		  (if (equal (org-element-property :type l) "id")
		      (org-element-property :path l)))))
	 (tags (cl-remove-if #'null  (mapcar #'org-roam-tags--tag-for-id ids))))
    tags))


;; ---------- Installation----------

;; Hang the hook function on the hook for opening links in org documents
(add-to-list 'org-open-at-point-functions
	     #'org-roam-tags--open-tag-link-at-point)


(provide 'org-roam-tags)
;; org-roam-tags.el ends here
