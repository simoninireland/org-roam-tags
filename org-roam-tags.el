;;; org-roam-tags.el --- Manage content tags in org-roam notes -*- lexical-binding: t -*-

;; Copyright (c) 2023 Simon Dobson <simoninireland@gmail.com>

;; Author: Simon Dobson <simoninireland@gmail.com>
;; Maintainer: Simon Dobson <simoninireland@gmail.com>
;; Version: 0.1.1
;; Keywords: hypermedia, multimedia
;; Homepage: https://github.com/simoninireland/ox-attach-publish
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
;; org-roam-tags provides two operations:
;;
;; - functions to add new content tags, either inline or at file lavel
;; - a change to how links to content tags behave when followed,
;;   making them open the backlinks page to show all the notes
;;   linking to this tag

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
  :tag "Org Roam content tag SQL LIKE expression"
  :type 'string
  :group 'org-roam)

(defcustom org-roam-tags-tag-regexp (rx (seq bol (one-or-more (in lower digit "-")) eol))
  "Regular expression for recognising tags.

Tags must always match `org-roam-tags-tag-sql'. This regexp further
constrains the patterns used. The default is single words composed
of lowercase letters, digits, and dashes."
  :tag "Org Roam content tag regexp"
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
;; construct a Lisp data structure. The tags are then filtered at
;; the Emacs level, since the pattern is too complicated for SQL.

(defun org-roam-tags--tags ()
  "Return the list of tags.

The tags are returned in alphabetical order."
  (let ((taglike (org-roam-db-query [:select title :from nodes
				     :where  (not (like title $s1))] org-roam-tags-tag-sql)))
    (sort (org-roam-tags--only-tags (mapcar #'car taglike)) #'string<)))

(defun org-roam-tags--id-for-tag (tag)
  "Return the id associated with TAG."
  (if (org-roam-tags-tag-p tag)
      (let ((id (org-roam-db-query [:select id :from nodes
				    :where (= title $s1)] tag)))
	(if (not (null id))
	    (caar id)))))

(defun org-roam-tags--tag-for-id (id)
  "Return the tag associated with ID."
  (let ((tag (org-roam-db-query [:select title :from nodes
				 :where (= id $s1)] id)))
    (if (and (not (null tag))
	     (org-roam-tags-tag-p (caar tag)))
	(caar tag))))

(defun org-roam-tags--file-for-id (id)
  "Return the file where ID is defined."
  (let ((files (org-roam-db-query [:select file :from nodes
				   :where (= id $s1)] id)))
    (if (not (null files))
	(caar files))))

(defun org-roam-tags--tag-exists-p (tag)
  "Test whether TAG exists as a tag."
  (not (null (org-roam-tags--id-for-tag tag))))


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
  "Ensire there is a tag page for TAG.

This is a no-op if the tag already exists, and creates it
if not."
  (if (not (org-roam-tags--tag-exists-p tag))
      (org-roam-tags--create-tag tag)))

(defun org-roam-tags--insert-file-tag (tag)
  "Insert TAG into the file tag line.

The tag line is of the form '+ tags ::' at the bottom
of the file."
  (save-excursion
    ;; find the tag line
    (goto-char (point-max))
    (if (re-search-backward (rx (seq bol "+ tags ::")) nil t)
	;; line found, move to the end
	(end-of-line)

      ;; line not found, add one
      (progn
	(goto-char (point-max))
	(insert "\n\n"
		"+ tags ::")))

    ;; add the tag link
    (insert " ")
    (org-roam-tags--ensure-tag-exists tag)
    (org-roam-tags--insert-link-for-tag tag)))

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

;; This uses `completing-read' by default, and `helm' if present.

(defun org-roam-tags--select-tag-and-go-default (f)
  "Select a content tag and pass it to F."
  (let ((tag (completing-read "Content tag: " (org-roam-tags--tags) nil nil)))
    (if tag
	(funcall f tag))))

(defun org-roam-tags--select-tag-and-go (f)
  "Select a content tag using `helm' and pass it to F."
  (helm :sources (helm-build-sync-source "org-roam tags"
		   :candidates #'org-roam-tags--tags
		   :volatile t
		   :must-match 'ignore
		   :action (list (cons "Add tag to tag line of note"
				       (lambda (tag)
					 (funcall f tag)))))
	:buffer "*org-roam content tags"
	:prompt "Content tag: "))

(if (featurep 'helm)
    (defun org-roam-tags--select-tag-and-go (f)
      "Select a content tag using `helm' and pass it to F."
      (helm :sources (helm-build-sync-source "org-roam tags"
		       :candidates #'org-roam-tags--tags
		       :volatile t
		       :must-match 'ignore
		       :action (list (cons "Add tag to tag line of note"
					   (lambda (tag)
					     (funcall f tag)))))
	    :buffer "*org-roam content tags"
	    :prompt "Content tag: "))

  (defun org-roam-tags--select-tag-and-go (f)
    "Select a content tag and pass it to F."
    (let ((tag (completing-read "Content tag: " (org-roam-tags--tags) nil nil)))
      (if tag
	  (funcall f tag)))))


;; ---------- Public interface ----------

(defun org-roam-tags-tag-note ()
  "Tag the current note by adding a tag to its tag line.

The tag is selected interactively. A tag line is added if needed.
If the tag is new, a tag note is created for it."
  (interactive)
  (org-roam-tags--select-tag-and-go #'org-roam-tags--insert-file-tag))

(defun org-roam-tags-tag-note-at-point ()
  "Tag the current note by inserting a tag at point.

The tag is selected interactively and if the tag is new, a
tag note is created for it."
  (interactive)
  (org-roam-tags--select-tag-and-go #'org-roam-tags--insert-tag))

(defun org-roam-tags-open-tag ()
  "Select a tag and open it."
  (interactive)
  (org-roam-tags--select-tag-and-go #'org-roam-tags--open-tag))


;; ---------- Installation----------

;; Hang the hook function on the hook for opening links in org documents
(add-to-list 'org-open-at-point-functions
	     #'org-roam-tags--open-tag-link-at-point)


(provide 'org-roam-tags)
;; org-roam-tags.el ends here
