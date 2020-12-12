;; org-capture-ref.el --- Extract bibtex info from captured websites  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Version: 0.3
;; Package-Requires: ((s "1.12.0") (org "9.3") (org-ref "1.1.1"))
;; Keywords: tex, multimedia, bib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a wrapper to `org-capture-templates' that automatically
;; extracts useful meta-information from the captured URLs. The
;; information is saved into bitex entry and can be reused to fill the
;; capture template.

;;; Code:

(require 'org-capture)
(require 'org-ref-url-utils)
(require 'org-ref-core)
(require 'org-ref-bibtex)
(require 'bibtex)
(require 'dom)
(require 's)

;;; Customization:

(defgroup org-capture-ref nil
  "Generation of bibtex info for captured webpages."
  :tag "Org capture bibtex generator"
  :group 'org-capture)

(defcustom org-capture-ref-get-buffer-functions '(org-capture-ref-get-buffer-from-html-file-in-query
				   org-capture-ref-retrieve-url)
  "Functions used to retrieve html buffer for the captured link.

Each function will be called without arguments in sequence.
First non-nil return value of these functions will be used as buffer
containing html source of the link.

These functions will be called only when `org-capture-ref-get-buffer' is invoked from anywhere."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-get-bibtex-functions '(;; First, pull generic data from capture
                                   org-capture-ref-get-bibtex-url-from-capture-data
				   org-capture-ref-get-bibtex-howpublished-from-url
                                   org-capture-ref-set-default-type
                                   org-capture-ref-set-access-date
                                   ;; Elfeed parsers
				   org-capture-ref-get-bibtex-from-elfeed-data
                                   ;; DOI retrieval
                                   org-capture-ref-get-bibtex-doi
                                   org-capture-ref-get-bibtex-aps
                                   org-capture-ref-get-bibtex-springer
                                   org-capture-ref-get-bibtex-wiley
                                   org-capture-ref-get-bibtex-tandfonline
                                   org-capture-ref-get-bibtex-semanticscholar
                                   org-capture-ref-get-bibtex-sciencedirect-article
                                   org-capture-ref-mark-links-with-known-absent-doi
                                   org-capture-ref-get-bibtex-from-first-doi
				   ;; Site-specific parsing
                                   org-capture-ref-get-bibtex-goodreads
                                   org-capture-ref-get-bibtex-amazon
				   org-capture-ref-get-bibtex-github
                                   org-capture-ref-get-bibtex-reddit
                                   org-capture-ref-get-bibtex-youtube-watch
                                   org-capture-ref-get-bibtex-habr
                                   org-capture-ref-get-bibtex-weixin
                                   org-capture-ref-get-bibtex-authortoday-work
                                   org-capture-ref-get-bibtex-authortoday-post
                                   org-capture-ref-get-bibtex-ficbook
                                   org-capture-ref-get-bibtex-lesswrong
                                   ;; OpenGraph parser
                                   org-capture-ref-parse-opengraph
				   ;; Generic parser
				   org-capture-ref-parse-generic)
  "Functions used to generate bibtex entry for captured link.

Each function will be called without arguments in sequence.
The functions are expected to use `org-capture-ref-set-bibtex-field'
and `org-capture-ref-set-capture-info'. to set the required bibtex
fields. `org-capture-ref-get-bibtex-field' and `org-capture-ref-get-capture-info' can
be used to retrieve information about the captured link.
Any function can throw an error and abort the capture process.
Any function can throw `:finish'. All the remaining functions from
this list will not be called then.

Any function can mark a field as not defined for the captured link.
This is done by setting that field to `org-capture-ref-placeholder-value'.
The following parsers will then be aware that there is no need to search for the field."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-get-bibtex-from-elfeed-functions '(org-capture-ref-get-bibtex-generic-elfeed
					       org-capture-ref-get-bibtex-habr-elfeed
                                               org-capture-ref-get-bibtex-rgoswami-elfeed-fix-author
                                               org-capture-ref-get-bibtex-reddit-elfeed-fix-howpublished)
  "Functions used to generate BibTeX entry from elfeed entry data defined in `:elfeed-data' field of the `org-protocol' capture query.

This variable is only used if `org-capture-ref-get-bibtex-from-elfeed-data' is listed in `org-capture-ref-get-bibtex-functions'.
The functions must follow the same rules as `org-capture-ref-get-bibtex-functions', but will be called with a single argument - efleed entry object.

These functions will only be called if `:elfeed-data' field is present in `:query' field of the `org-store-link-plist'."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-clean-bibtex-hook '(org-ref-bibtex-format-url-if-doi
				orcb-key-comma
				orcb-&
				orcb-%
				orcb-clean-year
				orcb-clean-doi
				orcb-clean-pages
				org-capture-ref-sort-bibtex-entry
				orcb-fix-spacing
                                org-capture-ref-clear-nil-bibtex-entries
                                org-capture-ref-normalize-type
                                org-capture-ref-replace-%)
  "Normal hook containing functions used to cleanup BiBTeX entry string.

Each function is called with point at undefined position inside buffer
containing a single BiBTeX entry.  The buffer is set to `bibtex-mode'.

The functions have access to `org-capture-ref-get-bibtex-field' and
`org-capture-ref-set-bibtex-field', but there is no guarantee that the
returned value is (or will be) in sync with the BiBTeX entry in the
buffer. It is recommended to use `bibtex-set-field' or
`bibtex-parse-entry' directly.

The new BiBTeX string will be parsed back into the BiBTeX data
structure, and thus may affect anything set by
`org-capture-ref-set-bibtex-field'."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-get-formatted-bibtex-functions '(org-capture-ref-get-formatted-bibtex-default)
  "Functions used to format BiBTeX entry string.

Each function will be called without arguments in sequence.
`org-capture-ref-get-bibtex-field' and `org-capture-ref-get-capture-info' can
be used to retrieve information about the captured link.
Return value of the first function returning non-nil will be used as final format."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-generate-key-functions '(org-capture-ref-generate-key-from-doi
                                     org-capture-ref-generate-key-from-isbn
				     org-capture-ref-generate-key-from-url)
  "Functions used to generate citation key if it is not yet present.
The functions will be called in sequence until any of them returns non-nil value."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-check-bibtex-functions '(org-capture-ref-check-key
				     org-capture-ref-check-url
				     org-capture-ref-check-link)
  "Functions used to check the validity of generated BiBTeX.
  
The functions are called in sequence without arguments.
Any function can throw an error and abort the capture process.
Any function can throw `:finish'. All the remaining functions from
this list will not be called then."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-message-functions '(org-capture-ref-message-qutebrowser
				;; This should be last
                                org-capture-ref-message-emacs)
  "List of functions used to report the progress/errors during capture.
The functions must accept one or two arguments: message and severity.
Severity is one of symbols `info', `warning', `error'.
The last default function in this hook `org-capture-ref-message-emacs'
may throw error and hence prevent any laster function to be executed."
  :type 'hook
  :group 'org-capture-ref)

;; Customisation for default functions

(defcustom org-capture-ref-field-regexps '((:doi . ("scheme=\"doi\" content=\"\\([^\"]*\\)\""
				     "citation_doi\" content=\"\\([^\"]*\\)\""
				     "data-doi=\"\\([^\"]*\\)\""
				     "content=\"\\([^\"]*\\)\" name=\"citation_doi"
				     "objectDOI\" : \"\\([^\"]*\\)\""
				     "doi = '\\([^']*\\)'"
				     "\"http://dx.doi.org/\\([^\"]*\\)\""
				     "/doi/\\([^\"]*\\)\">"
				     "doi/full/\\([^&]*\\)&"
				     "doi=\\([^&]*\\)&amp"))
                            (:year . ("class=\\(?:date.[^>]*?\\)>[^<]*?\\([0-9]\\{4\\}\\)[^<]*?</"))
                            (:author . ("\\(?:<meta name=\"author\" content=\"\\(.+\\)\" ?/?>\\)\""
					"\\(?:<[^>]*?class=\"author[^\"]*name\"[^>]*>\\([^<]+\\)<\\)"))
                            (:title . ("<title.?+?>\\([[:ascii:][:nonascii:]]*?\\|.+\\)</title>")))
  "Alist holding regexps used by `org-capture-ref-parse-generic' to populate common BiBTeX fields from html.
Keys of the alist are the field names (example: `:author') and the values are lists or regexps.
The regexps are searched one by one in the html buffer and the group 1 match is used as value in the BiBTeX field."
  :group 'org-capture-ref
  :type '(alist :key-type symbol :value-type (list string)))

(defcustom org-capture-ref-demand-doi-list '("tandfonline\\.com/doi/"
                              "aps\\.org"
                              "springer\\.com/\\(?:chapter/\\)?\\([0-9a-z-_/.]+\\)"
                              "sciencedirect\\.com/science/article"
                              "wiley\\.com/doi/abs/\\([0-9a-z-_/.]+\\)"
                              "science\\.sciencemag\\.org"
                              "nature\\.com")
  "List of regexps matching URLs that must have DOI.

If DOI retrieval fails on these URLs, fallback options are not used -
the capture exits with error."
  :group 'org-capture-ref
  :type '(list string))

(defcustom org-capture-ref-default-type "misc"
  "Default BiBTeX type of the captured entry."
  :group 'org-capture-ref
  :type 'string)

(defcustom org-capture-ref-placeholder-value "unused"
  "Key value indicating that this key is not applicable for the captured entry.
There is no need to attempt finding the value for this key.")

(defcustom org-capture-ref-default-bibtex-template "@${:type}{${:key},
      author       = {${:author}},
      title        = {${:title}},
      journal      = {${:journal}},
      volume       = {${:volume}},
      number       = {${:number}},
      pages        = {${:pages}},
      year         = {${:year}},
      doi          = {${:doi}},
      url          = {${:url}},
      howpublished = {${:howpublished}},
      publisher = {${:publisher}},
      keywords     = {${:keywords}},
      note         = {Online; accessed ${:urldate}}
      }"
  "Default template used to format BiBTeX entry.
If a keyword from the template is missing, it will remain empty."
  :type 'string
  :group 'org-capture-ref)

(defcustom org-capture-ref-check-regexp-method 'grep
  "Search method in `org-capture-ref-check-regexp'.
This variable affects `org-capture-ref-check-url' and `org-capture-ref-check-link'."
  :type '(choice (const :tag "Use Unix grep" grep)
		 (const :tag "Use `org-search-view'" org-search-view))
  :group 'org-capture-ref)

(defcustom org-capture-ref-check-key-method 'grep
  "Search method in `org-capture-ref-check-key' when searching for IDs."
  :type '(choice (const :tag "Use Unix grep" grep)
		 (const :tag "Use `org-id-find'" org-id-find))
  :group 'org-capture-ref)

(defcustom org-capture-ref-warn-when-using-generic-parser t
  "Non-nil means warn user if some fields are trying to be parsed using generic parser.
`debug' means show all the details."
  :type 'boolean
  :group 'org-capture-ref)

;;; API

(defun org-capture-ref-get-buffer ()
  "Return buffer containing contents of the captured link.

Retrieve the contents first if necessary.
This calls `org-capture-ref-get-buffer-functions'."
  (let ((buffer (or org-capture-ref--buffer
		    (run-hook-with-args-until-success 'org-capture-ref-get-buffer-functions))))
    (unless (buffer-live-p buffer) (org-capture-ref-message (format "<org-capture-ref> Failed to get live link buffer. Got %s" buffer) 'error))
    (setq org-capture-ref--buffer buffer)))

(defun org-capture-ref-get-dom ()
  "Return parsed html of the captured link."
  (or org-capture-ref--buffer-dom
      (setq org-capture-ref--buffer-dom (with-current-buffer (org-capture-ref-get-buffer)
			   (libxml-parse-html-region (point-min) (point-max))))))

(defun org-capture-ref-query-opengraph (key &optional separator)
  "Query opengraph KEY from the website.
The KEY can be a symbol or string not prefixed with og:.
See https://ogp.me/ for possible KEY values.
SEPARATOR is separator used to concat array of KEYs (default is \" and \")."
  (when (symbolp key) (setq key (symbol-name key)))
  (setq key (s-concat "og:" key))
  (let ((ans (s-join (or separator " and ")
                     (mapcar (lambda (node) (dom-attr node 'content))
                             (dom-search (org-capture-ref-get-dom)
                                         (lambda (node)
                                           (and (eq (car node) 'meta)
                                                (or (string= key (dom-attr node 'property))
                                                    (string= key (dom-attr node 'name))))))))))
    (if (string-empty-p ans) nil ans)))

(defun org-capture-ref-query-meta (key &optional separator)
  "Query KEY from the website metadata.
The KEY can be a symbol or string.
SEPARATOR is separator used to concat array of KEYs (default is \" and \")."
  (when (symbolp key) (setq key (symbol-name key)))
  (let ((ans (s-join (or separator " and ")
                     (mapcar (lambda (node) (dom-attr node 'content))
                             (dom-search (org-capture-ref-get-dom)
                                         (lambda (node)
                                           (and (eq (car node) 'meta)
                                                (or (string= key (dom-attr node 'property))
                                                    (string= key (dom-attr node 'name))))))))))
    (if (string-empty-p ans) nil ans)))

(defun org-capture-ref-extract-year-from-string (string)
  "Extract year from date string."
  (when (and string (string-match "[0-9]\\{4\\}" string))
    (match-string 0 string)))

(defun org-capture-ref-get-bibtex-field (field &optional return-placeholder-p)
  "Return the value of the BiBTeX FIELD or nil the FIELD is not set.
Unless RETURN-PLACEHOLDER-P is non-nil, return nil when the value is equal
to `org-capture-ref-placeholder-value'.
  
FIELD must be a symbol like `:author'.
See `org-capture-ref--bibtex-alist' for common field names."
  (if return-placeholder-p
      (alist-get field org-capture-ref--bibtex-alist)
    (let ((res (alist-get field org-capture-ref--bibtex-alist)))
      (unless (string-equal res org-capture-ref-placeholder-value) res))))

(defun org-capture-ref-get-capture-template-info (key)
  "Return value of KEY from `org-capture-plist'."
  (plist-get org-capture-plist key))

(defun org-capture-ref-get-capture-info (key)
  "Return value of KEY from `org-capture-ref--store-link-plist'.
  
See docstring of `org-capture-ref--store-link-plist' for possible KEYs.
KEY can be a list, which means that the `car' of KEY is a plist
containing `cdar' of KEY, an so on."
  (when (symbolp key) (setq key (list key)))
  (let ((plist org-capture-ref--store-link-plist))
    (while key
      (setq plist (plist-get plist (pop key))))
    plist))

(defun org-capture-ref-set-bibtex-field (field val &optional force)
  "Set BiBTeX FIELD to VAL.
  
FIELD must be a symbol like `:author'.
See `org-capture-ref--bibtex-alist' for common field names.
If VAL is empty string, do not do anything.
Bypass VAL check when FORCE is non-nil."
  (unless (or force (string-empty-p val) (not val))
    (setf (alist-get field org-capture-ref--bibtex-alist) val)))

(defun org-capture-ref-set-capture-info (key val)
  "Set KEY in capture info to VAL.
  
The KEY set here will be passed down to org-capture via
`org-store-link-plist'.
See docstring of `org-capture-ref--store-link-plist' for possible KEYs."
  (plist-put org-capture-ref--store-link-plist key val))

;;; Predefined functions

;; Getting html buffer

(defun org-capture-ref-get-buffer-from-html-file-in-query ()
  "Use buffer from file defined in `:html' field of `org-protocol' query."
  (let* ((html (org-capture-ref-get-capture-info '(:query :html))))
    (when html
      (with-current-buffer (get-buffer-create html)
	(insert-file-contents html)
        (current-buffer)))))

(defun org-capture-ref-retrieve-url ()
  "Retrieve html buffer from `:link' field of capture data."
  (let ((link (org-capture-ref-get-capture-info :link)))
    (when link
      (url-retrieve-synchronously link))))

(defun org-capture-ref--get-bibtex-string-from-isbn (isbn)
  "Get BiBTeX record for given ISBN.
Use https://www.ottobib.com to retrieve the BiBTeX record."
  (let ((url (concat "https://www.ottobib.com/isbn/" isbn "/bibtex"))
	data)
    (with-current-buffer
	(url-retrieve-synchronously
	 ;; (concat "http://dx.doi.org/" doi)
	 url)
      (setq data (buffer-substring url-http-end-of-headers (point-max)))
      (cond
       ((or (string-match "<title>Error: DOI Not Found</title>" data)
	    (string-match "Resource not found" data)
	    (string-match "Status *406" data)
	    (string-match "400 Bad Request" data))
	(error "Something went wrong.  We got this response:
%s" data))
       ;; everything seems ok with the data
       (t
        (let ((bibtex (dom-text (dom-by-tag (libxml-parse-html-region (point-min) (point-max)) 'textarea))))
          (if (string-empty-p bitex)
              (error "ISBN record %s not found" isbn)
            bibtex)))))))

;; Getting BiBTeX

(defun org-capture-ref-get-bibtex-from-elfeed-data ()
  "Run `org-capture-ref-get-bibtex-from-elfeed-functions'."
  (let ((elfeed-entry (org-capture-ref-get-capture-info '(:query :elfeed-data))))
    (when elfeed-entry
      (require 'elfeed)
      (run-hook-with-args 'org-capture-ref-get-bibtex-from-elfeed-functions elfeed-entry))))

(defun org-capture-ref-parse-opengraph ()
  "Generic parser for websites supporting OpenGraph protocol.

See https://ogp.me/ for details."
  (unless (-all-p (lambda (key)
		    (org-capture-ref-get-bibtex-field key 'consider-placeholder))
		  '(:title :url :howpublished))
    (let ((type (org-capture-ref-query-opengraph 'type))
          (title (org-capture-ref-query-opengraph 'title))
          (url (org-capture-ref-query-opengraph 'url))
          (howpublished (org-capture-ref-query-opengraph 'site_name)))
      (unless (org-capture-ref-get-bibtex-field :title t)
        (org-capture-ref-set-bibtex-field :title title))
      (org-capture-ref-set-bibtex-field :url url)
      (org-capture-ref-set-bibtex-field :howpublished howpublished)
      (pcase (org-capture-ref-query-opengraph 'type)
        ("article"
         (org-capture-ref-set-bibtex-field :type "article")
         (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-opengraph 'article:author))
         (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (org-capture-ref-query-opengraph 'article:published_time)))
         (org-capture-ref-set-bibtex-field :tag (org-capture-ref-query-opengraph 'article:tag ", ")))
        ("book"
         (org-capture-ref-set-bibtex-field :type "book")
         (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-opengraph 'book:author))
         (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (org-capture-ref-query-opengraph 'book:release_date)))
         (org-capture-ref-set-bibtex-field :isbn (org-capture-ref-extract-year-from-string (org-capture-ref-query-opengraph 'book:isbn)))
         (org-capture-ref-set-bibtex-field :tag (org-capture-ref-query-opengraph 'article:tag ", ")))))))

(defun org-capture-ref-parse-generic ()
  "Generic parser for the captured html.
Sets BiBTeX fields according to `org-capture-ref-field-regexps'.
Existing BiBTeX fields are not modified."
  ;; Do not bother is everything is already set.
  (unless (-all-p (lambda (key)
		    (org-capture-ref-get-bibtex-field key 'consider-placeholder))
		  (mapcar #'car org-capture-ref-field-regexps))
    (when org-capture-ref-warn-when-using-generic-parser
      (org-capture-ref-message "Capturing using generic parser..." 'warning))
    ;; Try to find in metadata first.
    (unless (org-capture-ref-get-bibtex-field :author)
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-meta 'author)))
    ;; Last resort is regexp.
    (with-current-buffer (org-capture-ref-get-buffer)
      (dolist (alist-elem org-capture-ref-field-regexps)
	(let ((key (car alist-elem))
	      (regexps (cdr alist-elem)))
          (unless (org-capture-ref-get-bibtex-field key 'consider-placeholder)
            (when (eq org-capture-ref-warn-when-using-generic-parser 'debug)
	      (org-capture-ref-message (format "Capturing using generic parser... searching %s..." key)))
            (catch :found
              (dolist (regex regexps)
		(goto-char (point-min))
		(when (re-search-forward regex nil t)
		  (org-capture-ref-set-bibtex-field key (decode-coding-string (match-string 1) 'utf-8))
		  (throw :found t))))
            (when (eq org-capture-ref-warn-when-using-generic-parser 'debug)
	      (if (org-capture-ref-get-bibtex-field :key)
		  (org-capture-ref-message (format "Capturing using generic parser... searching %s... found" key))
		(org-capture-ref-message (format "Capturing using generic parser... searching %s... failed" key) 'warning)))))))))

(defun org-capture-ref-mark-links-with-known-absent-doi ()
  "Prevent `org-capture-ref-get-bibtex-from-first-doi' from searching DOI in website text.

Some websites contain DOI references in the articles/blogs. However,
the actual website page does not have DOI.
`org-capture-ref-get-bibtex-from-first-doi' can sometimes give
false-positive results in such websites."
  (when (s-match (regexp-opt '("reddit.com"
			       "youtube.com"
                               "lesswrong.com"
                               "zettelkasten.de"
                               "github.com"
                               "wikipedia.org"
                               "app.dimensions.ai"
                               "scholar.google.com"
                               "gwern.net"))
                 (org-capture-ref-get-capture-info :link))
    (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)))

(defun org-capture-ref-get-bibtex-from-first-doi ()
  "Generate BiBTeX using first DOI record found in html or `:doi' field.
Use `doi-utils-doi-to-bibtex-string' to retrieve the BiBTeX record."
  (when (and (not (org-capture-ref-get-bibtex-field :doi 'consider-placeholder))
	     (alist-get :doi org-capture-ref-field-regexps))
    (let ((org-capture-ref-field-regexps (list (assq :doi org-capture-ref-field-regexps)))
	  org-capture-ref-warn-when-using-generic-parser)
      (org-capture-ref-parse-generic)))
  (let ((doi (org-capture-ref-get-bibtex-field :doi)))
    (when doi
      (org-capture-ref-message (format "Retrieving DOI record %s ..." doi))
      (let ((bibtex-string (condition-case err
			       ;; Ignore errors and avoid opening the DOI url.
			       (cl-letf (((symbol-function 'browse-url) #'ignore))
				 (doi-utils-doi-to-bibtex-string doi))
                             (t (org-capture-ref-message (format "%s" (error-message-string err)) 'warning)))))
        (unless bibtex-string (org-capture-ref-set-bibtex-field :doi nil 'force))
        (if (not bibtex-string)
            (if (-any-p (lambda (regexp) (s-match regexp (org-capture-ref-get-bibtex-field :url))) org-capture-ref-demand-doi-list)
                (org-capture-ref-message (format "Retrieving DOI record %s ... failed, but demanded for %s" doi (org-capture-ref-get-bibtex-field :url)) 'error)
              (org-capture-ref-message (format "Retrieving DOI record %s ... failed. Proceding with fallback options." doi) 'warning))
          (org-capture-ref-message "Retrieving DOI record... done")
	  (org-capture-ref-clean-bibtex bibtex-string 'no-hooks)
          (throw :finish t))))))

(defun org-capture-ref-get-bibtex-from-isbn ()
  "Generate BiBTeX using ISBN number found `:isbn' field."
  (let ((isbn (org-capture-ref-get-bibtex-field :isbn)))
    (when isbn
      (org-capture-ref-message (format "Retrieving ISBN record %s ..." isbn))
      (let ((bibtex-string (condition-case err
			       ;; Ignore errors and avoid opening the ISBN url.
			       (org-capture-ref--get-bibtex-string-from-isbn isbn)
                             (t (org-capture-ref-message (format "%s" (error-message-string err)) 'warning)))))
        (unless bibtex-string (org-capture-ref-set-bibtex-field :isbn nil 'force))
        (if (not bibtex-string)
            (org-capture-ref-message (format "Retrieving ISBN record %s ... failed. Proceding with fallback options." isbn) 'warning)
          (org-capture-ref-message "Retrieving ISBN record... done")
	  (org-capture-ref-clean-bibtex bibtex-string 'no-hooks)
          (throw :finish t))))))

(defun org-capture-ref-get-bibtex-url-from-capture-data ()
  "Get the `:url' using :link data from capture."
  (let ((url (org-capture-ref-get-capture-info :link)))
    (org-capture-ref-set-bibtex-field :url url)))

(defun org-capture-ref-get-bibtex-howpublished-from-url ()
  "Generate `:howpublished' field using `:url' BiBTeX field.
The generated value will be the website name."
  (unless (org-capture-ref-get-bibtex-field :howpublished)
    (let ((url (or (org-capture-ref-get-bibtex-field :url))))
      (when url
        (string-match "\\(?:https?://\\)?\\(?:www\\.\\)?\\([^/]+\\)\\.[^/]+/?" url)
        (org-capture-ref-set-bibtex-field :howpublished (capitalize (match-string 1 url)))))))

(defun org-capture-ref-set-default-type ()
  "Set `:type' of the BiBTeX entry to `org-capture-ref-default-type'."
  (unless (org-capture-ref-get-bibtex-field :type)
    (org-capture-ref-set-bibtex-field :type org-capture-ref-default-type)))

(defun org-capture-ref-set-access-date ()
  "Set `:urldate' field of the BiBTeX entry to now."
  (org-capture-ref-set-bibtex-field :urldate (format-time-string "%d %B %Y")))

(defun org-capture-ref-get-bibtex-weixin ()
  "Parse BiBTeX for Wechat article."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "mp\\.weixin\\.qq\\.com" link)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "\\(sn=[^&]+\\).*$" "\\1" link))
      (org-capture-ref-set-bibtex-field :howpublished "Wechat")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :title (s-trim (dom-text (dom-by-class (org-capture-ref-get-dom) "rich_media_title"))))
      (org-capture-ref-set-bibtex-field :author (s-trim (dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "rich_media_meta rich_media_meta_nickname") 'a))))
      (with-current-buffer (org-capture-ref-get-buffer)
	(goto-char (point-min))
        (when (re-search-forward "=\"\\([0-9]\\{4\\}\\)-[0-9]\\{2\\}-[0-9]\\{2\\}\"")
          (org-capture-ref-set-bibtex-field :year (match-string 1)))))))

(defun org-capture-ref-get-bibtex-reddit ()
  "Parse reddit link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:old\\.\\)?reddit\\.com\\(?:/r/\\([^/]+\\)\\)?" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "old\\.reddit\\.com" "reddit.com" link))
      (if (match-string 1 link)
	  (progn
            (org-capture-ref-set-bibtex-field :howpublished (format "Reddit:%s" (match-string 1 link)))
            (org-capture-ref-set-bibtex-field :title
			       (replace-regexp-in-string
				(rx (zero-or-more " ")
				    ":"
				    (zero-or-more " ")
				    (literal (match-string 1 link))
				    eol)
				""
				(org-capture-ref-get-capture-info :description))))
	(org-capture-ref-set-bibtex-field :howpublished "Reddit"))
      ;; Generic parser works ok.
      (let (org-capture-ref-warn-when-using-generic-parser)
	(org-capture-ref-parse-generic))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-github ()
  "Parse Github link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "git\\(hub\\|lab\\)\\.com" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      ;; Fix URL
      (when (string-match "^\\(.+\\)/tree/[a-zA-Z0-9]+$" link)
        (org-capture-ref-set-bibtex-field :url (match-string 1 link)))
      ;; Find author
      (unless (org-capture-ref-get-bibtex-field :author 'consider-placeholder)
	(when (string-match "\\(?:https://\\)?git\\(?:hub\\|lab\\)\\.com/\\([^/]+\\)" link)
          (org-capture-ref-set-bibtex-field :author (match-string 1 link))))
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
		      '(:title))
	(with-current-buffer (org-capture-ref-get-buffer)
	  ;; find title
	  (goto-char (point-min))
	  (when (re-search-forward "<title>\\([^>]+\\)</title>" nil t)
	    (let ((title (decode-coding-string (match-string 1) 'utf-8)))
              (when (string-match "^\\(.+\\) at [0-9a-zA-Z]\\{20,\\}$" title)
		(setq title (match-string 1 title)))
              ;; Remove trailing Gitlab in title
              (setq title (replace-regexp-in-string ".?\\{3\\}Gitlab" "" title))
              ;; Temove author name from title
              (setq title (replace-regexp-in-string "^[^/]*/[ \t]*" "" title))
              (org-capture-ref-set-bibtex-field :title title)))
          (when (string-match-p "/commit/[a-z0-9]+" link)
            (goto-char (point-min))
            (when (re-search-forward "commit-title\">\\([^<]+\\)" nil t)
	      (let ((title (decode-coding-string (match-string 1) 'utf-8)))
		(setq title (replace-regexp-in-string "([^(]*$" "" title))
		(setq title (s-trim title))
		(org-capture-ref-set-bibtex-field :title title))))
          (when (string-match "/issues/\\([0-9]+\\)" link)
            (goto-char (point-min))
            (let ((issue-number (match-string 1 link)))
              (when (re-search-forward "js-issue-title\">\\([^<]+\\)" nil t)
		(let ((title (decode-coding-string (match-string 1) 'utf-8)))
		  (setq title (s-trim title))
		  (org-capture-ref-set-bibtex-field :title (s-concat  "issue#" issue-number " " title)))))
            (goto-char (point-min))
            (when (re-search-forward ">\\([^<]+\\)</a>[^<]+opened this issue" nil t)
	      (let ((author (decode-coding-string (match-string 1) 'utf-8)))
		(org-capture-ref-set-bibtex-field :author author))))
	  ;; Year has no meaning for repo
	  (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
	  (when (string-match-p "github" link)
            (org-capture-ref-set-bibtex-field :howpublished "Github"))
	  (when (string-match-p "gitlab" link)
            (org-capture-ref-set-bibtex-field :howpublished "Gitlab")))))))

(defun org-capture-ref-get-bibtex-youtube-watch ()
  "Parse Youtube watch link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "youtube\\.com/watch" link)
      ;; Remove garbage from the link
      (setq link (replace-regexp-in-string "&[^/]+$" "" link))
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-capture-info :link link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
                      '(:author :title :year))
	;; Find author
        (org-capture-ref-set-bibtex-field :author (decode-coding-string
                                                   (dom-text
                                                    (dom-by-tag (dom-by-tag (org-capture-ref-get-dom)
                                                                            'ytd-channel-name)
                                                                'a))
                                                   'utf-8))
	;; Find title
        (org-capture-ref-set-bibtex-field :title (decode-coding-string
                                   (dom-text
                                    (dom-by-tag (dom-by-class
                                                 (org-capture-ref-get-dom)
                                                 "^title style-scope ytd-video-primary-info-renderer$")
                                                'yt-formatted-string))
                                   'utf-8))
	;; Find year
        (org-capture-ref-set-bibtex-field
         :year
         (org-capture-ref-extract-year-from-string (decode-coding-string (dom-text
                                                           (dom-by-tag (dom-by-id (org-capture-ref-get-dom)
                                                                                  "^date$")
                                                                       'yt-formatted-string))
                                                          'utf-8)))))))

(defun org-capture-ref-get-bibtex-habr ()
  "Parse Habrahabr link and generate BiBTeX entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "habr\\.com" link)
      ;; Unify company blog articles and normal articles
      (setq link (replace-regexp-in-string "company/[^/]+/blog/" "post/" link))
      (setq link (replace-regexp-in-string "/\\?[^/]+$" "/" link))
      (org-capture-ref-set-capture-info :link link)
      (org-capture-ref-set-bibtex-field :url link)
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
                      '(:url :author :title :year))
	(with-current-buffer (org-capture-ref-get-buffer)
          ;; Simplify url
	  (goto-char (point-min))
	  (when (re-search-forward "\"page_url_canonical\": \"\\([^\"]+\\)\"" nil t)
	    (let ((url (s-replace "\n" "" (match-string 1))))
              (setq url (s-replace "?[^/]+$" "" url))
              (org-capture-ref-set-bibtex-field :url (s-replace "\\" "" url))))
	  ;; Find authors
	  (goto-char (point-min))
	  (when (re-search-forward "\"article_authors\": \\[\\([^]]+\\)" nil t)
            (let ((authors (s-split "," (s-collapse-whitespace (s-replace "\n" "" (match-string 1))))))
              (setq authors (mapcar (apply-partially #'s-replace-regexp "^[ ]*\"\\(.+\\)\"[ ]*$" "\\1") authors))
              (setq authors (s-join ", " authors))
              (org-capture-ref-set-bibtex-field :author authors)))
	  ;; Find title
	  (goto-char (point-min))
	  (when (re-search-forward "\"page_title\": \"\\([^\"]+\\)\"" nil t)
	    (let ((title (match-string 1)))
              (org-capture-ref-set-bibtex-field :title (decode-coding-string title 'utf-8))))
	  ;; Find year
	  (goto-char (point-min))
	  (when (re-search-forward "datePublished\": \"\\([^\"]+\\)\"" nil t)
	    (let ((year (match-string 1)))
              (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string year)))))))))

(defun org-capture-ref-get-bibtex-authortoday-work ()
  "Generate BiBTeX for an author.today/work book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "author\\.today/work" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Author.Today")
      (org-capture-ref-set-bibtex-field :publisher "Author.Today")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
                      '(:url :author :title :year))
        (org-capture-ref-set-bibtex-field :title (s-trim (dom-text (dom-by-class (dom-by-class (org-capture-ref-get-dom)
                                                                                "book-meta-panel")
                                                                  "book-title"))))
        (org-capture-ref-set-bibtex-field :author (s-join " and " (mapcar #'dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "book-authors") 'a))))
        (let ((date (dom-attr (assq 'span (dom-by-class (org-capture-ref-get-dom) "hint-top")) 'data-hint)))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string date)))))))

(defun org-capture-ref-get-bibtex-authortoday-post ()
  "Generate BiBTeX for an author.today/post post."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "author\\.today/post" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :howpublished "Author.Today")
      (org-capture-ref-set-bibtex-field :publisher "Author.Today")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
                      '(:url :author :title :year))
        (org-capture-ref-set-bibtex-field :title (s-trim (dom-text (dom-by-class (org-capture-ref-get-dom) "post-title"))))
        (org-capture-ref-set-bibtex-field :author (s-join " and " (mapcar #'dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "^mr$") 'a))))
        (let ((date (dom-attr (assq 'span (dom-by-class (org-capture-ref-get-dom) "hint-top-right mr")) 'data-hint)))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string date)))))))

(defun org-capture-ref-get-bibtex-ficbook ()
  "Generate BiBTeX for an ficbook.net book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "ficbook\\.net" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Ficbook")
      (org-capture-ref-set-bibtex-field :publisher "Ficbook")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (unless (-all-p (lambda (key)
			(org-capture-ref-get-bibtex-field key 'consider-placeholder))
                      '(:url :author :title :year))
        (org-capture-ref-set-bibtex-field :title (s-trim (dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom)
                                                                                             "fanfic-main-info")
                                                                               'h1))))
        (org-capture-ref-set-bibtex-field :author (s-join " and " (mapcar #'dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "creator-info") 'a))))
        (let ((date (dom-text (or (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "list-of-fanfic-parts") 'span)
                                  (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "part-date") 'span)))))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string date)))))))

(defun org-capture-ref-get-bibtex-goodreads ()
  "Generate BiBTeX for Goodreads book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "goodreads\\.com/book" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :isbn (org-capture-ref-query-meta 'books:isbn))
      (if (string= "null" (org-capture-ref-get-bibtex-field :isbn))
          (org-capture-ref-set-bibtex-field :isbn nil 'force)
        (org-capture-ref-get-bibtex-from-isbn))
      (org-capture-ref-set-bibtex-field :author (s-join " and " (mapcar #'dom-texts (dom-by-class (org-capture-ref-get-dom) "^authorName$"))))
      (org-capture-ref-set-bibtex-field :title (s-trim (dom-text (dom-by-id (org-capture-ref-get-dom) "^bookTitle$"))))
      (let ((details (s-replace-regexp "  +" " " (s-replace "\n" " " (dom-texts (dom-by-id (org-capture-ref-get-dom) "^details$"))))))
        
        (when (string-match "Published .*?\\([0-9]\\{4\\}\\) *by *\\([^(]+\\)\\(?:More details...\\).*?(?" details)
          (org-capture-ref-set-bibtex-field :publisher (s-trim (match-string 2 details)))
          (org-capture-ref-set-bibtex-field :year (match-string 1 details)))))))

(defun org-capture-ref-get-bibtex-amazon ()
  "Generate BiBTeX for Amazon book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "amazon\\.com" link)
      (org-capture-ref-set-bibtex-field :url link)
      (let ((isbn-line (seq-find (lambda (str) (s-contains-p "ISBN-10" str))
                                 (mapcar #'dom-texts
                                         (dom-by-class (car (dom-by-id (org-capture-ref-get-dom)
                                                                       "^detailBullets_feature_div$"))
                                                       "^a-list-item$")))))
        (when (string-match "\\([0-9X]\\{10\\}\\)" isbn-line)
          (org-capture-ref-set-bibtex-field :isbn (match-string 1 isbn-line))))
      (org-capture-ref-get-bibtex-from-isbn))))

(defun org-capture-ref-get-bibtex-aps ()
  "Generate BiBTeX for APS publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "aps\\.org/doi/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-springer ()
  "Generate BiBTeX for Springer publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    ;; Sometimes, / in doi is coded as %2F.
    (setq link (replace-regexp-in-string "%2F" "/" link))
    (when (string-match "springer\\.com/\\(?:chapter/\\)?\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-tandfonline ()
  "Generate BiBTeX for Tandfonline publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "tandfonline\\.com/doi/\\(?:full\\|abd\\)/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-wiley ()
  "Generate BiBTeX for Wiley publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "wiley\\.com/doi/abs/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-semanticscholar ()
  "Generate BiBTeX for Semanticscholar page."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "semanticscholar\\.org" link)
      (org-capture-ref-set-bibtex-field :doi (dom-text (dom-by-class (org-capture-ref-get-dom) "doi__link")))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-sciencedirect-article ()
  "Generate BiBTeX for Sciencedirect publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "sciencedirect\\.com/science/article" link)
      (org-capture-ref-set-bibtex-field :doi (replace-regexp-in-string "https?://doi\\.org/" "" (dom-text (dom-by-class (org-capture-ref-get-dom) "^doi$"))))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-lesswrong ()
  "Generate BiBTeX for LessWrong publication."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "lesswrong\\.com" link)
      (org-capture-ref-set-bibtex-field :howpublished "Lesswrong")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (dom-text (dom-by-tag (dom-by-class (org-capture-ref-get-dom)
                                                                     "^PostsAuthors-authorName$")
                                                       'a)))
      (org-capture-ref-set-bibtex-field :title (dom-text (dom-by-class (org-capture-ref-get-dom) "PostsPageTitle")))
      (let ((date (dom-text (cdr (dom-by-tag (dom-by-class (org-capture-ref-get-dom) "PostsPageDate-date") 'span)))))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string date))))))

(defun org-capture-ref-get-bibtex-doi ()
  "Generate BiBTeX for an actual doi.org link."
  (let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "doi\\.org/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

;; Getting BiBTeX from elfeed entries

(defun org-capture-ref-get-bibtex-generic-elfeed (entry)
  "Parse generic elfeed capture and generate bibtex entry."
  (require 'elfeed-db)
  (unless (org-capture-ref-get-bibtex-field :url)
    (org-capture-ref-set-bibtex-field :url (elfeed-entry-link entry)))
  (unless (org-capture-ref-get-bibtex-field :author)
    (let ((authors (plist-get (elfeed-entry-meta entry) :authors)))
      (setq authors (mapcar #'cadr authors))
      (if authors
	  (org-capture-ref-set-bibtex-field :author (s-join ", " authors))
	;; fallback to feed title
	(org-capture-ref-set-bibtex-field :author (elfeed-feed-title (elfeed-entry-feed entry))))))
  (unless (org-capture-ref-get-bibtex-field :title)
    (org-capture-ref-set-bibtex-field :title (elfeed-entry-title entry)))
  (unless (org-capture-ref-get-bibtex-field :keywords)
    (org-capture-ref-set-bibtex-field :keywords (s-join ", " (plist-get (elfeed-entry-meta entry) :categories))))
  (unless (org-capture-ref-get-bibtex-field :year)
    (org-capture-ref-set-bibtex-field :year (format-time-string "%Y" (elfeed-entry-date entry)))))

(defun org-capture-ref-get-bibtex-habr-elfeed (entry)
  "Fix title in habr elfeed entries.
This function is expected to be ran after `org-capture-ref-bibtex-generic-elfeed'."
  ;; Habr RSS adds indication if post is translated or from sandbox,
  ;; but it is not the case in the website. Unifying to make it
  ;; consistent.
  (when (s-match "habr\\.com" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :title (s-replace-regexp "^\\[[^]]+\\][ ]*" "" (org-capture-ref-get-bibtex-field :title)))
    (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
    (org-capture-ref-get-bibtex-generic-elfeed entry)
    (org-capture-ref-get-bibtex-habr)
    (throw :finish t)))

(defun org-capture-ref-get-bibtex-rgoswami-elfeed-fix-author (_)
  "Populate author for https://rgoswami.me"
  (when (s-match "rgoswami\\.me" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :author "Rohit Goswami")))

(defun org-capture-ref-get-bibtex-reddit-elfeed-fix-howpublished (_)
  "Mention subreddit in :howpublished."
  (when (s-match "\\(old\\.\\)?reddit\\.com" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :howpublished
		       (format "%s:%s"
			       (org-capture-ref-get-bibtex-field :howpublished)
                               (org-capture-ref-get-bibtex-field :keywords)))))

;; Generating cite key

(defun org-capture-ref-generate-key-from-url ()
  "Generate citation key from URL."
  (when-let (url (org-capture-ref-get-bibtex-field :url))
    (setq url (replace-regexp-in-string "https?://\\(www\\.?\\)?" "" url))
    (setq url (replace-regexp-in-string "[^a-zA-Z0-9/.]" "-" url))
    (sha1 url)))

(defun org-capture-ref-generate-key-from-doi ()
  "Generate citation key from DOI."
  (when-let ((doi (org-capture-ref-get-bibtex-field :doi)))
    (sha1 doi)))

(defun org-capture-ref-generate-key-from-isbn ()
  "Generate citation key from ISBN."
  (when-let ((isbn (org-capture-ref-get-bibtex-field :isbn)))
    (sha1 isbn)))


;; Formatting BibTeX entry

(defun org-capture-ref-get-formatted-bibtex-default ()
  "Default BiBTeX formatter."
  (replace-regexp-in-string (format "^.+{\\(%s\\)?},$" org-capture-ref-placeholder-value) ""
			    (s-format org-capture-ref-default-bibtex-template
				      (lambda (key &optional _)
					(or (org-capture-ref-get-bibtex-field (intern key))
					    ""))
				      org-capture-ref--bibtex-alist)))

;; Cleaning up BiBTeX entry

(defun org-capture-ref-clear-nil-bibtex-entries ()
  "Remove {nil} in BiBTeX record."
  (goto-char 1)
  (while (re-search-forward "{nil}" nil 'noerror)
    (kill-whole-line)))

(defun org-capture-ref-normalize-type ()
  "Make sure that record type is downcased."
  (goto-char 1)
  (re-search-forward "@[a-z]+{" nil t)
  (replace-match (downcase (match-string 0))))

(defun org-capture-ref-sort-bibtex-entry ()
  "Call `org-ref-sort-bibtex-entry' without hooks."
  (let (bibtex-clean-entry-hook
	(bibtex-entry-format '(opts-or-alts
			       numerical-fields
                               whitespace
                               page-dashes
                               inherit-booktitle)))
    (org-ref-sort-bibtex-entry)))

(defun org-capture-ref-replace-% ()
  "Escape % chars to avoid confusing org-capture."
  (goto-char 1)
  (while (re-search-forward "%[^%]" nil 'noerror)
    (goto-char (match-beginning 0))
    (insert "\\")
    (goto-char (match-end 0))))

;;; Message functions

(defun org-capture-ref-message-emacs (msg &optional severity)
  "Show message in Emacs."
  (pcase severity
    (`error (user-error msg))
    (`warning (message msg))
    (_ (message msg))))

(defun org-capture-ref-message-qutebrowser (msg &optional severity)
  "Show message in qutebrowser assuming that qutebrowser fifo is
avaible in :query -> :qutebrowser-fifo capture info."
  (when-let  ((fifo (org-capture-ref-get-capture-info '(:query :qutebrowser-fifo))))
    (pcase severity
      (`error (start-process-shell-command "Send message to qutebrowser"
					   nil
					   (format "echo 'message-error \"%s\"' >> %s" msg fifo)))
      (`warning (start-process-shell-command "Send message to qutebrowser"
					     nil
					     (format "echo 'message-warning \"%s\"' >> %s" msg fifo)))
      (_ (start-process-shell-command "Send message to qutebrowser"
				      nil
				      (format "echo 'message-info \"%s\"' >> %s" msg fifo))))))

(defun org-capture-ref-message (msg &optional severity)
  "Send messages via `org-capture-ref-message-functions'."
  (run-hook-with-args 'org-capture-ref-message-functions msg severity))

;;; Verifying BiBTeX to be suitable for Org environment

(defun org-capture-ref-get-message-string (marker)
  "Generate message string if a headline at MARKER matches the capture."
  (org-with-point-at marker
    (org-back-to-heading t)
    (format "Already captured into: %s:%s" (org-entry-get (point) "CATEGORY") (org-get-heading 'no-tags nil 'no-priority 'no-comment))))

(defun org-capture-ref-check-regexp (regexp &optional dont-show-match-p)
  "Check if REGEXP exists in org files using `org-capture-ref-check-regexp-method'.
If DONT-SHOW-MATCH-P is non-nil, do not show the match or agenda search with all matches."
  (pcase org-capture-ref-check-regexp-method
    (`grep (org-capture-ref-check-regexp-grep regexp dont-show-match-p))
    (`org-search-view (org-capture-ref-check-regexp-search-view regexp dont-show-match-p))
    (_ (org-capture-ref-message (format "Invalid value of org-capture-ref-check-regexp-method: %s" org-capture-ref-check-regexp-method) 'error))))

(defun org-capture-ref-check-regexp-grep (regexp &optional dont-show-match-p)
  "Check if REGEXP exists in org files using grep.
If DONT-SHOW-MATCH-P is non-nil, do not show the match or agenda search with all matches."
  (unless (executable-find "grep") (org-capture-ref-message "Cannot find grep executable" 'error))
  (let (files
	matches)
    (setq files (org-agenda-files t t))
    (when (eq (car org-agenda-text-search-extra-files) 'agenda-archives)
      (pop org-agenda-text-search-extra-files))
    (setq files (append files org-agenda-text-search-extra-files))
    ;; Save buffers to make sure that grep can see latest changes.
    (let ((inhibit-message t)) (org-save-all-org-buffers))
    (dolist (file files)
      (when (file-exists-p file)
	;; Use -a switch to process UTF-16 files
	(let ((ans (shell-command-to-string (format "grep -anE '%s' '%s'" regexp file))))
          (unless (string-empty-p ans)
            (setq matches (append matches
				  (mapcar (lambda (str)
					    ;; Line number
                                            (when (string-match "^\\([0-9]+\\):" str)
                                              (let ((line-num (string-to-number (match-string 1 str))))
						(with-current-buffer (find-file-noselect file 'nowarn)
						  (save-excursion
						    (goto-line line-num)
                                                    (point-marker))))))
					  (s-lines ans))))))))
    (setq matches (remove nil matches))
    (when matches
      (unless dont-show-match-p
	(switch-to-buffer (marker-buffer (car matches)))
	(goto-char (car matches))
        (org-back-to-heading t)
	(org-show-entry))
      (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string matches) "\n") 'error))))

(defun org-capture-ref-check-regexp-search-view (regexp &optional dont-show-match-p)
  "Check if REGEXP exists in org files using `org-search-view'.
If DONT-SHOW-MATCH-P is non-nil, do not show the match or agenda search with all matches."
  (let ((org-agenda-sticky nil)
	(org-agenda-restrict nil))
    (org-search-view nil (format "{%s}" regexp)))
  (goto-char (point-min))
  (let (headlines)
    (while (< (point) (point-max))
      (when (get-text-property (point) 'org-hd-marker) (push (get-text-property (point) 'org-hd-marker) headlines))
      (goto-char (next-single-char-property-change (point) 'org-hd-marker)))
    (pcase (length headlines)
      (0 t)
      (1 (unless dont-show-match-p
	   (switch-to-buffer (marker-buffer (car headlines)))
	   (goto-char (car headlines))
           (if (functionp #'org-fold-reveal)
               (org-fold-reveal)
	     (org-reveal)))
         (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string headlines) "\n") 'error))
      (_ (when dont-show-match-p (kill-buffer))
         (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string headlines) "\n") 'error)))))

(defun org-capture-ref-check-key ()
  "Check if `:key' already exists.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (pcase org-capture-ref-check-key-method
    (`org-id-find
     (when-let ((mk (org-id-find (org-capture-ref-get-bibtex-field :key) 'marker)))
       (unless (org-capture-ref-get-capture-template-info :immediate-finish)
	 (switch-to-buffer (marker-buffer mk))
	 (goto-char mk)
	 (org-show-entry))
       (org-capture-ref-message (org-capture-ref-get-message-string mk) 'error)))
    (`grep
     (org-capture-ref-check-regexp-grep (format "^:ID:[ \t]+%s$" (regexp-quote (org-capture-ref-get-bibtex-field :key))) (org-capture-ref-get-capture-template-info :immediate-finish)))
    (_ (org-capture-ref-message (format "Invalid value of org-capture-ref-check-key-method: %s" org-capture-ref-check-key-method) 'error))))

(defun org-capture-ref-check-url ()
  "Check if `:url' already exists.
It is assumed that `:url' is captured into :SOURCE: property.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (org-capture-ref-check-regexp (format "^:Source:[ \t]+%s$" (regexp-quote (org-capture-ref-get-bibtex-field :url))) (org-capture-ref-get-capture-template-info :immediate-finish)))

(defun org-capture-ref-check-link ()
  "Check if captured `:link' already exists.
It is assumed that `:link' is captured into :SOURCE: property.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (org-capture-ref-check-regexp (format "^:Source:[ \t]+%s$" (regexp-quote (org-capture-ref-get-capture-info :link))) (org-capture-ref-get-capture-template-info :immediate-finish)))

;;; Internal variables

(defvar org-capture-ref--store-link-plist nil
  "A copy of `org-store-link-plist'.
  
The following keys are recognized by generic parser (though all
available keys can be accessed by user-defined parsers):
:link                 Captured link
:description          Page title, as given to `org-capture'
:query                Query provided to `org-protocol-capture'. The following special fields are recognized:
  :html               Path to html file containing the page. Providing
                      this will speed up processing since there will be no need to download
                      the link contents.
  :qutebrowser-fifo   Path to FIFO communicating with qutebrowser instance
  :elfeed-data        Elfeed entry containing the information about captured URL.")

(defvar org-capture-ref--bibtex-alist nil
  "Alist containing bibtex fields for the webpage being captured.
  
The fields include:
:type         - bibtex entry type
:key          - bibtex entry key
:author       - the author of the URL contents
:title        - title of the URL contents
:url          - cleaned-up URL
:year         - publication year
:urldate      - capture time
:journal      - journal name (for journal articles)
:howpublished - website name (for generic URLs)

Special field :bibtex-string contains formatted BiBTeX entry as a string.")

(defvar org-capture-ref--buffer nil
  "Buffer containing downloaded webpage being captured.")

(defvar org-capture-ref--buffer-dom nil
  "Parsed html of the captured page.")

;;; Main capturing routine

(defun org-capture-ref-reset-state ()
  "Refresh all the internal variables for fresh capture."
  (setq org-capture-ref--buffer nil
	org-capture-ref--buffer-dom nil
	org-capture-ref--bibtex-alist nil
        org-capture-ref--store-link-plist org-store-link-plist))

(defun org-capture-ref-clean-bibtex (string &optional no-hook)
  "Make sure that BiBTeX entry STRING is a valid BiBTeX.
Return the new entry string.

This runs `org-capture-ref-clean-bibtex-hook', unless NO-HOOK is non-nil."
  (with-temp-buffer
    (bibtex-mode)
    (bibtex-set-dialect 'BibTeX)
    (when string (insert string))
    (goto-char 1)
    (unless no-hook
      (run-hooks 'org-capture-ref-clean-bibtex-hook))
    (goto-char 1)
    (dolist (field (bibtex-parse-entry 'content))
      (pcase (intern (concat ":" (car field)))
	(':=type= (org-capture-ref-set-bibtex-field :type (cdr field)))
        (':=key= (org-capture-ref-set-bibtex-field :key (cdr field)))
        ;; Other fields may contain unwanted newlines.
        (key (org-capture-ref-set-bibtex-field key (replace-regexp-in-string "\n[ \t]*" " " (cdr field))))))
    (buffer-string)))

(defun org-capture-ref-format-bibtex ()
  "Return formatted BiBTeX string."
  (org-capture-ref-clean-bibtex (run-hook-with-args-until-success 'org-capture-ref-get-formatted-bibtex-functions)))

(defun org-capture-ref-get-bibtex ()
  "Parse the capture info and extract BiBTeX."
  (catch :finish
    (run-hooks 'org-capture-ref-get-bibtex-functions)))

(defun org-capture-ref-generate-key ()
  "Generate citation key.

The generated key will ideally be a fingerprint of the captured entry.
The same article/page should always get the same key (as much as it is
possible).

This calls `org-capture-ref-generate-key-functions'."
  (or (org-capture-ref-get-bibtex-field :key)
      (run-hook-with-args-until-success 'org-capture-ref-generate-key-functions)
      (org-capture-ref-message "Failed to generate BiBTeX key" 'error)))

(defun org-capture-ref-check-bibtex ()
  "Check if the entry is suitable for capture.

By default, we make sure that the key is unique, for example.

This runs `org-capture-ref-check-bibtex-functions'"
  (catch :finish
    (run-hooks 'org-capture-ref-check-bibtex-functions)))

(defun org-capture-ref-process-capture ()
  "Extract BiBTeX info from currently captured link and generate unique key.

The return value is always empty string, so that this function can be
used inside capture template."
  
  (unwind-protect
      (progn
	(org-capture-ref-reset-state)
	(org-capture-ref-message "Capturing BiBTeX...")
	(org-capture-ref-get-bibtex)
	(org-capture-ref-set-bibtex-field :key (org-capture-ref-generate-key))
	(org-capture-ref-set-bibtex-field :bibtex-string (org-capture-ref-format-bibtex))
	(org-capture-ref-check-bibtex)
	(org-capture-ref-message "Capturing BiBTeX... done"))
    (when (buffer-live-p org-capture-ref--buffer) (kill-buffer org-capture-ref--buffer)))
  "")

(provide 'org-capture-ref)
;;; org-capture-ref.el ends here
