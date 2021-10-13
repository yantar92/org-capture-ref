;; org-capture-ref.el --- Extract bibtex info from captured websites  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Version: 0.3
;; Package-Requires: ((s "1.12.0") (f "0.20.0") (asoc "0.6.1") (org "9.3") (org-ref "1.1.1") (doct "3.1.0"))
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
(require 'dash)
(require 's)
(require 'f)
(require 'asoc)
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

(defcustom org-capture-ref-get-bibtex-functions '( org-capture-ref-set-default-type
                                    org-capture-ref-set-access-date
                                    org-capture-ref-set-access-date-timestamp
                                    ;; Handle capturing from Emacs buffers
                                    org-capture-ref-get-bibtex-from-emacs-buffer
                                    ;; Pull generic data from capture
                                    org-capture-ref-get-bibtex-url-from-capture-data
				    org-capture-ref-get-bibtex-howpublished-from-url
                                    ;; Elfeed parsers
				    org-capture-ref-get-bibtex-from-elfeed-data
                                    ;; DOI retrieval
                                    org-capture-ref-get-bibtex-doi
                                    org-capture-ref-get-bibtex-aps
                                    org-capture-ref-get-bibtex-springer
                                    org-capture-ref-get-bibtex-wiley
                                    org-capture-ref-get-bibtex-tandfonline
                                    org-capture-ref-get-bibtex-ieee
                                    org-capture-ref-get-bibtex-semanticscholar
                                    org-capture-ref-get-bibtex-sciencedirect-article
                                    org-capture-ref-get-bibtex-sciencemag-careers-article
                                    org-capture-ref-get-bibtex-nature-careers-article
                                    org-capture-ref-get-bibtex-proquest
                                    org-capture-ref-get-bibtex-arxiv
                                    org-capture-ref-get-bibtex-ams-cn
                                    org-capture-ref-get-bibtex-ohiolink
                                    org-capture-ref-mark-links-with-known-absent-doi
                                    org-capture-ref-get-bibtex-from-first-doi
				    ;; Site-specific parsing
                                    org-capture-ref-get-bibtex-google-scholar-bibtex-page
                                    org-capture-ref-get-bibtex-wikipedia-book
                                    org-capture-ref-get-bibtex-wiki
                                    org-capture-ref-get-bibtex-goodreads
                                    org-capture-ref-get-bibtex-amazon
                                    org-capture-ref-get-bibtex-github-commit
                                    org-capture-ref-get-bibtex-github-issue
                                    org-capture-ref-get-bibtex-github-pull-request
                                    org-capture-ref-get-bibtex-github-repo
                                    org-capture-ref-get-bibtex-github-file
                                    org-capture-ref-get-bibtex-gitlab-commit
                                    org-capture-ref-get-bibtex-gitlab-repo
                                    org-capture-ref-get-bibtex-git-savannah-gnu-org-commit
                                    org-capture-ref-get-bibtex-gnu-org
                                    org-capture-ref-get-bibtex-nullprogram
                                    org-capture-ref-get-bibtex-srht-repo
                                    org-capture-ref-get-bibtex-reddit-wiki
                                    org-capture-ref-get-bibtex-reddit-planetemacs
                                    org-capture-ref-get-bibtex-reddit-comment
                                    org-capture-ref-get-bibtex-reddit
                                    org-capture-ref-get-bibtex-youtube-watch
                                    org-capture-ref-get-bibtex-youtube-channel
                                    org-capture-ref-get-bibtex-habr
                                    org-capture-ref-get-bibtex-weixin
                                    org-capture-ref-get-bibtex-samlib-book
                                    org-capture-ref-get-bibtex-fanfics-me
                                    org-capture-ref-get-bibtex-authortoday-reader
                                    org-capture-ref-get-bibtex-authortoday-work
                                    org-capture-ref-get-bibtex-authortoday-post
                                    org-capture-ref-get-bibtex-fantlab-author
                                    org-capture-ref-get-bibtex-fantlab-work
                                    org-capture-ref-get-bibtex-fantlab-edition
                                    org-capture-ref-get-bibtex-ficbook
                                    org-capture-ref-get-bibtex-lesswrong
                                    org-capture-ref-get-bibtex-archive-book
                                    org-capture-ref-get-bibtex-stallman
                                    org-capture-ref-get-bibtex-karl-voit
                                    org-capture-ref-get-bibtex-imdb-movie
                                    org-capture-ref-get-bibtex-orgmode-ml
                                    org-capture-ref-get-bibtex-list-gnu-ml
                                    org-capture-ref-get-bibtex-steam
                                    org-capture-ref-get-bibtex-google-books
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
                                               org-capture-ref-get-bibtex-nature-elfeed
					       org-capture-ref-get-bibtex-habr-elfeed
                                               org-capture-ref-get-bibtex-rgoswami-elfeed-fix-author
                                               org-capture-ref-get-bibtex-reddit-elfeed-fix-howpublished
                                               org-capture-ref-get-bibtex-ted-elfeed)
  "Functions used to generate BibTeX entry from elfeed entry data defined in `:elfeed-data' field of the `org-protocol' capture query.

This variable is only used if `org-capture-ref-get-bibtex-from-elfeed-data' is listed in `org-capture-ref-get-bibtex-functions'.
The functions must follow the same rules as `org-capture-ref-get-bibtex-functions', but will be called with a single argument - efleed entry object.

These functions will only be called if `:elfeed-data' field is present in `:query' field of the `org-store-link-plist'."
  :type 'hook
  :group 'org-capture-ref)
(defcustom org-capture-ref-get-bibtex-from-emacs-buffer-functions '(org-capture-ref-get-bibtex-bibtex-mode
                                                     org-capture-ref-get-bibtex-eaf-browser
                                                     org-capture-ref-get-bibtex-notmuch-show-mode
                                                     org-capture-ref-get-bibtex-notmuch-search-mode
                                                     org-capture-ref-get-bibtex-notmuch-tree-mode
                                                     org-capture-ref-update-bibtex-at-org-heading)
  "Functions used to generate BibTeX entry from Emacs buffer at point defined in `:buffer-marker' field of the `org-protocol' capture query.

This variable is only used if `org-capture-ref-get-bibtex-from-emacs-buffer' is listed in `org-capture-ref-get-bibtex-functions'.
The functions must follow the same rules as `org-capture-ref-get-bibtex-functions' and will be called with point at location to be captured.

These functions will only be called if `:buffer-marker' field is present in `:query' field of the `org-store-link-plist'."
  :type 'hook
  :group 'org-capture-ref)
(defcustom org-capture-ref-clean-bibtex-hook '(org-capture-ref-create-key-maybe
                                org-capture-ref-remove-double-comma
                                org-ref-bibtex-format-url-if-doi
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
                                org-capture-ref-replace-%
                                org-capture-ref-remove-garbage-symbols-from-authors
                                org-capture-ref-capitalize-author)
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

(defcustom org-capture-ref-generate-key-functions '(org-capture-ref-generate-key-human-readable)
  "Functions used to generate citation key if it is not yet present.
The functions will be called in sequence until any of them returns non-nil value."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-check-bibtex-functions '(org-capture-ref-check-key
                                     org-capture-ref-check-doi
				     org-capture-ref-check-url
				     org-capture-ref-check-link
                                     org-capture-ref-check-article-title
                                     org-capture-ref-check-author-title)
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

(defcustom org-capture-ref-headline-format-function #'org-capture-ref-headline-format
  "Function with no arguments returning the headline text for `org-capture-ref-get-org-entry'."
  :type 'function
  :group 'org-capture-ref)

(defcustom org-capture-ref-headline-tags '("BOOKMARK" :type :typealt)
  "List of tags to be added to org entry in `org-capture-ref-get-org-entry'.
Each element of the list can be either a string representing the tag
or a symbol representing the metadata to be used as a tag."
  :type '(repeat (choice string symbol))
  :group 'org-capture-ref)

(defcustom org-capture-ref-capture-target `(:file ,(f-join org-directory "inbox.org"))
  "Capture template target specification to be used in `org-capture-ref-capture-template'.
The specification will be inserted as is into a `doct' template. See
Target section of the `doct' docstring for details."
  :type 'sexp
  :group 'org-capture-ref)

(defcustom org-capture-ref-capture-keys '("z" "Z")
  "List of the capture template keys to be used for interactive and silent templates respectively."
  :type '(list string string)
  :group 'org-capture-ref)

(defcustom org-capture-ref-capture-template `( :group "org-capture-ref template"
 			        :type entry
                                ,@org-capture-ref-capture-target
 			        :fetch-bibtex (lambda () (org-capture-ref-process-capture)) ; this must run first
                                :link-type (lambda () (org-capture-ref-get-bibtex-field :type))
                                :extra (lambda () (if (org-capture-ref-get-bibtex-field :journal)
					         (s-join "\n"
						         '("- [ ] download and attach pdf"
						           "- [ ] [[elisp:org-attach-open][read paper capturing interesting references]]"
						           "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.semanticscholar.org/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check citing articles]]"
						           "- [ ] [[elisp:(browse-url (url-encode-url (format \"https://www.connectedpapers.com/search?q=%s\" (org-entry-get nil \"TITLE\"))))][check related articles]]"
                                                           "- [ ] check if bibtex entry has missing fields"
                                                           "- [ ] Consider subscribing to new citations"))
                                               ""))
                                :org-entry (lambda () (org-capture-ref-get-org-entry))
			        :template
                                ("%{fetch-bibtex}* TODO %?%{space}%{org-entry}"
                                 "%{extra}")
			        :children (("Interactive org-capture-ref template"
					    :keys ,(car org-capture-ref-capture-keys)
                                            :space " ")
				           ("Silent org-capture-ref template"
					    :keys ,(cadr org-capture-ref-capture-keys)
                                            :space ""
					    :immediate-finish t)))
  "Default capture template. The template is a template defined using
  `doct' syntax. See docstring of `doct' for details."
  :type 'list
  :group 'org-capture-ref)

;;;; Customisation for default functions

(defcustom org-capture-ref-field-rules `((:doi . ("scheme=\"doi\" content=\"\\([^\"]*?\\)\""
				   "citation_doi\" content=\"\\([^\"]*?\\)\""
				   "data-doi=\"\\([^\"]*?\\)\""
				   "content=\"\\([^\"]*?\\)\" name=\"citation_doi"
				   "objectDOI\" : \"\\([^\"]*?\\)\""
				   "doi = '\\([^']*?\\)'"))
                          (:journal . ((:meta "citation_journal_title")))
                          (:year . ((:meta "citation_publication_date" :apply ,#'org-capture-ref-extract-year-from-string)
                                    "class=\\(?:date.[^>]*?\\)>[^<]*?\\([0-9]\\{4\\}\\)[^<]*?</"))
                          (:author . ((:meta "author")
                                      (:meta "citation_author")
				      "\\(?:<[^>]*?class=\"author[^\"]*name\"[^>]*>\\([^<]+\\)<\\)"))
                          (:title . ((:meta "citation_title")
                                     "<title.?+?>\\([[:ascii:][:nonascii:]]*?\\|.+\\)</title>")))
  "Alist holding rules used by `org-capture-ref-parse-generic' to populate common BiBTeX fields from html.
Keys of the alist are the field names (example: `:author') and the values are lists of regexps or `org-capture-ref-query-dom' rules.
The regexps are searched one by one in the html buffer and the group 1 match is used as value in the BiBTeX field."
  :group 'org-capture-ref
  :type '(alist :key-type symbol :value-type (set string (set symbol string))))

(defcustom org-capture-ref-demand-doi-list '("aps\\.org"
                              "nature\\.com"
                              "aip\\.scitation\\.org"
                              "worldscientific\\.com"
                              "cambridge\\.org")
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
      typealt     = {${:typealt}},
      author       = {${:author}},
      title        = {${:title}},
      journal      = {${:journal}},
      school      = {${:school}},
      volume       = {${:volume}},
      number       = {${:number}},
      pages        = {${:pages}},
      year         = {${:year}},
      doi          = {${:doi}},
      isbn          = {${:isbn}},
      url          = {${:url}},
      howpublished = {${:howpublished}},
      publisher = {${:publisher}},
      keywords     = {${:keywords}},
      note         = {Online; accessed ${:urldate}},
      created         = {${:created}},
      effort       = {${:effort}},
      link         = {${:link}}
      }"
  "Default template used to format BiBTeX entry.
If a keyword from the template is missing, it will remain empty."
  :type 'string
  :group 'org-capture-ref)

(defcustom org-capture-ref-use-journal-abbreviations t
  "Shorten journal/howpublished names in `org-capture-ref-headline-format'.
The full names are replaced in the hadline by
`org-capture-ref-journal-abbreviations' or
`org-ref-bibtex-journal-abbreviations'."
  :type 'boolean
  :group 'org-capture-ref)

(defcustom org-capture-ref-journal-abbreviations '(("Nature Materials" . "NatureMat")
                                    ("Annual Review of Materials Research" . "AnnRevMatRes")
                                    ("Transactions of the Japan Institute of Metals" . "JIM")
                                    ("Physical Review Materials" . "PRMat")
                                    ("Physical Review Letters" . "PRL")
                                    ("Materials Science and Engineering: A" . "MSEA")
                                    ("Acta Materialia" . "ActaMat")
                                    ("Scripta Materialia" . "ScriptaMat")
                                    ("Journal of Materials Research" . "JMR")
                                    ("Advanced Engineering Materials" . "AdvEngMat")
                                    ("Philosophical Magazine" . "PhilMag")
                                    ("International Journal of Plasticity" . "IJP"))
  "List of personal journal abbreviations.  See `org-capture-ref-use-journal-abbreviations'."
  :type '(list (cons string string))
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

(defcustom org-capture-ref-check-link-regexp '((org-search-view . "^:\\(Source|URL\\+?\\):[ \t[]+%s[]]*$")
                                (grep . "^:(Source|URL\\+?):[ \t[]+%s[]]*$"))
  "Regexp used to match the captured link against existing headlines.
`%s' is replaced by the url.
The value must be an alist of `org-capture-ref-check-regexp-method' and the corresponding regexp.")

(defcustom org-capture-ref-collection-types '("playlist"
                               "author"
                               "proceedings"
                               "book"
                               "bookseries"
                               "collection")
  "List of bibtex item types (or typealt) that can contain subordinate
  items."
  :type '(list string)
  :group 'org-capture-ref)

(defcustom org-capture-ref-fetch-collections t
  "Whe non-nil and `org-capture-ref-capture-template-set-p' is set, try
capture items from currently captured URL with :type or :typealt
from `org-capture-ref-collection-types'"
  :type 'boolean
  :group 'org-capture-ref)

(defcustom org-capture-ref-fetch-collection-functions '( org-capture-ref-capture-collection-youtube)
  "Functions used to capture contents of the current collection.

Each function will be called without arguments in sequence.
Each function is expected to call relevant capture functions like
`org-capture-ref-capture-url' with side-effects."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-warn-when-using-generic-parser t
  "Non-nil means warn user if some fields are trying to be parsed using generic parser.
`debug' means show all the details."
  :type 'boolean
  :group 'org-capture-ref)

(defcustom org-capture-ref-quiet-verbosity t
  "Show less messages.")

;;; API

(defmacro org-capture-ref-unless-set (fields &rest body)
  "Run BODY unless all BiBTeX FIELDS are set."
  (declare (debug (sexp body)) (indent 1))
  `(unless (-all-p (lambda (key)
		     (org-capture-ref-get-bibtex-field key 'consider-placeholder))
		   ,fields)
     ,@body))

(defun org-capture-ref-set-new-url (url)
  "Reset environment as if capture was invoked for URL."
  (org-capture-ref-reset-state)
  (org-capture-ref-set-bibtex-field :url url)
  ;; Asquire the new URL.
  (org-capture-ref-set-capture-info :link (org-capture-ref-get-bibtex-field :url))
  (let ((org-capture-ref-get-buffer-functions '(org-capture-ref-retrieve-url)))
    (org-capture-ref-get-buffer 'force)))

(defun org-capture-ref-get-buffer (&optional force)
  "Return buffer containing contents of the captured link.

Retrieve the contents first if necessary or if FORCE is non-nil.
This calls `org-capture-ref-get-buffer-functions'."
  (let ((buffer (or (and (not force) org-capture-ref--buffer)
		    (run-hook-with-args-until-success 'org-capture-ref-get-buffer-functions))))
    (unless (buffer-live-p buffer) (org-capture-ref-message (format "<org-capture-ref> Failed to get live link buffer. Got %s" buffer) 'error))
    (setq org-capture-ref--buffer-dom nil)
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward "Checking your browser before accessing" nil t)
          (org-capture-ref-message "URL is behind cloudflare firewall. Try to refresh/open the page in browser to whilelist current IP" 'error))))
    (setq org-capture-ref--buffer buffer)))

(defun org-capture-ref-get-dom ()
  "Return parsed html of the captured link."
  (or org-capture-ref--buffer-dom
      (setq org-capture-ref--buffer-dom (with-current-buffer (org-capture-ref-get-buffer)
			   (libxml-parse-html-region (point-min) (point-max))))))

(defun org-capture-ref-query-dom (&rest query)
  "Query a dom element text from the website.
QUERY format:
:dom|:return-dom|:tag|:class|:id|:attr|:join|:meta|:apply value [:tag|:class|:id|:attr|:join value|:apply]...
Value is a symbol, regexp, or regexp when matching for tag,
class, or id respectively.
Value can be either a symbol or a cons (symbol . string) for :attr. If
value is a symbol, return the value of attribute represented by that
symbol. If value is the cons search dom elements with attribute value
equal to the strin in the cons.
:join sets a string to join multiple match. \" \" by default.
:dom sets dom to parse (default: org-capture-ref-get-dom).
:return-dom forces return value to be a DOM element instead of string when non-nil.
:meta runs query to html metadata. All other query fields (except
:join) are ignored then. :meta must be the first symbol in the query.
:apply applies provided function symbol to the result of preceding query."
  (save-match-data
    (let ((dom (if (eq ':dom (car query))
                   (prog1 (cadr query)
                     (setq query (cddr query)))
                 (org-capture-ref-get-dom)))
          (return-dom (and (eq (car query) ':dom)
                           (prog1 (cadr query)
                             (setq query (cddr query)))))
          (separator " "))
      (while query
        (unless (or (stringp dom)
                    (stringp (car dom))
                    (listp (car dom)))
          (setq dom (list dom)))
        (setq dom
              (pcase (car query)
                (:apply
                 (prog1 (funcall (cadr query) dom)
                   (setq query (cddr query))))
                (:meta
                 (prog1 (org-capture-ref-query-meta (cadr query) (or (plist-get query :join) separator))
                   (setq query (cddr query))))
                (:tag
                 (prog1 (-flatten-n 1 (mapcar (lambda (dom) (dom-by-tag dom (cadr query))) dom))
                   (setq query (cddr query))))
                (:class
                 (prog1 (-flatten-n 1 (mapcar (lambda (dom) (dom-by-class dom (cadr query))) dom))
                   (setq query (cddr query))))
                (:id
                 (prog1 (-flatten-n 1 (mapcar (lambda (dom) (dom-by-id dom (cadr query))) dom))
                   (setq query (cddr query))))
                (:attr
                 (pcase (cadr query)
                   ((and (pred consp)
                         (app car name)
                         (app cdr value))
                    (prog1 (-flatten-n 1 (mapcar (lambda (dom) (dom-search dom (lambda (node) (string= value (dom-attr node name))))) dom))
                      (setq query (cddr query))))
                   ((pred symbolp)
                    (prog1 (-flatten-n 1 (mapcar (lambda (dom) (dom-attr dom (cadr query))) dom))
                      (setq query (cddr query))))
                   (_ (error "Invalid :attr query: %s" (cadr query)))))
                (:join
                 (prog1 dom
                   (setq separator (cadr query))
                   (setq query (cddr query))))
                (_ (error "Invalid query: %s" query)))))
      (if return-dom
          dom
        (decode-coding-string
         (if (stringp dom)
             dom
           (unless (and (listp dom) (or (listp (car dom)) (stringp (car dom)))) (setq dom (list dom)))
           (if (stringp (car dom))
               (s-join separator (mapcar #'s-trim (delete-if #'string-empty-p dom)))
             (s-join separator (mapcar #'s-trim (delete-if #'string-empty-p (mapcar #'dom-texts dom))))))
         'utf-8)))))

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
                     (mapcar (lambda (node) (replace-regexp-in-string " +" " " (dom-attr node 'content)))
                             (dom-search (org-capture-ref-get-dom)
                                         (lambda (node)
                                           (and (eq (car node) 'meta)
                                                (or (string= key (dom-attr node 'property))
                                                    (string= key (dom-attr node 'itemprop)) ;; i.e. for Youtube video duration
                                                    (string= key (dom-attr node 'name))))))))))
    (if (string-empty-p ans) nil ans)))

(defun org-capture-ref-extract-year-from-string (string-or-dom)
  "Extract year from date string or DOM element."
  (let ((string (cond
                 ((stringp string-or-dom) string-or-dom)
                 ((stringp (car string-or-dom)) (mapconcat #'identity string-or-dom ""))
                 (t (dom-texts string-or-dom)))))
    (when (and string (string-match "[0-9]\\{4\\}" string))
      (match-string 0 string))))

(defun org-capture-ref-parse-timestamp (time)
  "Parse ISO8601 timestamp string.

See https://en.wikipedia.org/wiki/ISO_8601.

ISO8601 is, for example, used in Youtube video duration."
  (when (string-match "^PT\\(?:\\([0-9]+\\)H\\)?\\(?:\\([0-9]+\\)M\\)?\\(?:\\([0-9]+\\)S\\)?" time)
    (let ((hours (or (and (match-string 1 time)
                          (string-to-number (match-string 1 time)))
                     0))
          (minutes (or (and (match-string 2 time)
                            (string-to-number (match-string 2 time)))
                       0))
          (seconds (or (and (match-string 3 time)
                            (string-to-number (match-string 3 time)))
                       0)))
      (when (>= minutes 60)
        (cl-incf hours (floor (/ minutes 60)))
        (setq minutes (% minutes 60)))
      (when (> seconds 0) (cl-incf minutes))
      ;; We are interested in hours and
      ;; minuts. Drop seconds.
      (format "%.2d:%.2d" hours minutes))))

(defun org-capture-ref-get-bibtex-field (field &optional return-placeholder-p)
  "Return the value of the BiBTeX FIELD or nil the FIELD is not set.
Unless RETURN-PLACEHOLDER-P is non-nil, return nil when the value is equal
to `org-capture-ref-placeholder-value'.
  
FIELD must be a symbol like `:author'.
See `org-capture-ref--bibtex-alist' for common field names."
  (if return-placeholder-p
      (alist-get field org-capture-ref--bibtex-alist)
    (let ((res (alist-get field org-capture-ref--bibtex-alist)))
      (unless (and (stringp res) (string-equal res org-capture-ref-placeholder-value)) res))))

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
  (when (stringp val) (setq val (s-trim val)))
  (unless (and (not force) (or (and (stringp val) (string-empty-p val)) (not val)))
    (setf (alist-get field org-capture-ref--bibtex-alist) val)))

(defun org-capture-ref-set-capture-info (key val)
  "Set KEY in capture info to VAL.
  
The KEY set here will be passed down to org-capture via
`org-store-link-plist'.
See docstring of `org-capture-ref--store-link-plist' for possible KEYs."
  (plist-put org-capture-ref--store-link-plist key val))

;;; Predefined functions

;;;; Getting html buffer

(defun org-capture-ref-get-buffer-from-html-file-in-query ()
  "Use buffer from file defined in `:html' field of `org-protocol' query."
  (let* ((html (org-capture-ref-get-capture-info '(:query :html))))
    (when html
      (let ((coding-system-for-read 'utf-8))
        (let ((auto-mode-alist '((".+" . fundamental-mode))))
          (find-file-noselect html))))))

(defun org-capture-ref-retrieve-url ()
  "Retrieve html buffer from `:link' field of capture data."
  (let ((link (or (org-capture-ref-get-capture-info :link)
                  (org-capture-ref-get-bibtex-field :url))))
    (when link
      (url-retrieve-synchronously link))))

;;;; Getting bibtex using known web APIs
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
        (let ((bibtex (org-capture-ref-query-dom :dom (libxml-parse-html-region (point-min) (point-max)) :tag 'textarea)))
          (if (seq-empty-p bibtex)
              (error "ISBN record %s not found" isbn)
            bibtex)))))))

;; Getting BiBTeX

(defun org-capture-ref-get-bibtex-from-emacs-buffer ()
  "Run `org-capture-ref-get-bibtex-from-emacs-buffer-functions'.'"
  (let ((mk (org-capture-ref-get-capture-info '(:query :buffer-marker))))
    (when mk
      (org-with-point-at mk
        (run-hooks 'org-capture-ref-get-bibtex-from-emacs-buffer-functions)))))
(defun org-capture-ref-get-bibtex-from-elfeed-data ()
  "Run `org-capture-ref-get-bibtex-from-elfeed-functions'."
  (let ((elfeed-entry (org-capture-ref-get-capture-info '(:query :elfeed-data))))
    (when elfeed-entry
      (require 'elfeed)
      (run-hook-with-args 'org-capture-ref-get-bibtex-from-elfeed-functions elfeed-entry))))

(defun org-capture-ref-parse-opengraph ()
  "Generic parser for websites supporting OpenGraph protocol.

See https://ogp.me/ for details."
  (org-capture-ref-unless-set '(:title :url :howpublished)
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
Sets BiBTeX fields according to `org-capture-ref-field-rules'.
Existing BiBTeX fields are not modified."
  ;; Do not bother is everything is already set.
  (org-capture-ref-unless-set (mapcar #'car org-capture-ref-field-rules)
    (when org-capture-ref-warn-when-using-generic-parser
      (org-capture-ref-message "Capturing using generic parser..." 'warning))
    (when (org-capture-ref-get-buffer)
      (with-current-buffer (org-capture-ref-get-buffer)
        (cl-loop for (key . rules) in org-capture-ref-field-rules do
                 (unless (org-capture-ref-get-bibtex-field key 'consider-placeholder)
                   (when (eq org-capture-ref-warn-when-using-generic-parser 'debug)
	             (org-capture-ref-message (format "Capturing using generic parser... searching %s..." key)))
                   (cl-loop for rule in rules do
                            (pcase rule
                              ((pred listp)
                               (let ((val (apply #'org-capture-ref-query-dom rule)))
                                 (unless (string-empty-p val)
                                   (org-capture-ref-set-bibtex-field key val)
                                   (cl-return))))
                              ((pred stringp)
	                       (goto-char (point-min))
	                       (when (re-search-forward rule nil t)
		                 (org-capture-ref-set-bibtex-field key
                                                    (decode-coding-string (match-string 1)
                                                                          (or (get-char-property 0 'charset (match-string 1))
                                                                              'utf-8)))
		                 (cl-return)))
                              (_ (error "Invalid `org-capture-ref-field-rules' rule: %s" rule))))
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
                               "author.today"
                               "wikipedia.org"
                               "app.dimensions.ai"
                               "scholar.google.com"
                               "gwern.net"
                               "habr.com"))
                 (or (org-capture-ref-get-capture-info :link)
                     (org-capture-ref-get-bibtex-field :url)
                     ""))
    (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)))

(defvar org-capture-ref--doi-record-cache (make-hash-table :test #'equal)
  "Hash table storing downloaded DOI records.")

(defun org-capture-ref-get-bibtex-from-first-doi ()
  "Generate BiBTeX using first DOI record found in html or `:doi' field.
Use `doi-utils-doi-to-bibtex-string' to retrieve the BiBTeX record.
Return nil if DOI record is not found."
  (when (and (not (org-capture-ref-get-bibtex-field :doi 'consider-placeholder))
	     (alist-get :doi org-capture-ref-field-rules))
    (let ((org-capture-ref-field-rules (list (assq :doi org-capture-ref-field-rules)))
	  org-capture-ref-warn-when-using-generic-parser)
      (org-capture-ref-parse-generic)))
  (let ((doi (org-capture-ref-get-bibtex-field :doi)))
    (when doi
      (org-capture-ref-message (format "Retrieving DOI record %s ..." doi))
      (let ((bibtex-string (or (gethash doi org-capture-ref--doi-record-cache)
                               (condition-case err
		                   ;; Ignore errors and avoid opening the DOI url.
		                   (cl-letf (((symbol-function 'browse-url) #'ignore))
		                     (doi-utils-doi-to-bibtex-string doi))
                                 (t nil)))))
        (when (and bibtex-string (string-match "^[^ ]+ not supported yet.*" bibtex-string))
          (org-capture-ref-message (format "Retrieving DOI record %s ... failed. %S" doi (match-string 0 bibtex-string)) 'error))
        (if (not bibtex-string)
            (prog1 nil
              (if (-any-p (lambda (regexp) (s-match regexp (org-capture-ref-get-bibtex-field :url))) org-capture-ref-demand-doi-list)
                  (org-capture-ref-message (format "Retrieving DOI record %s ... failed, but demanded for %s" doi (org-capture-ref-get-bibtex-field :url)) 'error)
                (org-capture-ref-message (format "Retrieving DOI record %s ... failed. Proceeding with fallback options." doi) 'warning)))
          (unless org-capture-ref-quiet-verbosity (org-capture-ref-message "Retrieving DOI record... done"))
          (puthash doi bibtex-string org-capture-ref--doi-record-cache)
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
            (org-capture-ref-message (format "Retrieving ISBN record %s ... failed. Proceeding with fallback options." isbn) 'warning)
          (unless org-capture-ref-quiet-verbosity (org-capture-ref-message "Retrieving ISBN record... done"))
	  (org-capture-ref-clean-bibtex bibtex-string)
          ;; The provided keys are sometimes not unique.
          (org-capture-ref-set-bibtex-field :key nil 'force)
          (throw :finish t))))))

(defun org-capture-ref-get-bibtex-url-from-capture-data ()
  "Get the `:url' using :link data from capture."
  (when-let ((url (org-capture-ref-get-capture-info :link)))
    (when (string-match "/#[^/]*$" url)
      (setq url (replace-match "" nil nil url)))
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

(defun org-capture-ref-set-access-date-timestamp ()
  "Set `:created' field of the BiBTeX entry to now.
The value will be inactive org timestamp."
  (let ((stamp (with-temp-buffer
                 (org-insert-time-stamp (current-time) t t)
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (org-capture-ref-set-bibtex-field :created stamp)))
;;;; Capturing collections (playlist, author pages, book seried, etc)
(defun org-capture-ref-fetch-collection-maybe ()
  "Capture additional entries belonging to current entry that is from
`org-capture-ref-collection-types'.

This does nothing when `org-capture-ref-capture-template-set-p' is nil."
  (when (and org-capture-ref-capture-template-set-p
             org-capture-ref-fetch-collections
             (not org-note-abort)
             (or (member (org-capture-ref-get-bibtex-field :type) org-capture-ref-collection-types)
                 (member (org-capture-ref-get-bibtex-field :typealt) org-capture-ref-collection-types)))
    (run-hooks 'org-capture-ref-fetch-collection-functions)))

(defun org-capture-ref-capture-collection-youtube ()
  "Capture a series of URLs from a list of channel videos."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "youtube\\.com/\\(?:c\\|channel\\|user\\)/[^/]+\\(/videos\\|/featured\\)?" link)
      (unless (string= "/videos" (match-string 1 link))
        (if (match-string 1 link)
            (setq link (replace-match "/videos" nil nil link 1))
          (setq link (concat link "/videos")))
        (org-capture-ref-set-new-url link))
      (let ((urls (mapcar
                   (-partial #'concat "https://www.youtube.com")
                   (s-split "\n" (org-capture-ref-query-dom :join "\n" :class "ytd-grid-renderer" :id "video-title" :attr 'href)))))
        (when (equal (list "https://www.youtube.com") urls)
          (org-capture-ref-message "Failed to parse Youtube page. Is it not captured from browser?" :error))
        (mapc (lambda (url)
                (ignore-errors
                  (org-capture-ref-capture-url url)))
              urls)))))

;;;; Capturing individual web-pages
(defun org-capture-ref-get-bibtex-weixin ()
  "Parse BiBTeX for Wechat article."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "mp\\.weixin\\.qq\\.com" link)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "\\(sn=[^&]+\\).*$" "\\1" link))
      (org-capture-ref-set-bibtex-field :howpublished "Wechat")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "rich_media_title"))
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "rich_media_meta rich_media_meta_nickname" :tag 'a))
      (with-current-buffer (org-capture-ref-get-buffer)
	(goto-char (point-min))
        (when (re-search-forward "=\"\\([0-9]\\{4\\}\\)-[0-9]\\{2\\}-[0-9]\\{2\\}\"")
          (org-capture-ref-set-bibtex-field :year (match-string 1)))))))

(defun org-capture-ref-get-bibtex-wiki ()
  "Parse Wikipedia page and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:https?://\\)?\\([^.]+\\)\\.wikipedia\\.org" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :title (replace-regexp-in-string " +- +Wikipedia" "" (org-capture-ref-get-capture-info :description)))
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :howpublished (format "Wikipedia(%s)" (match-string 1 link)))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-reddit-planetemacs ()
  "Parse Planetemacs subreddit link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:old\\.\\)?\\(?:reddit\\.com\\|libredd\\.it\\)\\(?:/r/\\(planetemacs\\)\\)" link)
      (when-let ((url (org-capture-ref-query-dom :class "top-matter" :class "title" :attr 'href)))
        (unless (string-empty-p url)
          (org-capture-ref-set-new-url url)
          (org-capture-ref-get-bibtex)
          (throw :finish t))))))
(defun org-capture-ref-get-bibtex-reddit ()
  "Parse reddit link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:old\\.\\)?\\(?:reddit\\.com\\|libredd\\.it\\)\\(?:/r/\\([^/]+\\)\\(/[^/]+\\)?\\)?" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "old\\.reddit\\.com" "reddit.com" link))
      (if (match-string 1 link)
	  (org-capture-ref-unless-set '(:title :howpublished :author)
            (org-capture-ref-set-bibtex-field :howpublished (format "Reddit:%s" (match-string 1 link)))
            (when (match-string 2 link)
              (org-capture-ref-set-bibtex-field :author (format "/u/%s" (org-capture-ref-query-dom :class "content" :class "top-matter" :class "author")))
              (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "content" :class "tagline" :tag 'time :attr 'datetime :apply #'org-capture-ref-extract-year-from-string)))
            (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'og:title)))
	(org-capture-ref-set-bibtex-field :howpublished "Reddit"))
      ;; Generic parser works ok.
      (let (org-capture-ref-warn-when-using-generic-parser)
	(org-capture-ref-parse-generic))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-reddit-comment ()
  "Parse reddit comment link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:old\\.\\)?\\(?:reddit\\.com\\|libredd\\.it\\)\\(?:/r/\\([^/]+\\)\\)/comments/[^/]+/[^/]+/\\([^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "old\\.reddit\\.com" "reddit.com" link))
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "libredd\\.it" "reddit.com" (org-capture-ref-get-bibtex-field :url)))
      (if (match-string 1 link)
	  (progn
            (org-capture-ref-set-bibtex-field :howpublished (format "Reddit:%s" (match-string 1 link)))
            (org-capture-ref-set-bibtex-field :title
                               (format "Comment in %s" (org-capture-ref-query-dom :tag 'title))))
	(org-capture-ref-set-bibtex-field :howpublished "Reddit"))
      (when-let ((id (match-string 2 link)))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :id id :class "author" :apply #'car))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :id id :tag 'time :attr 'datetime :apply #'car :apply #'org-capture-ref-extract-year-from-string)))
      ;; Generic parser works ok.
      (let (org-capture-ref-warn-when-using-generic-parser)
	(org-capture-ref-parse-generic))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-reddit-wiki ()
  "Parse reddit wiki link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(?:old\\.\\)?\\(?:reddit\\.com\\|libredd\\.it\\)/r/\\([^/]+\\)/wiki" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "old\\.reddit\\.com" "reddit.com" link))
      (when (match-string 1 link)
	(org-capture-ref-unless-set '(:title :howpublished :author)
          (org-capture-ref-set-bibtex-field :howpublished (format "Reddit:%s" (match-string 1 link)))
          (org-capture-ref-set-bibtex-field :author org-capture-ref-placeholder-value)
          (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
          (org-capture-ref-set-bibtex-field :title "Wiki")))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-github-commit ()
  "Parse Github comit page and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com/\\(.+\\)/commit/\\([0-9a-z]+\\)" link)
      (let ((commit-number (match-string 2 link))
            (commit-repo (match-string 1 link)))
        (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
        ;; Find author
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "^commit-author user-mention$"))
        (org-capture-ref-set-bibtex-field :title  (format "Commit(%s): %s"
                                           (s-truncate 10 commit-number)
                                           (org-capture-ref-query-dom :class "commit-title")))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'relative-time :attr 'datetime :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :howpublished (format "Github:%s" commit-repo))
        (throw :finish t)))))

(defun org-capture-ref-get-bibtex-github-issue ()
  "Parse Github issue page and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com/\\(.+\\)/issues/\\([0-9]+\\)\\(?:#\\(issuecomment-[0-9]+\\)\\)?" link)
      (if (not (match-string 3 link))
          ;; Just capture the whole issue
          (let ((issue-number (match-string 2 link))
                (issue-repo (match-string 1 link)))
            (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
            ;; Find author
            (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "gh-header-meta" :class "author"))
            (org-capture-ref-set-bibtex-field :title  (format "issue#%s: %s" issue-number (org-capture-ref-query-dom :class "gh-header-title" :class "js-issue-title")))
            (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'relative-time :attr 'datetime :apply #'org-capture-ref-extract-year-from-string))
            (org-capture-ref-set-bibtex-field :howpublished (format "Github:%s" issue-repo))
            (throw :finish t))
        ;; Capture comment
        (let ((issue-number (match-string 2 link))
              (issue-repo (match-string 1 link))
              (comment-id (match-string 3 link)))
          (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
          ;; Find author
          (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :id comment-id :class "author"))
          (org-capture-ref-set-bibtex-field :title (format "comment on issue#%s: %s" issue-number (s-truncate fill-column (car (s-lines (org-capture-ref-query-dom :id comment-id :class "comment-body"))))))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :id comment-id :class "timestamp" :tag 'relative-time :attr 'datetime :apply #'car :apply #'org-capture-ref-extract-year-from-string))
          (org-capture-ref-set-bibtex-field :howpublished (format "Github:%s" issue-repo))
          (throw :finish t))
        ))))

(defun org-capture-ref-get-bibtex-github-repo ()
  "Parse Github repo link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com/[^/]+/[^/]+/?\\(?:tree/[a-z0-9]+\\)?$" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      ;; Find author
      (unless (org-capture-ref-get-bibtex-field :author 'consider-placeholder)
	(when (string-match "\\(?:https://\\)?git\\(?:hub\\|lab\\)\\.com/\\([^/]+\\)" link)
          (org-capture-ref-set-bibtex-field :author (match-string 1 link))))
      (org-capture-ref-unless-set '(:title)
        (org-capture-ref-set-bibtex-field :title (replace-regexp-in-string "Github +- +" "" (org-capture-ref-query-opengraph 'title)))
	;; Year has no meaning for repo
	(org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
        (org-capture-ref-set-bibtex-field :howpublished "Github")))))

(defun org-capture-ref-get-bibtex-srht-repo ()
  "Parse git.sr.ht repo link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "git\\.sr\\.ht/~\\([^/]+\\)/\\([^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (match-string 1 link))
      (org-capture-ref-set-bibtex-field :title (format "%s: %s" (match-string 2 link) (org-capture-ref-query-dom :class "header-extension" :class "container")))
      ;; Year has no meaning for repo
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :howpublished "Sourcehut")
      (throw :finish t))
    (when (string-match "sr\\.ht/~\\([^/]+\\)/\\([^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (match-string 1 link))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :tag 'head :tag 'title))
      ;; Year has no meaning for repo
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :howpublished "Sourcehut"))))

(defun org-capture-ref-get-bibtex-github-file ()
  "Parse Github file page and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com/\\([^/]+\\)/\\([^/]+\\)/blob/\\([^/]+\\)/\\(.+\\)" link)
      (let ((user (match-string 1 link))
            (repo (match-string 2 link))
            (branch (match-string 3 link))
            (file (match-string 4 link)))
        (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "gh-header-meta" :class "author"))
        (org-capture-ref-set-bibtex-field :title  (format "%s:%s" branch file))
        (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
        (org-capture-ref-set-bibtex-field :howpublished (format "Github:%s/%s" user repo))
        (throw :finish t)))))

(defun org-capture-ref-get-bibtex-github-pull-request ()
  "Parse Github pull request link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com/\\(.+\\)/pull/\\([0-9]+\\)" link)
      (let ((pull-number (match-string 2 link))
            (pull-repo (match-string 1 link))
            (pull-status (let ((status-string (org-capture-ref-query-dom :class "State" :attr 'title)))
                           (string-match "Status:[^:]+: \\(.+\\)" status-string)
                           (match-string 1 status-string))))
        (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
        ;; Find author (first comment)
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :id "partial-discussion-header" :class "author" :apply #'car))
        (org-capture-ref-set-bibtex-field :title (format "pull#%s: [%s] %s" pull-number pull-status (org-capture-ref-query-dom :class "gh-header-title" :class "^js-issue-title")))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'relative-time :attr 'datetime :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :howpublished (format "Github:%s" pull-repo))
        (throw :finish t)))))

(defun org-capture-ref-get-bibtex-gitlab-repo ()
  "Parse GitLab repo link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "gitlab\\.com/\\([^/]+\\)/\\([^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      ;; Find author
      (org-capture-ref-unless-set '(:author)
        (org-capture-ref-set-bibtex-field :author (match-string 1 link)))
      (org-capture-ref-unless-set '(:title)
        (org-capture-ref-set-bibtex-field :title (format "%s/%s: %s" (match-string 1 link) (match-string 2 link) (org-capture-ref-query-dom :meta 'og:description)))
	;; Year has no meaning for repo
	(org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
        (org-capture-ref-set-bibtex-field :howpublished "GitLab")))))

(defun org-capture-ref-get-bibtex-gitlab-commit ()
  "Parse GitLab commit link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "gitlab\\.com/\\([^/]+/[^/]+\\)/-/commit/\\([0-9a-z]+\\)" link)
      (let ((commit-number (match-string 2 link))
            (commit-repo (match-string 1 link)))
        (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
        ;; Find author
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "^commit-author-name$"))
        (org-capture-ref-set-bibtex-field :title  (format "Commit(%s): %s"
                                           (s-truncate 10 commit-number)
                                           (org-capture-ref-query-dom :class "^commit-title$")))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "^header-main-content$" :class "^js-timeago$" :attr 'datetime :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :howpublished (format "GitLab:%s" commit-repo))
        (throw :finish t)))))

(defun org-capture-ref-get-bibtex-git-savannah-gnu-org-commit ()
  "Parse git.savannah.gnu.org comit page and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "git\\.savannah\\.gnu\\.org/cgit/\\([^/]+\\)\\.git/commit/.+\\(?:?id=\\([0-9a-z]+\\)\\)" link)
      (let ((commit-number (match-string 2 link))
            (commit-repo (match-string 1 link)))
        (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
        ;; Find author
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "^commit-info$" :tag 'tr :apply #'car :tag 'td :apply #'car))
        (org-capture-ref-set-bibtex-field :title  (format "Commit(%s): %s"
                                           (s-truncate 10 commit-number)
                                           (org-capture-ref-query-dom :class "^commit-subject$")))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "^commit-info$" :tag 'tr :apply #'car :tag 'td :apply #'cadr :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :howpublished (format "git.savannah.gnu.org:%s" commit-repo))
        (throw :finish t)))))

(defun org-capture-ref-get-bibtex-gnu-org ()
  "Parse gnu.org page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "gnu\\.org" link)
      (org-capture-ref-unless-set '(:title))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "^top$")))))

(defun org-capture-ref-get-bibtex-nullprogram ()
  "Parse nullprogram.com blog and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "nullprogram\\.com" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author "Chris Wellons")
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :tag 'title))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'time :attr 'datetime :apply #'org-capture-ref-extract-year-from-string))
      (org-capture-ref-set-bibtex-field :howpublished "null program")
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-youtube-watch ()
  "Parse Youtube watch link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "youtube\\.com/watch" link)
      ;; Remove garbage from the link
      (let ((video-id (and (string-match "v=[0-9a-zA-Z_-]+" link) (match-string 0 link))))
        (setq link (replace-regexp-in-string "\\?[^/]+$" (concat "?" video-id) link)))
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :typealt "video")
      (org-capture-ref-set-capture-info :link link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :effort (org-capture-ref-parse-timestamp (org-capture-ref-query-dom :meta 'duration)))
      (org-capture-ref-unless-set '(:author :title :year)
	;; Find author
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :attr '(itemprop . "author") :attr '(itemprop . "name") :attr 'content))
	;; Find title
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'name))
	;; Find year
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta 'datePublished :apply #'org-capture-ref-extract-year-from-string))))))

(defun org-capture-ref-get-bibtex-youtube-channel ()
  "Parse Youtube channel link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "\\(https://www\\.youtube\\.com/channel/[^/]+\\)\\(?:/featured\\)?$" link)
      (org-capture-ref-set-bibtex-field :url (match-string 1 link))
      (org-capture-ref-set-bibtex-field :typealt "author")
      (org-capture-ref-set-capture-info :link link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :id "channel-name"
                                            :class "ytd-channel-name"
                                            :id "text"
                                            :apply #'car)))))

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
      (org-capture-ref-unless-set '(:url :author :title :year)
	(with-current-buffer (org-capture-ref-get-buffer)
	  ;; Find authors
          (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "article__head-wrapper" :class "user-info__username"))
	  (goto-char (point-min))
	  (when (re-search-forward "\"article_authors\": \\[\\([^]]+\\)" nil t)
            (let ((authors (s-split "," (s-collapse-whitespace (s-replace "\n" "" (match-string 1))))))
              (setq authors (mapcar (apply-partially #'s-replace-regexp "^[ ]*\"\\(.+\\)\"[ ]*$" "\\1") authors))
              (setq authors (s-join ", " authors))
              (org-capture-ref-set-bibtex-field :author authors)))
	  ;; Find title
          (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'og:title))
	  ;; Find year
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "tm-article-snippet__datetime-published" :attr 'datetime :apply #'org-capture-ref-extract-year-from-string)))))))

(defun org-capture-ref-get-bibtex-samlib-book ()
  "Generate BiBTeX for a samlib.ru book page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (and (string-match "\\(?:samlib\\|budclub\\)\\.ru/\\(?:editors/\\)?[a-z]/[^/]+/\\(.+html\\)" link)
               (match-string 1 link)
               (not (s-match "index\\(title\\)?" (match-string 1 link))))
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "budclub" "samlib" link))
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Samlib")
      (org-capture-ref-set-bibtex-field :publisher "Samlib")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :isbn org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (let ((mstring (org-capture-ref-query-dom :tag 'h3 :apply #'car)))
                                   (when (string-match "\\(.+\\):" mstring) (match-string 1 mstring))))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :tag 'center :tag 'h2 :apply #'car))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'center :tag 'table :apply (apply-partially #'nth 4)
                                           :tag 'ul :tag 'li :apply (apply-partially #'nth 2)
                                           :apply #'dom-text :apply #'org-capture-ref-extract-year-from-string)))))

(defun org-capture-ref-get-bibtex-fantlab-author ()
  "Generate BiBTeX for a fantlab.ru author page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "fantlab\\.ru/autor" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :typealt "author")
      (org-capture-ref-set-bibtex-field :howpublished "Fantlab")
      (org-capture-ref-set-bibtex-field :publisher "Fantlab")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :isbn org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :title (let ((author-string (org-capture-ref-query-dom :class "main-info-block-header" :tag 'h1 :attr '(itemprop . "name"))))
                                  (when (string-match "\\(.+?\\) *(\\(.+\\))" author-string)
                                    (format "%s / %s" (match-string 1 author-string) (match-string 2 author-string)))))
      (org-capture-ref-set-bibtex-field :author org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value))))

(defun org-capture-ref-get-bibtex-fantlab-work ()
  "Generate BiBTeX for a fantlab.ru book page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "fantlab\\.ru/work" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Fantlab")
      (org-capture-ref-set-bibtex-field :publisher "Fantlab")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :isbn org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :id "^work-names-unit$" :attr '(itemprop . "author")))
      (org-capture-ref-set-bibtex-field :title (s-concat (org-capture-ref-query-dom :join " / " :id "^work-names-unit$" :attr '(itemprop . "name"))
                                          (let ((extra-title (org-capture-ref-query-dom :id "^work-names-unit$" :tag 'p :apply #'car)))
                                            (cond
                                             ((seq-empty-p extra-title)
                                              "")
                                             ((string-match (rx (or "год" "цикл")) extra-title)
                                              "")
                                             ((string-match (rx "Другие названия: " (group (1+ nonl))) extra-title)
                                              (format " / %s" (match-string 1 extra-title)))
                                             (t
                                              (format " / %s" extra-title))))))
      (when (let ((case-fold-search nil)) (string-match-p "Цикл" (org-capture-ref-query-dom :id "^work-names-unit$" :tag 'p :apply #'cadr)))
        (org-capture-ref-set-bibtex-field :title (format "Series: %s" (org-capture-ref-get-bibtex-field :title)))
        (org-capture-ref-set-bibtex-field :type "misc")
        (org-capture-ref-set-bibtex-field :typealt "series")
        (throw :finish t))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :id "^work-names-unit$" :attr '(itemprop . "datePublished"))))))

(defun org-capture-ref-get-bibtex-fantlab-edition ()
  "Generate BiBTeX for a fantlab.ru book edition page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "fantlab\\.ru/edition" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :howpublished "Fantlab")
      (org-capture-ref-set-bibtex-field :isbn (org-capture-ref-query-dom :class "^titles-block-center$" :class "^isbn$"))
      ;; We still parse manually since internation ISBN for Russian books is ugly
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "^titles-block-center$" :attr '(itemprop . "author")))
      (when (seq-empty-p (org-capture-ref-get-bibtex-field :author))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "^titles-block-center$" :id "autors" :tag 'a)))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "^titles-block-center$" :id "^name$"))
      (org-capture-ref-set-bibtex-field :publisher (org-capture-ref-query-dom :class "^titles-block-center$" :id "^publisher$"))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "^titles-block-center$" :id "^year$" :apply #'dom-text :apply #'org-capture-ref-extract-year-from-string))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-fanfics-me ()
  "Generate BiBTeX for an fanfics.me/fic* book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "fanfics\\.me/fic" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Fanfics.me")
      (org-capture-ref-set-bibtex-field :publisher "Fanfics.me")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-unless-set '(:url :author :title :year)
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "FicHead" :tag 'h1 :apply #'caddar))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "FicHead" :class "tr" :apply #'car :class "user"))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "FicHead" :class "DateUpdate" :apply #'org-capture-ref-extract-year-from-string)))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-authortoday-work ()
  "Generate BiBTeX for an author.today/work book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "author\\.today/work" link)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "?[^?]+$" "" link))
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Author.Today")
      (org-capture-ref-set-bibtex-field :publisher "Author.Today")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-unless-set '(:url :author :title :year)
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "book-meta-panel" :class "book-title"))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "book-authors" :tag 'a))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "book-meta-panel" :class "hint-top" :tag 'span :attr 'data-time :apply #'car :apply #'org-capture-ref-extract-year-from-string)))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-authortoday-reader ()
  "Generate BiBTeX for an author.today book opened for reading."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "author\\.today/reader/\\([^/]+\\)" link)
      (org-capture-ref-set-new-url (format "https://author.today/work/%s" (match-string 1 link)))
      (org-capture-ref-get-bibtex-authortoday-work))))

(defun org-capture-ref-get-bibtex-authortoday-post ()
  "Generate BiBTeX for an author.today/post post."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "author\\.today/\\(?:post\\|review\\)" link)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "?[^?]+$" "" link))
      (org-capture-ref-set-bibtex-field :howpublished "Author.Today")
      (org-capture-ref-set-bibtex-field :publisher "Author.Today")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-unless-set '(:url :author :title :year)
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "post-title"))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "^mr$" :tag 'a))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "hint-top-right mr" :tag 'span :attr 'data-time :apply #'car :apply #'org-capture-ref-extract-year-from-string))))))

(defun org-capture-ref-get-bibtex-ficbook ()
  "Generate BiBTeX for an ficbook.net book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "ficbook\\.net" link)
      (when (string-match "^.+ficbook\\.net/readfic/[^/#]+" link)
        (setq link (match-string 0 link)))
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Ficbook")
      (org-capture-ref-set-bibtex-field :publisher "Ficbook")
      ;; Mark unneeded fields
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-unless-set '(:url :author :title :year)
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "fanfic-main-info" :tag 'h1))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "creator-info" :tag 'a))
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
          (org-capture-ref-set-bibtex-field :isbn org-capture-ref-placeholder-value 'force)
        (org-capture-ref-get-bibtex-from-isbn))
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :id "metacol" :id "bookAuthors" :join " and " :class "^authorName$"))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :id "^bookTitle"))
      (let ((details (s-replace-regexp "  +" " " (s-replace "\n" " " (dom-texts (dom-by-id (org-capture-ref-get-dom) "^details$"))))))
        
        (when (string-match "Published .*?\\([0-9]\\{4\\}\\)\\(?: *by *\\([^(;]+?\\)\\(?:(\\|More details...\\|Original title\\)\\)" details)
          (org-capture-ref-set-bibtex-field :publisher (when (match-string 2 details) (s-trim (match-string 2 details))))
          (org-capture-ref-set-bibtex-field :year (match-string 1 details))))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-amazon ()
  "Generate BiBTeX for Amazon book."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "amazon\\." link)
      (org-capture-ref-set-bibtex-field :url link)
      (when-let ((isbn-line (seq-find (lambda (str) (s-contains-p "ISBN-10" str))
                                      (mapcar #'dom-texts
                                              (dom-by-class (car (dom-by-id (org-capture-ref-get-dom)
                                                                            "^detailBullets_feature_div$"))
                                                            "^a-list-item$")))))
        (when (string-match "\\([0-9X]\\{10,\\}\\)" isbn-line)
          (org-capture-ref-set-bibtex-field :isbn (match-string 1 isbn-line))))
      (when-let ((isbn-line (seq-find (lambda (str) (s-contains-p "ISBN-10" str))
                                      (mapcar #'dom-texts
                                              (dom-by-class (car (dom-by-id (org-capture-ref-get-dom)
                                                                            "^isbn_feature_div$"))
                                                            "^a-row$")))))
        (when (string-match "\\([0-9X]\\{10,\\}\\)" isbn-line)
          (org-capture-ref-set-bibtex-field :isbn (match-string 1 isbn-line))))
      (when-let ((isbn-line (seq-find (lambda (str) (s-contains-p "ISBN-10" str))
                                      (mapcar #'dom-texts
                                              (dom-by-class (car (dom-by-id (org-capture-ref-get-dom)
                                                                            "^rich_product_information$"))
                                                            "a-section")))))
        (when (string-match "\\([0-9X]\\{10,\\}\\)" isbn-line)
          (org-capture-ref-set-bibtex-field :isbn (match-string 1 isbn-line))))
      (unless (org-capture-ref-get-bibtex-from-isbn)
        ;; Parse books
        (when (or (s-match ": Books$" (org-capture-ref-query-dom :meta "title"))
                  (s-match ": Audible Audiobooks$" (org-capture-ref-query-dom :meta "title")))
          (org-capture-ref-set-bibtex-field :type "book")
          (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
          (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :id "^productTitle$"))
          (org-capture-ref-set-bibtex-field :author (when (s-matches-p "Author" (org-capture-ref-query-dom :class "author" :class "contribution"))
                                       (let ((author (org-capture-ref-query-dom :class "author" :class "contributorNameID")))
                                         (if (string-empty-p author)
                                             (org-capture-ref-query-dom :id "bylineInfo" :class "author" :class "a-link-normal" :tag 'a :apply #'car)
                                           author))))
          (org-capture-ref-set-bibtex-field :publisher (org-capture-ref-query-dom :class "detail-bullet-list"
                                                    :apply #'dom-texts
                                                    :apply (lambda (str)
                                                             (string-match "Publisher[\n :]+\\([^\n]+?\\)\\(?:[;(][^\n]+?\\)" str)
                                                             (match-string 1 str))))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "detail-bullet-list"
                                               :apply #'dom-texts
                                               :apply (lambda (str)
                                                        (string-match "Publisher[\n :]+\\(?:[^\n]+?\\)\\([;(][^\n]+\\)" str)
                                                        (match-string 1 str))
                                               :apply #'org-capture-ref-extract-year-from-string)))))))

(defun org-capture-ref-get-bibtex-steam ()
  "Generate BiBTeX for Steam page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "store\\.steampowered\\.com/app/" link)
      (org-capture-ref-set-bibtex-field :url link)
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :typealt "game")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (when-let ((info (org-capture-ref-query-dom :class "game_details"
                                   :class "details_block"
                                   ;; We need to distinguish "<br>" tags or string becomes unparseable.
                                   :apply (lambda (block) (mapcar (lambda (div)
                                                               (mapcar
                                                                (lambda (el)
                                                                  (if (and (listp el) (eq (car el) 'br)) "\n" el))
                                                                div))
                                                             block))
                                   :apply #'dom-texts)))
        (when (string-match "Title:[ \n\t]+\\([^:\n\t]+\\)" info)
          (org-capture-ref-set-bibtex-field :title (match-string 1 info)))
        (when (string-match "Developer:[ \n\t]+\\([^:\n\t]+\\)" info)
          (org-capture-ref-set-bibtex-field :author (match-string 1 info)))
        (when (string-match "Publisher:[ \n\t]+\\([^:\n\t]+\\)" info)
          (org-capture-ref-set-bibtex-field :publisher (match-string 1 info)))
        (when (string-match "Release date:[ \n\t]+\\([^:\n\t]+\\)" info)
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (match-string 1 info))))))))

(defun org-capture-ref-get-bibtex-aps ()
  "Generate BiBTeX for APS publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "aps\\.org/doi/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-springer ()
  "Generate BiBTeX for Springer publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    ;; Sometimes, / in doi is coded as %2F.
    (setq link (replace-regexp-in-string "%2F" "/" link))
    (setq link (replace-regexp-in-string "%3A" ":" link))
    (when (string-match "springer\\.com/\\(?:chapter/\\|article/\\)?\\(.+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (if (org-capture-ref-get-bibtex-from-first-doi)
          (throw :finish t)
        (org-capture-ref-set-bibtex-field :type "article")
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'citation_title))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (org-capture-ref-query-dom :meta 'prism.publicationDate)))
        (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :meta 'citation_journal_title))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :meta 'dc.creator))
        (org-capture-ref-set-bibtex-field :pages (format "%s-%s" (org-capture-ref-query-dom :meta 'citation_firstpage) (org-capture-ref-query-dom :meta 'citation_lastpage)))))))

(defun org-capture-ref-get-bibtex-tandfonline ()
  "Generate BiBTeX for Tandfonline publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "tandfonline\\.com/doi/\\(?:full\\|abs\\)/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        (let ((pagerangehistory (org-capture-ref-query-dom :class "itemPageRangeHistory")))
          (when (string-match "Pages \\([0-9]+-[0-9]+\\).+Published online: [0-9]+ [a-zA-Z]+ \\([0-9]\\{4\\}\\)" pagerangehistory)
            (org-capture-ref-set-bibtex-field :pages (match-string 1 pagerangehistory))
            (org-capture-ref-set-bibtex-field :year (match-string 2 pagerangehistory))))
        (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :class "journal-heading" :tag 'a))
        (let ((issue-heading (org-capture-ref-query-dom :class "issue-heading")))
          (when (string-match "Volume \\([0-9]+\\)" issue-heading)
            (org-capture-ref-set-bibtex-field :volume (match-string 1 issue-heading))))
        (let ((issue (org-capture-ref-query-dom :class "nav-toc-list")))
          (when (string-match "Issue \\([0-9]+\\)" issue)
            (org-capture-ref-set-bibtex-field :issue (match-string 1 issue))))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-meta 'dc.Creator))
        (org-capture-ref-set-bibtex-field :keywords (org-capture-ref-query-meta 'keywords ", "))))))

(defun org-capture-ref-get-bibtex-wiley ()
  "Generate BiBTeX for Wiley publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "wiley\\.com/doi/\\(?:abs\\|full\\)/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        (org-capture-ref-set-bibtex-field :type "article")
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'citation_title))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :meta 'citation_author))
        (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :meta 'citation_journal_title))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta 'citation_online_date :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :pages (org-capture-ref-query-dom :meta 'citation_firstpage))))))

(defun org-capture-ref-get-bibtex-ieee ()
  "Generate BiBTeX for IEEE publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "ieeexplore\\.ieee\\.org" link)
      (org-capture-ref-set-bibtex-field :doi (org-capture-ref-query-dom :class "stats-document-abstract-doi" :tag 'a))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        (org-capture-ref-set-bibtex-field :type "article")
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'og:title))
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "authors-info" :tag 'a))
        (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :class "stats-document-abstract-publishedIn"))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "doc-abstract-pubdate" :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :pages (org-capture-ref-query-dom :meta 'citation_firstpage))))))

(defun org-capture-ref-get-bibtex-semanticscholar ()
  "Generate BiBTeX for Semanticscholar page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "semanticscholar\\.org" link)
      (org-capture-ref-set-bibtex-field :doi (org-capture-ref-query-dom :class "doi__link"))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        ;; Try arXiv papers.
        (when-let ((arxiv-link (org-capture-ref-query-dom :attr '(data-selenium-selector . "paper-link") :attr 'href)))
          (when (string-match "arxiv\\.org/pdf/\\(.+\\)\\.pdf" arxiv-link)
            (setq arxiv-link (format "https://arxiv.org/abs/%s" (match-string 1 arxiv-link)))
            (org-capture-ref-set-new-url arxiv-link)
            (org-capture-ref-get-bibtex-arxiv)
            (throw :finish t))
          ;; Parse generic metadata. Note that generic parsing may not
          ;; work because Semanticscholar duplicates metadata and puts
          ;; confusing "undefined" value when some metadata is missing.
          (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :tag 'meta :attr '(name . "citation_author") :attr 'content))
          (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :tag 'meta :attr '(name . "citation_publication_date") :attr 'content))
          (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :tag 'meta :attr '(name . "citation_title") :attr 'content))
          (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :tag 'meta
                                                  :attr '(name . "citation_journal_title")
                                                  :attr 'content
                                                  :apply
                                                  (lambda (journal)
                                                    (unless (or (not (stringp journal))
                                                                (string= "undefined" journal))
                                                      journal)))))))))

(defun org-capture-ref-get-bibtex-ohiolink ()
  "Generate BiBTeX for Ohiolink PhD thesis page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "ohiolink\\.edu" link)
      (org-capture-ref-set-bibtex-field :type "phdthesis")
      (org-capture-ref-set-bibtex-field :url (org-capture-ref-query-dom :id "^P10_PERMALINK$"))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :id "^P10_TITLE$"))
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "abstract-author"))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :id "^P10_DEGREE_NAME$" :apply #'org-capture-ref-extract-year-from-string))
      (org-capture-ref-set-bibtex-field :school (let ((info (org-capture-ref-query-dom :id "^P10_DEGREE_NAME$")))
                                   (when (string-match "Doctor of Philosophy, \\(.+?\\)\\.?$" info)
                                     (match-string 1 info))))
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value))))

(defun org-capture-ref-get-bibtex-proquest ()
  "Generate BiBTeX for ProQuest page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "search\\.proquest\\.com" link)
      (with-current-buffer (org-capture-ref-get-buffer)
        (goto-char 1)
        (when (re-search-forward "DOI:\\([0-9.]+/.+\\)" nil t)
          (org-capture-ref-set-bibtex-field :doi (match-string 1))
          (org-capture-ref-get-bibtex-from-first-doi))))))

(defun org-capture-ref-get-bibtex-sciencedirect-article ()
  "Generate BiBTeX for Sciencedirect publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "sciencedirect\\.com/science/article" link)
      (org-capture-ref-set-bibtex-field :doi (replace-regexp-in-string "https?://doi\\.org/" "" (org-capture-ref-query-dom :class "^doi$")))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (org-capture-ref-query-meta 'citation_publication_date)))
        (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-meta 'citation_journal_title))
        (when-let ((volume-date-page (org-capture-ref-query-dom :class "publication-volume" :class "^text-xs$")))
          (when (string-match "Volume \\([0-9]+\\) , [0-9]+ [a-zA-Z]+ [0-9]+, \\([0-9-]+\\)" volume-date-page)
            (org-capture-ref-set-bibtex-field :volume (match-string 1 volume-date-page))
            (org-capture-ref-set-bibtex-field :pages (match-string 2 volume-date-page))))
        (when-let ((author-surnames (mapcar #'dom-texts (dom-by-class (dom-by-class (org-capture-ref-get-dom) "author") "surname")))
                   (author-names (mapcar #'dom-texts (dom-by-class (dom-by-class (org-capture-ref-get-dom) "author") "given-name"))))
          (org-capture-ref-set-bibtex-field :author (mapconcat #'identity
                                                (cl-mapcar (lambda (surname name)
                                                             (format "%s %s" name surname))
                                                           author-surnames author-names)
                                                " and ")))))))

(defun org-capture-ref-get-bibtex-sciencemag-careers-article ()
  "Generate BiBTeX for Science carreers publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "sciencemag\\.org/careers/" link)
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "article__header" :class "byline--article" :tag 'a))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :class "article__header" :class "byline--article" :tag 'time :apply #'dom-text :apply #'org-capture-ref-extract-year-from-string))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "article__header" :class "article__headline"))
      (org-capture-ref-set-bibtex-field :publisher "Science")
      (org-capture-ref-set-bibtex-field :howpublished "Science")
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-nature-careers-article ()
  "Generate BiBTeX for Nature carreers publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "nature\\.com/articles/" link)
      (org-capture-ref-set-bibtex-field :type "article")
      (org-capture-ref-set-bibtex-field :doi (org-capture-ref-query-dom :meta "DOI"))
      (org-capture-ref-set-bibtex-field :author (let ((author (org-capture-ref-query-dom :join " and " :meta "citation_author")))
                                   (if (string-empty-p author)
                                       org-capture-ref-placeholder-value
                                     author))
                         'force)
      (if (org-capture-ref-get-bibtex-field :author)
          (progn
            (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta "citation_online_date" :apply #'org-capture-ref-extract-year-from-string))
            (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta "citation_title"))
            (org-capture-ref-set-bibtex-field :journal (org-capture-ref-query-dom :meta "citation_journal_title"))
            (org-capture-ref-set-bibtex-field :publisher (org-capture-ref-query-dom :meta "citation_publisher")))
        ;; News articles store author in different field.
        (org-capture-ref-set-bibtex-field :type "misc")
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and" :meta "dc.creator"))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta "dc.date" :apply #'org-capture-ref-extract-year-from-string))
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta "dc.title"))
        (org-capture-ref-set-bibtex-field :publisher (org-capture-ref-query-dom :meta "dc.publisher")))
      (org-capture-ref-set-bibtex-field :howpublished "Nature")
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-ams-cn ()
  "Generate BiBTeX for Acta Metallurgica Sinica publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "ams\\.org\\.cn/[^/]+/\\([^/]+/[^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (unless (org-capture-ref-get-bibtex-from-first-doi)
        (org-capture-ref-clean-bibtex (with-current-buffer (url-retrieve-synchronously (org-capture-ref-query-dom :id "bibtex_export" :attr 'href))
                         (goto-char 1)
                         (re-search-forward "@article{")
                         ;; The key they provide is garbage.
                         (setf (buffer-substring (point) (line-end-position)) (format "%s," (org-capture-ref-generate-key-from-url)))
                         (string-clean-whitespace (buffer-substring url-http-end-of-headers (point-max)))))))))

(defun org-capture-ref-get-bibtex-lesswrong ()
  "Generate BiBTeX for LessWrong publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "lesswrong\\.com" link)
      (org-capture-ref-set-bibtex-field :howpublished "Lesswrong")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-unless-set '(:author :title :year)
        (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :class "^PostsAuthors-authorName$" :tag 'a))
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "PostsPageTitle"))
        (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (org-capture-ref-query-dom :class "PostsPageDate-date" :tag 'span)))))))

(defun org-capture-ref-get-bibtex-stallman ()
  "Generate BiBTeX for stallman.org publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "stallman\\.org" link)
      (org-capture-ref-set-bibtex-field :howpublished "Stallman")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author "Richard Stallman")
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :tag 'head :tag 'title))
      ;; Not publication date info here.
      (org-capture-ref-set-bibtex-field :year org-capture-ref-placeholder-value))))

(defun org-capture-ref-get-bibtex-karl-voit ()
  "Generate BiBTeX for karl-voit.at publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "karl-voit\\.at" link)
      (org-capture-ref-set-bibtex-field :howpublished "Karl-Voit")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :meta "author"))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta "og:title"))
      (org-capture-ref-set-bibtex-field :key (org-capture-ref-query-dom :meta "orgmode-id"))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta "article:published_time" :apply #'org-capture-ref-extract-year-from-string)))))

(defun org-capture-ref-get-bibtex-imdb-movie ()
  "Generate BiBTeX for a IMDB movie page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "imdb\\.com/title/[a-z0-9]+" link)
      (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string "/\\?ref_=.+$" "" link))
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :typealt "movie")
      (org-capture-ref-set-bibtex-field :howpublished "IMDB")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :isbn org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :effort (org-capture-ref-parse-timestamp (org-capture-ref-query-dom :class "^title_wrapper$" :tag 'time :attr 'datetime)))
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :class "^plot_summary" :class "^credit_summary_item$" :apply #'car :tag 'a))
      (let ((title-field (org-capture-ref-query-dom :class "title_wrapper" :tag 'h1)))
        (org-capture-ref-set-bibtex-field :title (replace-regexp-in-string (rx (0+ whitespace) "(" (0+ whitespace) (1+ digit) (0+ whitespace) ")") "" title-field))
        (org-capture-ref-set-bibtex-field :year (and (string-match (rx "(" (0+ whitespace) (group (= 4 digit)) (0+ whitespace) ")") title-field)
                                      (match-string 1 title-field)))))))

(defun org-capture-ref-get-bibtex-wikipedia-book ()
  "Generate BiBTeX for a Wikipedia book page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (and (string-match "wikipedia\\.org/wiki/" link)
               (or (string-match "(book)" (org-capture-ref-query-dom :meta 'og:title))
                   (and (string-match "Author" (org-capture-ref-query-dom :class "infobox vcard" :apply #'dom-texts))
                        (string-match "\\(Publication[ \t\n]+date\\|Published\\)" (org-capture-ref-query-dom :class "infobox vcard" :apply #'dom-texts)))))
      (org-capture-ref-set-bibtex-field :type "book")
      (org-capture-ref-set-bibtex-field :howpublished "Wikipedia")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (let ((info-text (org-capture-ref-query-dom :class "infobox vcard" :tag 'tr :apply (lambda (els) (mapconcat #'dom-texts els "\n")))))
        (org-capture-ref-set-bibtex-field :isbn (and (string-match "\\([0-9-]+\\{10,13\\}\\)" info-text) (match-string 1 info-text)))
        (org-capture-ref-set-bibtex-field :publisher (and (string-match "Publisher[ \t\n]+\\([^\n]+\\)" info-text) (match-string 1 info-text)))
        (org-capture-ref-set-bibtex-field :author (and (string-match "Author[ \t\n]+\\([^\n]+\\)" info-text) (match-string 1 info-text)))
        (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :class "infobox-title"))
        (org-capture-ref-set-bibtex-field :year (and (string-match "[0-9]\\{4\\}" info-text) (match-string 0 info-text))))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-orgmode-ml ()
  "Generate BiBTeX for an Org-mode mailing list page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "orgmode\\.org/list/\\([^/]+\\)" link)
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :typealt "email")
      (org-capture-ref-set-bibtex-field :howpublished "ML:Org mode")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :key (match-string 1 link))
      (org-capture-ref-set-bibtex-field :link (format "notmuch:id:%s" (match-string 1 link)))
      (let ((body-text (org-capture-ref-query-dom :id "^b$")))
        (org-capture-ref-set-bibtex-field :author (and (string-match "^From: \\(.+\\)$" body-text) (match-string 1 body-text)))
        (org-capture-ref-set-bibtex-field :title (and (string-match "^Subject: \\(.+\\)$" body-text) (match-string 1 body-text)))
        (org-capture-ref-set-bibtex-field :year (and (string-match "^Date: .+?\\([0-9]\\{4\\}\\)" body-text) (match-string 1 body-text))))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-list-gnu-ml ()
  "Generate BiBTeX for a lists.gnu.org mailing list page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "lists\\.gnu\\.org/archive/html/\\([^/]+\\)/.+msg.+" link)
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :typealt "email")
      (org-capture-ref-set-bibtex-field :howpublished (org-capture-ref--get-email-howpublished (match-string 1 link)))
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :key (org-capture-ref-query-dom :tag 'form :tag 'input :attr '(name . "d") :attr 'value))
      (org-capture-ref-set-bibtex-field :link (format "notmuch:id:%s" (org-capture-ref-get-bibtex-field :key)))
      (let ((body-text (org-capture-ref-query-dom :tag 'table :tag 'tbody)))
        (org-capture-ref-set-bibtex-field :author (and (string-match "From\\(?:[ \n]*\\):\\(?:[ \n]*\\)\\(.+\\)$" body-text) (match-string 1 body-text)))
        (org-capture-ref-set-bibtex-field :title (and (string-match "Subject\\(?:[ \n]*\\):\\(?:[ \n]*\\)\\(.+\\)$" body-text) (match-string 1 body-text)))
        (org-capture-ref-set-bibtex-field :year (and (string-match "Date\\(?:[ \n]*\\):\\(?:[ \n]*\\).+?\\([0-9]\\{4\\}\\)" body-text) (match-string 1 body-text))))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-arxiv ()
  "Generate BiBTeX for ArXiv publication."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "arxiv\\.org/abs" link)
      (when-let ((doi (org-capture-ref-query-dom :class "metatable" :class "link-https link-external" :attr 'data-doi)))
        (org-capture-ref-set-bibtex-field :doi doi)
        (org-capture-ref-get-bibtex-from-first-doi))
      (org-capture-ref-set-bibtex-field :type "misc")
      (org-capture-ref-set-bibtex-field :howpublished "ArXiv")
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-set-bibtex-field :author (org-capture-ref-query-dom :join " and " :meta 'citation_author))
      (org-capture-ref-set-bibtex-field :title (org-capture-ref-query-dom :meta 'citation_title))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-query-dom :meta 'citation_date :apply #'org-capture-ref-extract-year-from-string)))))

(defun org-capture-ref-get-bibtex-archive-book ()
  "Generate BiBTeX to archive.org book page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "archive\\.org/details/" link)
      (when-let ((isbns (org-capture-ref-query-dom :join "\n" :class "metadata-expandable-list" :class "metadata-definition" :attr '(itemprop . "isbn"))))
        (mapc (lambda (isbn)
                (org-capture-ref-set-bibtex-field :isbn isbn)
                (org-capture-ref-get-bibtex-from-isbn))
              (s-split "\n" isbns))))))

(defun org-capture-ref-get-bibtex-doi ()
  "Generate BiBTeX for an actual doi.org link."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "doi\\.org/\\([0-9a-z-_/.]+\\)" link)
      (org-capture-ref-set-bibtex-field :doi (match-string 1 link))
      (org-capture-ref-get-bibtex-from-first-doi))))

(defun org-capture-ref-get-bibtex-google-scholar-bibtex-page ()
  "Harvest bibtex entry from Google Scholar bibtex page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "scholar\\.googleusercontent\\.com/scholar\\.bib" link)
      (org-capture-ref-set-bibtex-field :doi org-capture-ref-placeholder-value)
      (org-capture-ref-clean-bibtex (org-capture-ref-query-dom) 'no-hooks))))

(defun org-capture-ref-get-bibtex-google-books ()
  "Harvest bibtex entry from Google Books page."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "books\\.google" link)
      (when-let ((metadata (org-capture-ref-query-dom :id "metadata_content_table" :apply #'dom-texts)))
        (when (string-match "[0-9]\\{10\\}" metadata)
          (org-capture-ref-set-bibtex-field :isbn (match-string 0 metadata))
          (org-capture-ref-get-bibtex-from-isbn))))))

;; Getting BiBTeX from elfeed entries

;;;; Capturing from elfeed
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

(defun org-capture-ref-get-bibtex-ted-elfeed (_)
  "Fix google redirect in TED feeds."
  (when (string-match "feedproxy\\.google\\.com/~r/TEDTalks_video/.+/\\([^/]+\\)" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :howpublished "TED")
    (org-capture-ref-set-bibtex-field :url (format "https://www.ted.com/talks/%s" (match-string 1 (org-capture-ref-get-bibtex-field :url))))
    (when (org-capture-ref-get-bibtex-field :author)
      (org-capture-ref-set-bibtex-field :title (replace-regexp-in-string (format " | %s" (regexp-quote (org-capture-ref-get-bibtex-field :author)))
                                                          ""
                                                          (org-capture-ref-get-bibtex-field :title))))))

(defun org-capture-ref-get-bibtex-nature-elfeed (_)
  "Fix redirect in nature RSS feeds."
  (when (string-match "feeds\\.nature\\.com.+/\\([^/]+\\)" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-new-url (format "https://www.nature.com/articles/%s" (match-string 1 (org-capture-ref-get-bibtex-field :url))))
    (org-capture-ref-get-bibtex-nature-careers-article)
    (throw :finish t)))

;; Getting BiBTeX from Emacs buffer

;;;; Capturing from Emacs buffers
(defun org-capture-ref-get-bibtex-bibtex-mode ()
  "Get BiBTeX entry at point from bibtex buffer."
  (when (eq major-mode 'bibtex-mode)
    (when-let* ((beg (bibtex-beginning-of-entry))
                (entry-valid-p (bibtex-valid-entry t))
                (end (progn (beginning-of-line 2) (bibtex-end-of-entry)))
                (bibtex-string (buffer-substring-no-properties beg end)))
      (org-capture-ref-clean-bibtex bibtex-string 'no-hooks)
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-eaf-browser ()
  "Get BiBTeX entry from EAF buffer."
  (when (featurep 'eaf)
    (when (and (eq major-mode 'eaf-mode)
               (equal "browser" eaf--buffer-app-name))
      (let ((html-file (make-temp-file "org-capture-ref-"))
            (html (eaf-call-sync "call_function" eaf--buffer-id "get_html")))
        (with-temp-file html-file
          (insert html))
        (plist-put (plist-get org-capture-ref--store-link-plist :query) :html html-file)
        (org-capture-ref-set-bibtex-field :url eaf--buffer-url)))))

(defun org-capture-ref--get-email-howpublished (string)
  "Parse STRING containing To: and CC: fields to produce `:howpublished'."
  (cond
   ((string-match-p "emacs-devel@gnu.org" string) "ML:Emacs devel")
   ((string-match-p "emacs-devel" string) "ML:Emacs devel")
   ((string-match-p "emacs-orgmode@gnu.org" string) "ML:Org mode")
   ((string-match-p "emacs-orgmode" string) "ML:Org mode")
   (t "Email")))

(defun org-capture-ref--notmuch-get-info (message-id)
  "Get sexp for MESSAGE-ID."
  (with-temp-buffer
    (insert "(list ")
    (call-process notmuch-command nil t nil "show" "--format=sexp" message-id)
    (insert ")")
    (let ((data (caadr (read (buffer-string)))))
      (while (not (eq ':id (car data)))
        (setf data (car data)))
      data)))

(defun org-capture-ref-get-bibtex-notmuch-show-mode ()
  "Get BiBTeX entry at point from notmuch show buffer."
  (when (eq major-mode 'notmuch-show-mode)
    (org-capture-ref-set-bibtex-field :author (notmuch-show-get-from))
    (org-capture-ref-set-bibtex-field :title (notmuch-show-get-subject))
    (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (notmuch-show-get-date)))
    (org-capture-ref-set-bibtex-field :howpublished (org-capture-ref--get-email-howpublished
                                      (format "%s %s"
                                              (notmuch-show-get-to)
                                              (notmuch-show-get-cc))))
    (org-capture-ref-set-bibtex-field :typealt "email")
    (org-capture-ref-set-bibtex-field :link (format "notmuch:%s" (notmuch-show-get-message-id)))
    (throw :finish t)))

(defun org-capture-ref-get-bibtex-notmuch-search-mode ()
  "Get BiBTeX entry at point from thread in notmuch search buffer."
  (when (eq major-mode 'notmuch-search-mode)
    (when-let ((thread-info (notmuch-search-get-result)))
      (org-capture-ref-set-bibtex-field :author (replace-regexp-in-string "[,|] " " and " (plist-get thread-info :authors)))
      (org-capture-ref-set-bibtex-field :title (plist-get thread-info :subject))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (current-time-string (plist-get thread-info :timestamp))))
      (org-capture-ref-set-bibtex-field :howpublished (org-capture-ref--get-email-howpublished
                                        (let ((message-info (org-capture-ref--notmuch-get-info
                                                             (format "thread:%s" (plist-get thread-info :thread)))))
                                          (format "%s %s"
                                                  (plist-get (plist-get message-info :headers)
                                                             :To)
                                                  (plist-get (plist-get message-info :headers)
                                                             :Cc)))))
      (org-capture-ref-set-bibtex-field :typealt "email_thread")
      (org-capture-ref-set-bibtex-field :link (format "notmuch:thread:%s" (plist-get thread-info :thread)))
      (throw :finish t))))

(defun org-capture-ref-get-bibtex-notmuch-tree-mode ()
  "Get BiBTeX entry at point from thread in notmuch tree buffer."
  (when (eq major-mode 'notmuch-tree-mode)
    (when-let* ((id (notmuch-tree-get-message-id))
                (message-emails (let ((message-info (org-capture-ref--notmuch-get-info id)))
                                  (format "%s %s"
                                          (plist-get (plist-get message-info :headers)
                                                     :To)
                                          (plist-get (plist-get message-info :headers)
                                                     :Cc))))
                (thread-id (with-temp-buffer
                             (call-process notmuch-command nil t nil "search" "--output=threads" id)
                             (goto-char (point-min))
                             (buffer-substring 1 (line-end-position))))
                (thread-info (with-temp-buffer
                               (insert "(list ")
                               (call-process notmuch-command nil t nil "search" "--format=sexp" thread-id)
                               (insert ")")
                               (caadr (read (buffer-string))))))
      (org-capture-ref-set-bibtex-field :author (replace-regexp-in-string "[,|] " " and " (plist-get thread-info :authors)))
      (org-capture-ref-set-bibtex-field :title (plist-get thread-info :subject))
      (org-capture-ref-set-bibtex-field :year (org-capture-ref-extract-year-from-string (current-time-string (plist-get thread-info :timestamp))))
      ;; Number of messages
      (org-capture-ref-set-bibtex-field :pages (format "%S" (plist-get thread-info :total)))
      (org-capture-ref-set-bibtex-field :howpublished (org-capture-ref--get-email-howpublished message-emails))
      (org-capture-ref-set-bibtex-field :typealt "email_thread")
      (org-capture-ref-set-bibtex-field :link (format "notmuch:thread:%s" (plist-get thread-info :thread)))
      (throw :finish t))))

;;;; Generating cite key

(defun org-capture-ref-generate-key-from-link ()
  "Generate citation key from LINK."
  (when-let ((link (org-capture-ref-get-bibtex-field :link)))
    (sha1 link)))

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

(defun org-capture-ref-generate-key-human-readable ()
  "Use `bibtex-generate-autokey' to generate key.
Most of the relevant bibtex.el customisations apply.
The overridden autokey customisations are:
- `bibtex-autokey-year-length'
- `bibtex-autokey-titleword-ignore'
- `bibtex-autokey-title-terminators'
- `bibtex-autokey-prefix-string'."
  (unless (org-capture-ref-get-bibtex-field :key)
    (org-capture-ref-set-bibtex-field :key "placeholder"))
  (let ((bibtex-string (org-capture-ref-format-bibtex))
        (bibtex-autokey-year-length 4)
        (bibtex-autokey-titleword-ignore '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das" "For"
                                           "a" "an" "on" "the" "eine?" "der" "die" "das" "for"
                                           ;; "[^[:upper:]].*"
                                           ".*[^[:upper:][:lower:]0-9].*"))
        (bibtex-autokey-title-terminators (rx unmatchable))
        (bibtex-autokey-prefix-string (if (string= (org-capture-ref-get-bibtex-field :type) "misc")
                                          (concat
                                           (replace-regexp-in-string
                                            " " bibtex-autokey-titleword-separator
                                            (or (org-capture-ref-get-bibtex-field :publisher)
                                                (org-capture-ref-get-bibtex-field :howpublished)))
                                           bibtex-autokey-titleword-separator)
                                        "")))
    (when (string= (org-capture-ref-get-bibtex-field :key) "placeholder")
      (org-capture-ref-set-bibtex-field :key nil 'force))
    (with-temp-buffer
      (bibtex-mode)
      (bibtex-set-dialect 'BibTeX)
      (insert bibtex-string)
      (goto-char 1)
      ;; Work around requirement to have year in the BiBTeX entry.
      (cl-letf (((symbol-function 'bibtex-autokey-get-year) (condition-case err
                                                                `(lambda () ,(bibtex-autokey-get-year))
                                                              (t (lambda () "")))))
        (concat (bibtex-generate-autokey)
                ;; Add unique hash to avoid collisions.
                bibtex-autokey-titleword-separator
                (format "%.3s"
                        (or (org-capture-ref-generate-key-from-doi)
                            (org-capture-ref-generate-key-from-isbn)
                            (org-capture-ref-generate-key-from-url)
                            (org-capture-ref-generate-key-from-link))))))))

;;;; Formatting BibTeX entry

(defun org-capture-ref-get-formatted-bibtex-default ()
  "Default BiBTeX formatter."
  (replace-regexp-in-string
   (format "^.+{\\(%s\\)?},$" org-capture-ref-placeholder-value) ""
   (s-format org-capture-ref-default-bibtex-template
	     (lambda (key &optional _)
	       (or (and (org-capture-ref-get-bibtex-field (intern key))
                        (s-trim (replace-regexp-in-string "[{}]" "" (org-capture-ref-get-bibtex-field (intern key)))))
		   ""))
	     org-capture-ref--bibtex-alist)))

;; Cleaning up BiBTeX entry

(defun org-capture-ref-remove-double-comma ()
  "Remove malformatted \",\" from entry."
  (let ((inhibit-message t))
    (replace-regexp ",,\n" ",\n")))

(defun org-capture-ref-create-key-maybe ()
  "Generate BiBTeX key if it is missing."
  (goto-char 1)
  (unless (looking-at-p bibtex-entry-head)
    (when (looking-at bibtex-any-entry-maybe-empty-head)
      (line-end-position)
      (insert (org-capture-ref-generate-key)))))

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

(defvar org-capture-ref-bibtex-author-garbage-symbols '("*" "§" "¶")
  "Garbage that sometimes appear in author bibtex entries for scientific articles.")

(defun org-capture-ref-remove-garbage-symbols-from-authors ()
  "Remove *, symbols from author field."
  (goto-char 1)
  (when (org-capture-ref-get-bibtex-field :journal)
    (when-let ((author-field (bibtex-search-forward-field "author")))
      (when (cdr author-field)
        (goto-char (cadr author-field))
        (while (re-search-forward (format "[%s]" (mapconcat #'identity org-capture-ref-bibtex-author-garbage-symbols ""))
                                  (caddr author-field)
                                  t)
          (replace-match ""))))))

(defun org-capture-ref-capitalize-author ()
  "Capitalize authors in bibtex entries with :journal."
  (goto-char 1)
  (when (org-capture-ref-get-bibtex-field :journal)
    (when-let ((author-field (bibtex-search-forward-field "author")))
      (when (cdr author-field)
        (goto-char (cadr author-field))
        (while (re-search-forward (rx (1+ word))
                                  (caddr author-field)
                                  t)
          (unless (string= "and" (save-match-data (match-string 0)))
            (setf (buffer-substring (match-beginning 0) (match-end 0)) (capitalize (save-match-data (match-string 0))))
            (goto-char (match-end 0))))))))

;;; Message functions

(defun org-capture-ref-message-emacs (msg &optional severity)
  "Show message in Emacs."
  (pcase severity
    (`error (user-error "%s" msg))
    (`warning (message "%s" msg))
    (_ (message msg))))

(defun org-capture-ref-message-qutebrowser (msg &optional severity)
  "Show message in qutebrowser assuming that qutebrowser fifo is
avaible in :query -> :qutebrowser-fifo capture info."
  (when-let  ((fifo (org-capture-ref-get-capture-info '(:query :qutebrowser-fifo))))
    (pcase severity
      (`error (start-process-shell-command "Send message to qutebrowser"
					   nil
					   (format "echo \"message-error '%s'\" >> %s" msg fifo)))
      (`warning (start-process-shell-command "Send message to qutebrowser"
					     nil
					     (format "echo \"message-warning '%s'\" >> %s" msg fifo)))
      (_ (start-process-shell-command "Send message to qutebrowser"
				      nil
				      (format "echo \"message-info '%s'\" >> %s" msg fifo))))))

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
    (`grep (org-capture-ref-check-regexp-grep (s-replace-all  '(("\\|" . "|")
                                                 ("|" . "")
                                                 ("\\\\\\." . "\\\\.")
                                                 ("'" . ".") ; We use ' as external quotes.
                                                 ("\\(" . "(")
                                                 ("\\)" . ")")
                                                 )
                                               regexp)
                               dont-show-match-p))
    (`org-search-view (org-capture-ref-check-regexp-search-view regexp dont-show-match-p))
    (_ (org-capture-ref-message (format "Invalid value of org-capture-ref-check-regexp-method: %s" org-capture-ref-check-regexp-method) 'error))))

(defvar org-capture-ref--org-agenda-files-cached nil
  "Cached value of `org-agenda-files' from previous run.")
(defvar org-capture-ref--org-agenda-files-and-archives-cached nil
  "Cached value of `org-agenda-files' + archives from previous run.")
(defun org-capture-ref-check-regexp-grep (regexp &optional dont-show-match-p)
  "Check if REGEXP exists in org files using grep.
If DONT-SHOW-MATCH-P is non-nil, do not show the match or agenda search with all matches."
  (unless (executable-find "grep") (org-capture-ref-message "Cannot find grep executable" 'error))
  (let (files
	matches)
    (if (eq org-capture-ref--org-agenda-files-cached org-agenda-files)
        (setq files org-capture-ref--org-agenda-files-and-archives-cached)
      (setq files (org-agenda-files t t))
      (setq org-capture-ref--org-agenda-files-cached org-agenda-files)
      (setq org-capture-ref--org-agenda-files-and-archives-cached files))
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
        (save-excursion
          (save-restriction
	    (switch-to-buffer (marker-buffer (car matches)))
	    (goto-char (car matches))
            (org-back-to-heading t)
	    (org-show-set-visibility 'lineage)
            (org-show-entry)
            (jit-lock-fontify-now (point) (save-excursion (outline-next-heading)))
            (when (yes-or-no-p "Update the entry according to the new capture? ")
              (org-capture-ref-get-bibtex-org-heading)
              (add-hook 'org-capture-after-finalize-hook #'org-capture-ref-update-heading-maybe 100)
              (throw :finish t)))))
      (if dont-show-match-p (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string matches) "\n") 'error)
        (user-error "")))))

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
           (save-excursion
             (save-restriction
	       (switch-to-buffer (marker-buffer (car headlines)))
	       (goto-char (car headlines))
               (if (functionp #'org-fold-reveal)
                   (org-fold-reveal)
	         (org-reveal))
               (jit-lock-fontify-now (point) (save-excursion (outline-next-heading)))
               (when (yes-or-no-p "Update the entry according to the new capture? ")
                 (org-capture-ref-get-bibtex-org-heading)
                 (add-hook 'org-capture-after-finalize-hook #'org-capture-ref-update-heading-maybe 100)
                 (throw :finish t)))))
         (if dont-show-match-p (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string headlines) "\n") 'error)
           (user-error "")))
      (_ (when dont-show-match-p (kill-buffer))
         (if dont-show-match-p (org-capture-ref-message (string-join (mapcar #'org-capture-ref-get-message-string headlines) "\n") 'error)
           (user-error ""))))))

(defun org-capture-ref-check-key ()
  "Check if `:key' already exists.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (org-capture-ref-get-bibtex-field :key)
    (pcase org-capture-ref-check-key-method
      (`org-id-find
       (when-let ((mk (org-id-find (org-capture-ref-get-bibtex-field :key) 'marker)))
         (unless (org-capture-ref-get-capture-template-info :immediate-finish)
           (save-excursion
             (save-restriction
	       (switch-to-buffer (marker-buffer mk))
	       (goto-char mk)
	       (org-show-set-visibility 'lineage)
               (org-show-entry)
               (jit-lock-fontify-now (point) (save-excursion (outline-next-heading)))
               (when (yes-or-no-p "Update the entry according to the new capture? ")
                 (org-capture-ref-get-bibtex-org-heading)
                 (add-hook 'org-capture-after-finalize-hook #'org-capture-ref-update-heading-maybe 100)
                 (throw :finish t)))))
         (when (org-capture-ref-get-capture-info :immediate-finish) (org-capture-ref-message (org-capture-ref-get-message-string mk) 'error))))
      (`grep
       (org-capture-ref-check-regexp-grep (format "^:ID:[ \t]+%s$" (regexp-quote (org-capture-ref-get-bibtex-field :key))) (org-capture-ref-get-capture-template-info :immediate-finish)))
      (_ (org-capture-ref-message (format "Invalid value of org-capture-ref-check-key-method: %s" org-capture-ref-check-key-method) 'error)))))

(defun org-capture-ref-check-url ()
  "Check if `:url' already exists.
It is assumed that `:url' is captured into :SOURCE: property.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (org-capture-ref-get-bibtex-field :url)
    (org-capture-ref-check-regexp (format "^:\\(Source\\|URL\\):[ \t]+%s$" (regexp-quote (org-capture-ref-get-bibtex-field :url))) (org-capture-ref-get-capture-template-info :immediate-finish))))

(defun org-capture-ref-check-doi ()
  "Check if `:doi' already exists.
It is assumed that `:doi' is captured into :DOI: property.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (org-capture-ref-get-bibtex-field :doi)
    (org-capture-ref-check-regexp (format "^:DOI:[ \t]+%s[ \t]*$" (replace-regexp-in-string "[()]" "." (regexp-quote (org-capture-ref-get-bibtex-field :doi)))) (org-capture-ref-get-capture-template-info :immediate-finish))))

(defun org-capture-ref-check-link ()
  "Check if captured `:link' already exists.
The matching is done using `org-capture-ref-check-link-regexp'.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (org-capture-ref-get-capture-info :link)
    (org-capture-ref-check-regexp (format (alist-get org-capture-ref-check-regexp-method org-capture-ref-check-link-regexp)
                           (regexp-quote (org-capture-ref-get-capture-info :link)))
                   (org-capture-ref-get-capture-template-info :immediate-finish))))

(defun org-capture-ref-check-article-title ()
  "Check if `:title' already exists in some heading title.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (and (member (org-capture-ref-get-bibtex-field :type) '("article"))
             (org-capture-ref-get-bibtex-field :title))
    (org-capture-ref-check-regexp (format "^\\*+.+%s" (regexp-quote
                                       (replace-regexp-in-string
                                        "[|]+" ""
                                        (org-capture-ref-get-bibtex-field :title))))
                   (org-capture-ref-get-capture-template-info :immediate-finish))))

(defun org-capture-ref-check-author-title ()
  "Check if `:author' and `:title' already exists in some heading title.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (when (and (org-capture-ref-get-bibtex-field :author)
             (org-capture-ref-get-bibtex-field :title))
    (org-capture-ref-check-regexp (format "^\\*+.+%s.+%s\\(?:[ \t]+:.+:\\)?$"
                           (regexp-quote (org-capture-ref-get-bibtex-field :author))
                           (regexp-quote (org-capture-ref-get-bibtex-field :title)))
                   (org-capture-ref-get-capture-template-info :immediate-finish))))

;;; Updating existing entry
(defun org-capture-ref-get-bibtex-org-heading ()
  "Generate BiBTeX for an org heading at point."
  (when (and (eq 'org-mode major-mode)
	     (org-at-heading-p))
    (dolist (prop (org-entry-properties))
      (pcase (car prop)
	("ID" (org-capture-ref-set-bibtex-field :key (cdr prop)))
        ((and name (or (guard (member name org-special-properties))
		       (guard (member name org-default-properties))))
         nil)
        (name (org-capture-ref-set-bibtex-field (intern (format ":%s" (downcase name))) (cdr prop)))))
    (org-capture-ref-set-bibtex-field :org-props (org-entry-properties))
    (org-capture-ref-set-bibtex-field :org-hd-marker (save-restriction (org-back-to-heading) (point-marker)))))

(defun org-capture-ref-update-bibtex-at-org-heading ()
  "Update BiBTeX entry from Org heading at point."
  (when (and (eq major-mode 'org-mode)
             (org-at-heading-p))
    (let ((bibtex-string (org-bibtex-headline)))
      (if (not bibtex-string)
          (org-capture-ref-set-bibtex-field :url (replace-regexp-in-string
                                   "^\\(?:\\[\\[\\)?\\(.+?\\)\\(?:\\]\\]\\)?$"
                                   "\\1"
                                   (or (org-entry-get (point) "URL")
                                       (org-entry-get (point) "SOURCE"))))
        (org-capture-ref-clean-bibtex bibtex-string 'no-hooks)
        (when (string= (org-capture-ref-get-bibtex-field :key) "nil")
          (org-capture-ref-set-bibtex-field :key org-capture-ref-placeholder-value)))
      (org-capture-ref-get-bibtex-org-heading))))

(defvar org-capture-ref-update-heading-history nil)

(defvar org-capture-ref-update-heading-properties-allow-duplicates `("HOWPUBLISHED" ,@org-special-properties)
  "List of heading properties, whose values may be repeated in other properties.")

(defun org-capture-ref-update-heading-maybe ()
  "Use last captured heading to update existing heading at `:org-hd-marker' bibtex property."
  (when (and (org-capture-ref-get-bibtex-field :org-hd-marker)
             (not org-note-abort))
    (unwind-protect
        (progn
          (org-capture-goto-last-stored)
          (let ((current-heading-props (org-entry-properties))
                (body (save-excursion
                        (save-restriction
                          (org-back-to-heading)
                          (narrow-to-region (point) (save-excursion (or (outline-next-heading) (point-max))))
                          (goto-char (point-at-bol 2))
                          (when (org-at-planning-p) (goto-char (point-at-bol 1)))
                          (re-search-forward org-property-drawer-re nil t)
                          (when (re-search-forward ":LOGBOOK:" nil t)
                            (re-search-forward ":END:"))
                          (buffer-substring-no-properties (point) (point-max))))))
            (save-excursion
              (save-restriction
                (org-back-to-heading)
                (org-cut-subtree)
                (switch-to-buffer (marker-buffer (org-capture-ref-get-bibtex-field :org-hd-marker)))
                (goto-char (org-capture-ref-get-bibtex-field :org-hd-marker))
                (org-show-set-visibility 'lineage)
                (org-show-entry)
                (org-narrow-to-subtree)
	        (dolist (prop current-heading-props)
                  (when prop
                    (unless (member (car prop) '("ALLTAGS" "FILE" "CATEGORY"))
                      (unless (equal (org-entry-get nil (car prop)) (cdr prop))
                        (pcase (or (and (seq-empty-p (org-entry-get nil (car prop) nil t)) ?y)
                                   (pcase (car prop)
                                     ("TAGS"
                                      (read-char-from-minibuffer (format "Update %s from \"%s\" to \"%s\"? (y/n/[m]erge/[c]ustom)" (car prop) (org-entry-get nil (car prop)) (cdr prop))
                                                                 '(?y ?n ?c ?m)))
                                     (_
                                      (read-char-from-minibuffer (format "Update %s from \"%s\" to \"%s\"? (y/n/[c]ustom)" (car prop) (org-entry-get nil (car prop)) (cdr prop))
                                                                 '(?y ?n ?c)))))
                          (?y (cond
                               ((member (car prop) org-special-properties)
                                (pcase (car prop)
                                  ("TAGS" (org-set-tags (cdr prop)))
                                  ("ITEM"
                                   (re-search-forward org-complex-heading-regexp)
                                   (replace-match (cdr prop) nil t nil 4)
                                   (org-back-to-heading))
                                  ("TODO"
                                   (org-todo (cdr prop)))
                                  ("PRIORITY"
                                   (org-priority (string-to-char (cdr prop))))
                                  ("SCHEDULED"
                                   (org-schedule nil (cdr prop)))
                                  ("DEADLINE"
                                   (org-deadline nil (cdr prop)))
                                  (_ nil)))
                               (t (org-entry-put nil (car prop) (cdr prop)))))
                          (?n nil)
                          (?m (pcase (car prop)
                                ("TAGS"
                                 (let ((old-tags (s-split ":" (org-entry-get nil (car prop)) t))
                                       (new-tags (s-split ":" (cdr prop) t)))
                                   (org-set-tags (cl-remove-duplicates (seq-filter #'identity (append old-tags new-tags)) :test #'string=))))
                                (_ (error "Unhandled case"))))
                          (?c (setq org-capture-ref-update-heading-history (list (org-entry-get nil (car prop)) (cdr prop)))
                              (when-let ((str (read-string (format "New value of %s [\"%s\", \"%s\"]: "
                                                                   (car prop)
                                                                   (org-entry-get nil (car prop))
                                                                   (cdr prop))
                                                           nil
                                                           'history)))
                                (cond
                                 ((member (car prop) org-special-properties)
                                  (pcase (car prop)
                                    ("TAGS" (org-set-tags str))
                                    ("ITEM"
                                     (re-search-forward org-complex-heading-regexp)
                                     (replace-match str nil t nil 4)
                                     (org-back-to-heading))
                                    ("TODO"
                                     (org-todo str))
                                    (_ nil)))
                                 (t (org-entry-put nil (car prop) str)))))
                          (_ nil))))))
                (org-back-to-heading)
                ;; Remove BIBTEX drawer from older versions of the format.
                (when (and (re-search-forward "^:BIBTEX:\n#\\+begin_src bibtex"  (save-excursion (or (outline-next-heading) (point-max))) t)
                           (yes-or-no-p "Remove :BIBTEX: drawer? "))
                  (re-search-backward ":BIBTEX:")
                  (re-search-forward "^[ 	]*:BIBTEX:[ 	]*\n\\(?:.*\n\\)*?[ 	]*:END:[ 	]*$")
                  (replace-match ""))
                ;; Add any extra text
                (when (and (not (seq-empty-p body))
                           (not (string-match-p (rx string-start (1+ (any whitespace "\n")) string-end) body))
                           (not (save-excursion (search-forward body (save-excursion (outline-next-heading)) t))))
                  (when-let ((inp (read-char-from-minibuffer (format "Append \"%s\" to body? (y/n/[r]eplace)" body) '(?y ?n ?r))))
                    (pcase inp
                      (?y
                       (widen)
                       (or (outline-next-heading) (goto-char (point-max)))
                       (backward-char)
                       (insert body))
                      (?r
                       (org-back-to-heading)
                       (beginning-of-line 2)
                       (when (looking-at-p org-planning-line-re) (beginning-of-line 2))
                       (let ((next-heading (save-excursion (or (outline-next-heading) (point-max)))))
                         (re-search-forward org-property-drawer-re next-heading t)
                         (re-search-forward org-logbook-drawer-re next-heading t)
                         (setf (buffer-substring (point) next-heading) body))))))
                ;; Check for duplicate metadata
                (let ((current-heading-props (sort (org-entry-properties) (lambda (a b) (string-lessp (cdr a) (cdr b)))))
                      prop
                      next-prop)
                  (setq prop (pop current-heading-props))
                  (while current-heading-props
                    (setq next-prop (pop current-heading-props))
                    (when (and (stringp (cdr prop)) (stringp (cdr next-prop))
                               (not (member (car prop) org-capture-ref-update-heading-properties-allow-duplicates))
                               (not (member (car next-prop) org-capture-ref-update-heading-properties-allow-duplicates))
                               (string= (cdr prop) (cdr next-prop)))
                      (pcase (read-char-from-minibuffer (format "Properties %s¹ and %s² have the same value %s. Remove? (1/2/n)"
                                                                (car prop) (car next-prop) (cdr prop) (car next-prop))
                                                        '(?1 ?2 ?n))
                        (?1 (org-entry-delete (point) (car prop)))
                        (?2 (org-entry-delete (point) (car next-prop)))
                        (_ nil)))
                    (setq prop next-prop)))))))
      (org-capture-ref-reset-state))))
;;; Formatting Org entry

(defun org-capture-ref-headline-format ()
  "Format title as the following:
First author, last author [Journal|School|Publisher|Howpublished] (Year) Title"
  (format "%s%s%s%s"
	  (or (when (org-capture-ref-get-bibtex-field :author)
                (let* ((authors (s-split " +and +" (string-clean-whitespace (org-capture-ref-get-bibtex-field :author))))
		       (author-surnames (mapcar (lambda (author)
                                                  (cond
                                                   ((string-match (rx (group (1+ (not whitespace))) ",") author)
                                                    (match-string 1 author))
                                                   (t (car (last (s-split " +" author))))))
						authors)))
                  (unless (string= "article" (org-capture-ref-get-bibtex-field :type))
                    (setq author-surnames (mapcar
                                           (lambda (author)
                                             (if (string-match (rx (group (1+ word) whitespace (1+ word)) whitespace "<") author)
                                                 (match-string 1 author)
                                               author))
                                           authors)))
		  (if (= 1 (length author-surnames))
                      (format "%s " (car author-surnames))
                    (format "%s, %s " (car author-surnames) (car (last author-surnames))))))
              "")
          (let ((full-name (or (when (org-capture-ref-get-bibtex-field :journal)
		                 (format "[%s] " (org-capture-ref-get-bibtex-field :journal)))
                               (when (org-capture-ref-get-bibtex-field :school)
		                 (format "[%s] " (org-capture-ref-get-bibtex-field :school)))
                               (when (org-capture-ref-get-bibtex-field :publisher)
		                 (format "[%s] " (org-capture-ref-get-bibtex-field :publisher)))
                               (when (org-capture-ref-get-bibtex-field :howpublished)
                                 (format "[%s] " (org-capture-ref-get-bibtex-field :howpublished)))
                               "")))
            (let ((repl (or (cdr (car (cl-member-if (lambda (el) (string= full-name (format "[%s] " (car el)))) org-capture-ref-journal-abbreviations)))
                            (caddr (car (cl-member-if (lambda (el) (string= full-name (format "[%s] " (cadr el)))) org-ref-bibtex-journal-abbreviations)))
                            full-name)))
              (if (string= full-name repl)
                  repl
                (format "[%s] " repl))))
          (or (when (org-capture-ref-get-bibtex-field :year)
                (format "(%s) " (org-capture-ref-get-bibtex-field :year)))
              "")
          (or (org-capture-ref-get-bibtex-field :title)
              "")))

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
(defvar org-capture-ref-capture-template-set-p nil
  "Non-nil when `org-capture-ref-capture-template' is set.")
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
  (unless (org-capture-ref-get-bibtex-field :org-hd-marker) ; Already found the match.
    (catch :finish
      (run-hooks 'org-capture-ref-check-bibtex-functions))))

(defun org-capture-ref-process-capture ()
  "Extract BiBTeX info from currently captured link and generate unique key.

The return value is always empty string, so that this function can be
used inside capture template."
  (unwind-protect
      (progn
	(org-capture-ref-reset-state)
        (add-hook 'org-capture-after-finalize-hook #'org-capture-ref-fetch-collection-maybe 100)
	(unless org-capture-ref-quiet-verbosity (org-capture-ref-message "Capturing BiBTeX..."))
        ;; Early check if the entry is already captured.
        (org-capture-ref-check-bibtex)
	(org-capture-ref-get-bibtex)
        (unless (org-capture-ref-get-bibtex-field :key)
	  (org-capture-ref-set-bibtex-field :key (org-capture-ref-generate-key)))
	(org-capture-ref-set-bibtex-field :bibtex-string (org-capture-ref-format-bibtex))
        ;; Set `:org-hd-marker' if it was provided during capture call.
        (when (or (org-capture-ref-get-capture-info '(:query :org-hd-marker))
                  (org-capture-ref-get-bibtex-field :org-hd-marker))
          (org-with-point-at (or (org-capture-ref-get-capture-info '(:query :org-hd-marker))
                                 (org-capture-ref-get-bibtex-field :org-hd-marker))
            (org-back-to-heading)
            (org-capture-ref-get-bibtex-org-heading)
            (add-hook 'org-capture-after-finalize-hook #'org-capture-ref-update-heading-maybe 100)))
	(org-capture-ref-check-bibtex)
	(unless org-capture-ref-quiet-verbosity (org-capture-ref-message "Capturing BiBTeX... done")))
    (when (buffer-live-p org-capture-ref--buffer) (kill-buffer org-capture-ref--buffer)))
  "")

(defun org-capture-ref-get-org-entry ()
  "Return org entry according to :bibtex-string.
The entry will not have leading stars.
The function uses `org-bibtex-write' internally. Relevant
customisations may apply.
Overridden customisations:
- `org-bibtex-headline-format-function' = `org-capture-ref-headline-format-function'
- `org-bibtex-key-property' = \"ID\"."
  (require 'ol-bibtex)
  (with-temp-buffer
    (insert (org-capture-ref-get-bibtex-field :bibtex-string))
    (org-bibtex-read)
    (with-temp-buffer
      (org-mode)
      (let ((org-bibtex-headline-format-function (lambda (_) (funcall org-capture-ref-headline-format-function)))
            (org-bibtex-key-property "ID"))
        (org-bibtex-write)
        (goto-char 1)
        (when org-capture-ref-headline-tags
          (org-set-tags (append (org-get-tags nil t)
                                (delq nil
                                      (mapcar (lambda (tag)
                                                (pcase tag
                                                  ((pred stringp) tag)
                                                  (field (org-capture-ref-get-bibtex-field field))))
                                              org-capture-ref-headline-tags))))))
      (replace-regexp-in-string "^\\*+ *" "" (substring-no-properties (buffer-string))))))

(defun org-capture-ref-set-capture-template ()
  "Set default capture template according to `org-capture-ref-capture-template'."
  (setq org-capture-ref-capture-template-set-p t)
  (custom-reevaluate-setting 'org-capture-ref-capture-target)
  (custom-reevaluate-setting 'org-capture-ref-capture-keys)
  (custom-reevaluate-setting 'org-capture-ref-capture-template)
  (let ((templates (doct org-capture-ref-capture-template)))
    (dolist (template templates)
      (asoc-put! org-capture-templates
	         (car template)
	         (cdr  template)
	         'replace))))

(defun org-capture-ref-capture-url (url &optional interactive-capture)
  "Capture URL using `org-capture-ref-capture-template'.
With `\\[universal-argument]' argument, use interactive version of the template.
With `\\[universal-argument] \\[universal-argument]' argument, update heading at point."
  (interactive (list (read-string "URL: " (thing-at-point 'url t))
                     current-prefix-arg))
  (unless org-capture-ref-capture-template-set-p
    (user-error "Please, set default capture template with `org-capture-ref-set-capture-template'"))
  (org-protocol-capture
   (list :template (if interactive-capture
                       (car org-capture-ref-capture-keys)
                     (cadr org-capture-ref-capture-keys))
         :org-hd-marker (when (equal interactive-capture '(16))
                          (point-marker))
         :url url)))

(defun org-capture-ref-capture-doi (doi &optional interactive-capture)
  "Capture DOI using `org-capture-ref-capture-template'.
With prefix argument, use interactive version of the template.
With `\\[universal-argument] \\[universal-argument]' argument, update heading at point."
  (interactive "sDOI: \nP")
  (unless org-capture-ref-capture-template-set-p
    (user-error "Please, set default capture template with `org-capture-ref-set-capture-template'"))
  (org-protocol-capture
   (list :template (if interactive-capture
                       (car org-capture-ref-capture-keys)
                     (cadr org-capture-ref-capture-keys))
         :org-hd-marker (when (eq interactive-capture '(16))
                          (point-marker))
         :url (format "https://doi.org/%s" doi))))

(defun org-capture-ref-capture-at-point (interactive-capture)
  "Capture object at point using `org-capture-ref-capture-template'
With prefix argument, use interactive version of the template."
  (interactive "P")
  (unless org-capture-ref-capture-template-set-p
    (user-error "Please, set default capture template with `org-capture-ref-set-capture-template'"))
  (org-protocol-capture
   (list :template (if interactive-capture
                       (car org-capture-ref-capture-keys)
                     (cadr org-capture-ref-capture-keys))
         :buffer-marker (point-marker))))

(provide 'org-capture-ref)
;;; org-capture-ref.el ends here
