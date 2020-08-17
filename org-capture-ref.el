;;; org-capture-ref.el --- Extract bibtex info from captured websites  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
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
(require 'bibtex)

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

(defcustom org-capture-ref-get-bibtex-functions '(org-capture-ref-get-bibtex-url-from-capture-data
				   org-capture-ref-get-bibtex-howpublished-from-url
                                   org-capture-ref-set-default-type
                                   org-capture-ref-set-access-date
				   org-capture-ref-get-bibtex-from-elfeed-data
				   org-capture-ref-parse-generic
                                   org-capture-ref-get-bibtex-github
                                   org-capture-ref-get-bibtex-youtube-watch
                                   org-capture-ref-get-bibtex-habr)
  "Functions used to generate bibtex entry for captured link.

Each function will be called without arguments in sequence.
The functions are expected to use `org-capture-ref-set-bibtex-field'
and `org-capture-ref-set-capture-info'. to set the required bibtex
fields. `org-capture-ref-get-bibtex-field' and `org-capture-ref-get-capture-info' can
be used to retrieve information about the captured link.
Any function can throw an error and abort the capture process.
Any function can throw `:finish'. All the remaining functions from
this list will not be called then.

If any of the listed functions modifies :key field of the `org-capture-ref-bibtex-alist',
the field will be overwritten by functions from `org-capture-ref-generete-key-functions'."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-get-bibtex-from-elfeed-functions '(org-capture-ref-get-bibtex-generic-elfeed
					       org-capture-ref-get-bibtex-habr-elfeed-fix-title
                                               org-capture-ref-get-bibtex-rgoswami-elfeed-fix-author
                                               org-capture-ref-get-bibtex-reddit-elfeed-fix-howpublished)
  "Functions used to generate BibTeX entry from elfeed entry data defined in `:elfeed-data' field of the `org-protocol' capture query.

This variable is only used if `org-capture-ref-get-bibtex-from-elfeed-data' is listed in `org-capture-ref-get-bibtex-functions'.
The functions must follow the same rules as `org-capture-ref-get-bibtex-functions', but will be called with a single argument - efleed entry object.

These functions will only be called if `:elfeed-data' field is present in `:query' field of the `org-store-link-plist'.")

(defcustom org-capture-ref-clean-bibtex-hook '()
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
  'group org-capture-ref)

(defcustom org-capture-ref-get-formatted-bibtex-functions '(org-capture-ref-get-formatted-bibtex-default)
  "Functions used to format BiBTeX entry string.

Each function will be called without arguments in sequence.
`org-capture-ref-get-bibtex-field' and `org-capture-ref-get-capture-info' can
be used to retrieve information about the captured link.
Return value of the first function returning non-nil will be used as final format."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-generate-key-functions '(org-capture-ref-generate-key-from-doi
				     org-capture-ref-generate-key-from-url)
  "Functions used to generate citation key.
The functions will be called in sequence until any of them returns non-nil value."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-check-bibtex-functions '(org-capture-ref-check-url
				     org-capture-ref-check-key)
  "Functions used to check the validity of generated BiBTeX.
  
The functions are called in sequence without arguments.
Any function can throw an error and abort the capture process.
Any function can throw `:finish'. All the remaining functions from
this list will not be called then."
  :type 'hook
  :group 'org-capture-ref)

(defcustom org-capture-ref-message-functions '(org-capture-ref-message-emacs
				org-capture-ref-message-qutebrowser)
  "List of functions used to report the progress/errors during capture.
The functions must accept one or two arguments: message and severity.
Severity is one of symbols `info', `warning', `error'.")

;; Customisation for default functions

(defcustom org-capture-ref-field-regexps '((:doi . ("scheme=\"doi\" content=\"\\([^\"]*\\)\""
				     "citation_doi\" content=\"\\([^\"]*\\)\""
				     "data-doi=\"\\([^\"]*\\)\""
				     "content=\"\\([^\"]*\\)\" name=\"citation_doi"
				     "objectDOI\" : \"\\([^\"]*\\)\""
				     "doi = '\\([^']*\\)'"
				     "\"http://dx.doi.org/\\([^\"]*\\)\""
				     "/doi/\\([^\"]*\\)\">"
				     "doi/full/\\(.*\\)&"
				     "doi=\\([^&]*\\)&amp"))
                            (:year . ("<[a-z].+ class=\\(.?+date.[^>]*\\)>\\([[:ascii:][:nonascii:]]*?\\)</[a-z].+>"))
                            (:author . ("\\(?:<meta name=\"author\" content=\"\\(.+\\)\" ?/?>\\)\""
					"\\(?:<[^>]*?class=\"author[^\"]*name\"[^>]*>\\([^<]+\\)<\\)"))
                            (:title . ("<title.?+?>\\([[:ascii:][:nonascii:]]*?\\|.+\\)</title>")))
  "Alist holding regexps used by `org-capture-ref-parse-generic' to populate common BiBTeX fields from html.
Keys of the alist are the field names (example: `:author') and the values are lists or regexps.
The regexps are searched one by one in the html buffer and the group 1 match is used as value in the BiBTeX field."
  :group 'org-capture-ref
  :type '(alist :key-type symbol :value-type (list string)))

(defcustom org-capture-ref-default-type "misc"
  "Default BiBTeX type of the captured entry."
  :group 'org-capture-ref
  :type 'string)

(defcustom org-capture-ref-default-bibtex-template "@${:type}{${:key},
      title        = {${:title}},
      author       = {${:author}},
      howpublished = {${:howpublished}},
      url          = {${:url}},
      year         = {${:year}},
      keywords     = {${:keywords}},
      note         = {Online; accessed ${:urldate}}
      }"
  "Default template used to format BiBTeX entry.
If a keyword from the template is missing, it will remain empty.")

;;; API

(defun org-capture-ref-get-buffer ()
  "Return buffer containing contents of the captured link.

Retrieve the contents first if necessary.
This calls `org-capture-ref-get-buffer-functions'."
  (let ((buffer (or org-capture-ref--buffer
		    (run-hook-with-args-until-success 'org-capture-ref-get-buffer-functions))))
    (unless (buffer-live-p buffer) (org-capture-ref-message (format "<org-capture-ref> Failed to get live link buffer. Got %s" buffer) 'error))
    (setq org-capture-ref--buffer buffer)))

(defun org-capture-ref-get-bibtex-field (field)
  "Return the value of the BiBTeX FIELD or nil the FIELD is not set.
  
FIELD must be a symbol like `:author'.
See `org-capture-ref--bibtex-alist' for common field names."
  (alist-get field org-capture-ref--bibtex))

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

(defun org-capture-ref-set-bibtex-field (field val)
  "Set BiBTeX FIELD to VAL.
  
FIELD must be a symbol like `:author'.
See `org-capture-ref--bibtex-alist' for common field names."
  (setf (alist-get field org-capture-ref--bibtex) val))

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

;; Getting BiBTeX

(defun org-capture-ref-get-bibtex-from-elfeed-data ()
  "Run `org-capture-ref-get-bibtex-from-elfeed-functions'."
  (require 'elfeed)
  (let ((elfeed-entry (org-capture-ref-get-capture-info '(:query :elfeed-data))))
    (when elfeed-entry
      (run-hook-with-args 'org-capture-ref-get-bibtex-from-elfeed-functions elfeed-entry))))

(defun org-capture-ref-parse-generic ()
  "Generic parser for the captured html.
Sets BiBTeX fields according to `org-capture-ref-field-regexps'.
Existing BiBTeX fields are not modified."
  ;; Do not bother is everything is already set.
  (unless (-all-p #'org-capture-ref-get-bibtex-field (mapcar #'car org-capture-ref-field-regexps))
    (with-current-buffer (org-capture-ref-get-buffer)
      (dolist (alist-elem org-capture-ref-field-regexps)
	(let ((key (car alist-elem))
	      (regexps (cdr alist-elem)))
          (unless (org-capture-ref-get-bibtex-field key)
            (catch :found
              (dolist (regex regexps)
		(goto-char (point-min))
		(when (re-search-forward regex  nil t)
		  (org-capture-ref-set-bibtex-field key (match-string 1))
		  (throw :found t))))))))))

(defun org-capture-ref-get-bibtex-from-first-doi ()
  "Generate BiBTeX using first DOI record found in html.
Use `doi-utils-doi-to-bibtex-string' to retrieve the BiBTeX record."
  (when (alist-get :doi org-capture-ref-field-regexps)
    (let ((org-capture-ref-field-regexps (list (assq :doi org-capture-ref-field-regexps))))
      (org-capture-ref-parse-generic))
    (let ((doi (org-capture-ref-get-bibtex-field :doi)))
      (when doi
	(let ((bibtex-string (with-demoted-errors
				 (doi-utils-doi-to-bibtex-string doi))))
          (when bibtex-string
	    (org-capture-ref-clean-bibtex bibtex-string 'no-hooks)))))))

(defun org-capture-ref-get-bibtex-url-from-capture-data ()
  "Get the `:url' using :link data from capture."
  (let ((url (org-capture-ref-get-capture-info :link)))
    (when url (org-capture-ref-set-bibtex-field :url url))))

(defun org-capture-ref-get-bibtex-howpublished-from-url ()
  "Generate `:howpublished' field using `:url' BiBTeX field.
The generated value will be the website name."
  (let ((url (or (org-capture-ref-get-bibtex-field :url))))
    (when url
      (string-match "\\(?:https?://\\)?\\(?:www\\.\\)?\\([^/]+\\)\\.[^/]+/?" url)
      (when (match-string 1 url)
	(org-capture-ref-set-bibtex-field :howpublished (upcase (match-string 1 url)))))))

(defun org-capture-ref-set-default-type ()
  "Set `:type' of the BiBTeX entry to `org-capture-ref-default-type'."
  (org-capture-ref-set-bibtex-field :type org-capture-ref-default-type))

(defun org-capture-ref-set-access-date ()
  "Set `:urldate' field of the BiBTeX entry to now."
  (org-capture-ref-set-bibtex-field :urldate (format-time-string "%d %B %Y")))

(defun org-capture-ref-get-bibtex-github ()
  "Parse Github link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "github\\.com" link)
      (with-current-buffer (org-capture-ref-get-buffer)
        ;; Fix URL
        (when (string-match "^\\(.+\\)/tree/[a-zA-Z0-9]+$" link)
          (org-capture-ref-set-bibtex-field :url (match-string 1 link)))
	;; Find author
        (when (string-match "\\(?:https://\\)?github\\.com/\\([^/]+\\)" link)
          (org-capture-ref-set-bibtex-field :author (match-string 1 link)))
	;; find title
	(goto-char (point-min))
	(when (re-search-forward "<title>\\([^>]+\\)</title>" nil t)
	  (let ((title (match-string 1)))
            (when (string-match "^\\(.+\\) at [0-9a-zA-Z]\\{20,\\}$" title)
	      (setq title (match-string 1 title)))
            (org-capture-ref-set-bibtex-field :title title)))
        (org-capture-ref-set-bibtex-field :howpublished "Github")))))

(defun org-capture-ref-get-bibtex-youtube-watch ()
  "Parse Youtube watch link and generate bibtex entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (string-match "youtube\\.com/watch" link)
      (with-current-buffer (org-capture-ref-get-buffer)
	;; Find author
	(goto-char (point-min))
	(when (re-search-forward "channelName\":\"\\([^\"]+\\)\"" nil t)
	  (let ((channel-name (match-string 1)))
	    (org-capture-ref-set-bibtex-field :author channel-name)))
	;; Find title
	(goto-char (point-min))
	(when (re-search-forward "title\":\"\\([^\"]+\\)\"" nil t)
	  (let ((title (match-string 1)))
	    (org-capture-ref-set-bibtex-field :title title)))
	;; Find year
	(goto-char (point-min))
	(when (re-search-forward "publishDate\":\"\\([^\"]+\\)\"" nil t)
	  (let ((year (match-string 1)))
	    (string-match "[0-9]\\{4\\}" year)
            (org-capture-ref-set-bibtex-field :year (match-string 0 year))))))))

(defun org-capture-ref-get-bibtex-habr ()
  "Parse Habrahabr link and generate BiBTeX entry."
  (when-let ((link (org-capture-ref-get-bibtex-field :url)))
    (when (s-match "habr\\.com" link)
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
            (org-capture-ref-set-bibtex-field :title title)))
	;; Find year
	(goto-char (point-min))
	(when (re-search-forward "datePublished\": \"\\([^\"]+\\)\"" nil t)
	  (let ((year (match-string 1)))
	    (string-match "[0-9]\\{4\\}" year)
            (org-capture-ref-set-bibtex-field :year (match-string 0 year))))))))

;; Getting BiBTeX from elfeed entries

(defun org-capture-ref-get-bibtex-generic-elfeed (entry)
  "Parse generic elfeed capture and generate bibtex entry."
  (org-capture-ref-set-bibtex-field :url (elfeed-entry-link entry))
  (let ((authors (plist-get (elfeed-entry-meta entry) :authors)))
    (setq authors (mapcar #'cadr authors))
    (if authors
	(org-capture-ref-set-bibtex-field :author (s-join ", " authors))
      ;; fallback to feed title
      (org-capture-ref-set-bibtex-field :author (elfeed-feed-title (elfeed-entry-feed entry)))))
  (org-capture-ref-set-bibtex-field :title (elfeed-entry-title elfeed-entry))
  (org-capture-ref-set-bibtex-field :keywords (s-join ", " (plist-get (elfeed-entry-meta entry) :categories)))
  (org-capture-ref-set-bibtex-field :year (format-time-string "%Y" (elfeed-entry-date elfeed-entry))))

(defun org-capture-ref-get-bibtex-habr-elfeed-fix-title (entry)
  "Fix title in habr elfeed entries.
This function is expected to be ran after `org-capture-ref-bibtex-generic-elfeed'."
  ;; Habr RSS adds indication if post is translated or from sandbox,
  ;; but it is not the case in the website. Unifying to make it
  ;; consistent.
  (when (s-match "habr\\.com" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :title (s-replace-regexp "^\\[[^]]+\\][ ]*" "" (org-capture-ref-get-bibtex-field :title)))))

(defun org-capture-ref-get-bibtex-rgoswami-elfeed-fix-author (entry)
  "Populate author for https://rgoswami.me"
  (when (s-match "rgoswami\\.me" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :author "Rohit Goswami")))

(defun org-capture-ref-get-bibtex-reddit-elfeed-fix-howpublished (entry)
  "Mention subreddit in :howpublished."
  (when (s-match "reddit\\.com" (org-capture-ref-get-bibtex-field :url))
    (org-capture-ref-set-bibtex-field :howpublished
		       (format "%s:%s"
			       (org-capture-ref-get-bibtex-field :howpublished)
                               (org-capture-ref-get-bibtex-field :keywords)))))

;; Generating cite key

(defun org-capture-ref-generate-key-from-url ()
  "Generate citation key from URL."
  (when-let (url (org-capture-ref-get-bibtex-field :url))
    (setq url (s-replace-all '(("https?://\\(www\\.?\\)?" . "")
			       ("/" . "-"))
                             url))
    url))

(defun org-capture-ref-generate-key-from-doi ()
  "Generate citation key from DOI."
  (when-let ((doi (org-capture-ref-get-bibtex-field :doi)))
    (s-replace "/" "-" doi)))

;; Formatting BibTeX entry

(defun org-capture-ref-get-formatted-bibtex-default ()
  "Default BiBTeX formatted."
  (s-format org-capture-ref-default-bibtex-template
	    (lamdba (key)
		    (or (org-capture-ref-get-bibtex-field key)
			""))
            org-capture-ref--bibtex-alist))

;;; Message functions

(defun org-capture-ref-message-emacs (msg &optional severity)
  "Show message in Emacs."
  (pcase severity
    (`error (error msg))
    (`warning (warn msg))
    (_ (message msg))))

(defun org-capture-ref-message-qutebrowser (msg &optional severity)
  "Show message in qutebrowser assuming that qutebrowser fifo is
avaible in :query -> :qutebrowser-fifo capture info."
  (when-let  ((fifo (org-capture-ref-get-capture-info '(:query :html))))
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
  (run-hook-with-args org-capture-ref-message-functions msg severity))

;;; Verifying BiBTeX to be suitable for Org environment

(defun org-capture-ref-check-regexp (search-string &optional show-match-p)
  "Check if SEARCH-STRING exists in org files.
SEARCH-STRING and list of searched files follows the rules for `org-search-view'.
If SHOW-MATCH-P is non-nil, show the match or agenda search with all matches."
  (org-search-view nil search-string)
  (goto-char (point-min))
  (let (headlines)
    (while (< (point) (point-max))
      (when (get-text-property (point) 'org-hd-marker) (push (get-text-property (point) 'org-hd-marker) headlines))
      (goto-char (next-single-char-property-change (point) 'org-hd-marker)))
    (cond (length headlines)
	  (0 t)
          (1 (when show-match-p
               (switch-to-buffer (marker-buffer (car headlines)))
               (goto-char (car headlines))
               (org-reveal))
             (org-capture-ref-message (format "%s found in org files" search-string) 'error))
          (_ (unless show-match-p (kill-buffer))
             (org-capture-ref-message (format "%s found in org files" search-string) 'error)))))

(defun org-capture-ref-check-key ()
  "Check if `:key' already exists.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (org-capture-ref-check-regexp (format "{^:ID:[ \t]+%s}" (org-capture-ref-get-bibtex-field :key)) (org-capture-ref-get-capture-info :immediate-finish)))

(defun org-capture-ref-check-url ()
  "Check if `:url' already exists.
It is assumed that `:url' is captured into :SOURCE: property.
Show the matching entry unless `:immediate-finish' is set in the
capture template."
  (org-capture-ref-check-regexp (format "{^:Source:[ \t]+%s}" (org-capture-ref-get-bibtex-field :url)) (org-capture-ref-get-capture-info :immediate-finish)))

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

;;; Main capturing routine

(defun org-capture-ref-reset-state ()
  "Refresh all the internal variables for fresh capture."
  (setq org-capture-ref--buffer nil
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
    (dolist (field (bibtex-parse-entry))
      (pcase (intern (concat ":" (car field)))
	(':=type= (org-capture-ref-set-bibtex-field :type (cdr field)))
        (':=key= (org-capture-ref-set-bibtex-field :key (cdr field)))
        (key (org-capture-ref-set-bibtex-field key (cdr field)))))
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
  (unless (run-hook-with-args-until-success 'org-capture-ref-generate-key-functions)
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
  (org-capture-ref-message "Capturing BiBTeX...")
  (with-demoted-errors "Error while extracting BiBTeX: %S"
    (org-capture-ref-reset-state)
    (org-capture-ref-get-bibtex)
    (org-capture-ref-set-bibtex-field :key (org-capture-ref-generate-key))
    (org-capture-ref-set-bibtex-field :bibtex-string (org-capture-ref-format-bibtex))
    (org-capture-ref-check-bibtex)
    (org-capture-ref-message "Capturing BiBTeX... done"))
  (when (buffer-live-p org-capture-ref--buffer) (kill-buffer org-capture-ref--buffer))
  "")

(provide 'org-capture-ref)
;;; org-capture-ref.el ends here
