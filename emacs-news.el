;; [[file:emacs-news-code.org::export-ascii][export-ascii]]
(setq org-ascii-links-to-notes nil)
;; export-ascii ends here

;; [[file:emacs-news-code.org::reddit][reddit]]
(defvar my-reddit-upvoted-json "https://www.reddit.com/user/emacsnews/upvoted.json" "JSON for upvoted posts.")
(require 'helm-utils)

(defvar my-emacs-news-reddit-client-id "your-client-id")
(defvar my-emacs-news-reddit-client-secret "your-client-secret")
(defvar my-emacs-news-reddit-username "your-username")
(defvar my-emacs-news-reddit-password "your-password")
(defvar my-emacs-news-reddit-user-agent "EmacsNews/1.0 by u/sachac")
(defvar my-emacs-news-reddit-access-token nil)
(defvar my-emacs-news-reddit-token-expires nil)

(defun my-emacs-news-reddit-authenticate ()
  "Authenticate with Reddit using OAuth password grant"
  (let* ((auth-string (concat my-emacs-news-reddit-client-id ":" my-emacs-news-reddit-client-secret))
         (encoded-auth (base64-encode-string auth-string))
         (url-request-method "POST")
         (url-request-data
          (concat "grant_type=password"
                  "&username=" (url-hexify-string my-emacs-news-reddit-username)
                  "&password=" (url-hexify-string my-emacs-news-reddit-password)))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " encoded-auth))
            ("Content-Type" . "application/x-www-form-urlencoded")
            ("User-Agent" . ,my-emacs-news-reddit-user-agent))))
    (with-current-buffer
        (url-retrieve-synchronously "https://www.reddit.com/api/v1/access_token")
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (token (cdr (assoc 'access_token response)))
             (expires-in (cdr (assoc 'expires_in response))))
        (setq my-emacs-news-reddit-access-token token)
        (setq my-emacs-news-reddit-token-expires (+ (float-time) expires-in))
        (message "Reddit authentication successful")))))

(defun my-emacs-news-reddit-get-upvoted-posts (&optional limit after)
  "Get your upvoted posts using OAuth
LIMIT: number of posts to retrieve (default 25, max 100)
AFTER: fullname of a thing for pagination (e.g., 't3_abc123')"
  (unless (and my-emacs-news-reddit-access-token
               (< (float-time) my-emacs-news-reddit-token-expires))
    (my-emacs-news-reddit-authenticate))
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " my-emacs-news-reddit-access-token))
            ("User-Agent" . ,my-emacs-news-reddit-user-agent)))
         (query-params
          (concat "?limit=" (number-to-string (or limit 25))
                  (when after (concat "&after=" (url-hexify-string after)))))
         (url (concat "https://oauth.reddit.com/user/" my-emacs-news-reddit-username "/upvoted" query-params)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun my-reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00")))
				after
				page
				data
				page
        results)
		(catch 'done
			(while t
				(setq data (assoc-default "data" (my-emacs-news-reddit-get-upvoted-posts 50 after) 'string=))
				(setq page
							(mapconcat
							 (lambda (item)
								 (let* ((o (assoc-default 'data item))
												(title (assoc-default 'title o))
												(url (helm-html-decode-entities-string (assoc-default 'url o)))
												(date (seconds-to-time (assoc-default 'created_utc o)))
												(permalink (concat "https://www.reddit.com" (assoc-default 'permalink o)))
												(num-comments (assoc-default 'num_comments o 'eq 0)))
									 (when (time-less-p threshold date)
										 (if (and (> num-comments 0) (not (string-match "reddit\\.com" url)))
												 (format "- %s (%s)\n"
																 (org-link-make-string (url-unhex-string (if (string-match "^/" url)
																																						 (concat "https://www.reddit.com" url)
																																					 url))
																											 title)
																 (org-link-make-string (url-unhex-string (if (string-match "^/" permalink)
																																						 (concat "https://www.reddit.com" permalink)
																																					 permalink))
																											 "Reddit"))
											 (format "- %s\n" (org-link-make-string (url-unhex-string (if (string-match "^/" url)
																																										(concat "https://www.reddit.com" url)
																																									url))
																															title))))))
							 (assoc-default 'children data) ""))
				(setq after (assoc-default 'after data))
				(if (or (string= page "") (null after))
						(throw 'done results))
				(setq results (concat page "\n" results))))
    results))
;;  (my-reddit-list-upvoted "-mon")
;; reddit ends here

;; [[file:emacs-news-code.org::youtube][youtube]]
(defvar my-emacs-news-playlist "https://www.youtube.com/playlist?list=PL4th0AZixyREOtvxDpdxC9oMuX7Ar7Sdt"
  "Public playlist with recent videos to include in Emacs News.")

(defun my-emacs-news-get-videos (playlist)
  (json-parse-string
       (concat "["
               (mapconcat
                'identity
                (split-string
                 (shell-command-to-string
                  (concat "yt-dlp -j --flat-playlist " (shell-quote-argument my-emacs-news-playlist) " 2> /dev/null"))
                 "\n" t)
                ",")
               "]")
       :object-type 'plist))

(defun my-save-new-items (file alist)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let* ((old-list (read (current-buffer)))
           (date (format-time-string "%Y-%m-%d"))
           (new-items (seq-difference (mapcar 'car alist)
                                      (mapcar 'car old-list)))
           (combined (append (mapcar (lambda (o) (append (list o date) (cdr (assoc o alist))))
                                     new-items)
                             old-list))
           (print-length nil))
      (erase-buffer)
      (insert "("
              (mapconcat 'prin1-to-string
                          combined
                          "\n")
              ")")
      (save-buffer)
      combined)))

(defun my-emacs-news-youtube-playlist (playlist &optional start-date end-date)
  (let* ((videos (my-emacs-news-get-videos playlist))
         (combined (my-save-new-items "video-list.el"
                                      (mapcar (lambda (o) (cons (plist-get o :id) o))
                                              videos))))
    (mapconcat
     (lambda (o)
       (if (numberp (plist-get o :duration))
           (format "- %s (%s)\n"
                   (org-link-make-string
                    (concat "https://www.youtube.com/watch?v=" (plist-get o :id))
                    (plist-get o :title))
                   (format-seconds "%.2h:%z%.2m:%.2s" (plist-get o :duration)))
         (format "- %s\n"
                   (org-link-make-string
                    (concat "https://www.youtube.com/watch?v=" (plist-get o :id))
                    (plist-get o :title))
                   )))
     (mapcar 'cddr
             (seq-filter (lambda (o) (and
                                      (or (null start-date) (not (string< (cadr o) start-date)))
                                      (or (null end-date) (string< (cadr o) end-date))))
                         combined))
     "")))
;; youtube ends here

;; [[file:emacs-news-code.org::sort][sort]]
(defun my-org-sort-list-in-custom-order (order)
  "Sort the current Org list so that items are in the specified order.
  ORDER is a list of regexps."
  (org-sort-list
   nil ?f
   (lambda ()
     (let ((case-fold-search t)
           (item
            (when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
              (org-sort-remove-invisible (buffer-substring (match-end 0) (point-at-eol))))))
       (or (cl-position item order :test (lambda (a b) (string-match b a))) (1+ (length order)))))
   '<))

(defvar my-emacs-news-sort-order
  '("Help wanted"
		"Security"
    ("Upcoming events" "meetup")
    ("Beginner" "beginner")
    ("Emacs configuration" "config")
    ("Emacs Lisp" "emacs lisp")
    ("Appearance" "theme\\|theming")
    ("Navigation" "search\\|tab bar")
		("Hyperbole" "hyperbole")
    "TRAMP"
    ("Dired" "dired")
    "Writing"
    ("Denote" "denote")
    ("Org Mode" "org[ -]?mode\\|\\<org\\>\\|\\box-\\|\\bob-")
    ("Completion" "company\\|helm\\|selectrum\\|completion\\|vertico")
    ("Coding" "java\\|php\\|sql\\|cider\\|git\\|programming\\|scala\\|python\\|eglot\\|lsp")
    "Math"
    "Shells"
    ("Web" "eww")
    ("Chat" "mastodon\\|IRC\\|erc")
    ("Mail, news, and chat" "gnus\\|rmail\\|mail\\|notmuch\\|mu4e\\|elfeed")
    ("Evil mode" "evil")
    ("Spacemacs" "spacemacs")
    ("Doom Emacs" "doom")
    "Multimedia"
    ("EXWM" "exwm")
    "Fun"
		("AI" "chatgpt\\|\\bai\\b\\|\\bllm\\b\\|openai\\|gemini")
    ("Community" "Weekly tips")
    "Other"
    "Emacs development"
    "Discussion"
    "Outside Emacs"
    "New packages"))
(defun my-emacs-news-sort-order-headings ()
  (mapcar (lambda (o) (if (stringp o) o (car o))) my-emacs-news-sort-order))
(defun my-emacs-news-sort-list ()
  (interactive)
  (goto-char (org-list-get-top-point (org-list-struct)))
  (my-org-sort-list-in-custom-order (my-emacs-news-sort-order-headings)))
;; sort ends here

;; [[file:emacs-news-code.org::*Experimenting with using LLMs for link categorization][Experimenting with using LLMs for link categorization:1]]
(defvar my-emacs-news-link-categories nil
  "List of (url category).")

(defun my-emacs-news-read-categories ()
  (interactive)
  (setq my-emacs-news-link-categories (json-read)))

(defun my-emacs-news-get-categories-from-spookfox ()
  (interactive)
  (setq my-emacs-news-link-categories (my-spookfox-ai-parse-latest-json)))

(defvar my-emacs-news-categorize-prompt
  "Categorize links into standardized categories in Org Mode format. Input: Raw list of Emacs-related links. Output: JSON array of [URL, category] pairs, where each URL is mapped to exactly one category from the given list. Standard categories:

- Beginner: Basic tutorials, introductory resources
- Emacs Lisp: Elisp packages, libraries, DWIM commands, snippets, elisp programming
- Emacs development: Core Emacs commits, NEWS file updates, development discussions
- Navigation: Window management, buffer navigation, frame handling, tab-bar, registers
- Appearance: Themes, modelines, visual customization, syntax highlighting
- Denote: Denote note-taking system
- Org Mode: Org-mode features, org-roam, agenda, capture templates, org development
- Writing: Document creation, note-taking, journaling, translation, grammar checking
- Dired: File management in Emacs
- Coding: Programming languages support, LSP, eglot, tree-sitter, development environments, Magit, git, version control workflows
- Shells: Eshell, terminal emulation, shell integration
- Completion: Completion frameworks, auto-completion
- Web: EWW browser, web browsing in Emacs, working with web browsers
- Mail, news, and chat: Email clients (mu4e, gnus), RSS (elfeed), messaging (Signal, Slack)
- Multimedia: Audio, video, speech-to-text,, image handling
- AI: LLM integration, AI assistants (Ollama, Claude, GPT), AI-powered tools
- Fun: Games, entertainment, amusements
- Community: People-related
- Other: Miscellaneous items

Consider only the first link on each line. Try to categorize it based on the text of the title.

Example input:

- [[https://github.com/gggion/duckdb-query.el][duckdb-query.el - DuckDB SQL/Elisp: query parquet, CSV, org-tables, alists in the same SQL statement seamlessly]] ([[https://www.reddit.com/r/emacs/comments/1qj00l1/duckdbqueryel_bidirectional_duckdb_sqlelisp/][Reddit]], [[https://www.reddit.com/r/emacs/comments/1qppfeh/duckdbqueryel_v070_10x_faster_queries_via/][Reddit]])
- [[https://protesilaos.com/codelog/2026-01-17-emacs-doric-themes-0-6-0/][Protesilaos Stavrou: Emacs: doric-themes version 0.6.0]]
- [[https://rony.novaparis.art.br/blog/primeiros-passos-no-gnu-emacs.html][Primeiros passos no GNU Emacs]] ([[https://organica.social/@iui/115894477036431409][@iui@organica.social]])

Example mappings:

[
  [\"https://github.com/gggion/duckdb-query.el\", \"Emacs Lisp\"],
  [\"https://protesilaos.com/codelog/2026-01-17-emacs-doric-themes-0-6-0/\", \"Appearance\"],
  [\"https://rony.novaparis.art.br/blog/primeiros-passos-no-gnu-emacs.html\", \"Beginner\"]
]
")

(defun my-emacs-news-spookfox-ai-categorize (&optional read-answer)
  (interactive (list current-prefix-arg))
  (setq my-emacs-news-link-categories
        (if read-answer
            (my-spookfox-ai-parse-latest-json)
          (my-spookfox-ai-send-text-and-get-json
           (concat my-emacs-news-categorize-prompt
                   "\n\n###\n\n"
                   (my-org-subtree-text))))))

(defun my-emacs-news-guess-category ()
  (save-excursion
    (when (re-search-forward org-any-link-re (line-end-position) t)
      (let* ((link (org-element-context))
             (description
              (buffer-substring-no-properties
               (org-element-property :contents-begin link)
               (org-element-property :contents-end link)))
             (url (org-element-property :raw-link link)))
        (or
         (car (seq-find (lambda (o)
                          (and (listp o)
                               (string-match (cadr o) description)))
                        my-emacs-news-sort-order))
         (car (assoc-default url my-emacs-news-link-categories 'string=)))))))
;; Experimenting with using LLMs for link categorization:1 ends here

;; [[file:emacs-news-code.org::*Experimenting with using LLMs for link categorization][Experimenting with using LLMs for link categorization:2]]
(defun my-emacs-news-auto-categorize ()
  (interactive)
  (while (and (looking-at "^- \\[\\[") (not (looking-at "^- New package")))
    (let ((category (my-emacs-news-guess-category)))
      (if category
          (my-org-move-current-item-to-category (concat category ":"))
        (forward-line 1)
        (goto-char (line-beginning-position))))))
;; Experimenting with using LLMs for link categorization:2 ends here

;; [[file:emacs-news-code.org::my-update-package-list][my-update-package-list]]
(defvar my-package-list-file "~/sync/emacs-news/package-list.el")

(defun my-read-sexp-from-file (filename)
  (with-temp-buffer (insert-file-contents filename)
                    (goto-char (point-min)) (read (current-buffer))))

(defun my-update-package-list (&optional date)
	"Update the list of packages. Mark new packages with DATE."
  (interactive (list (format-time-string "%Y-%m-%d")))
	(setq date (or date (format-time-string "%Y-%m-%d")))
  (let* ((archives (my-get-current-packages date))
         (old-list (my-read-sexp-from-file my-package-list-file)))
    (mapc (lambda (o)
            (let* ((old-entry (assoc-default (car o) old-list))
                   (new-archives
                    (seq-difference
                     (mapcar 'cadr (cdr o))
                     (mapcar 'car (car (assoc-default (car o) old-list))))))
              (cond
               ((null (assoc (car o) old-list))
                ;; new package, add it to the list
                (setq old-list
                      (cons (list (car o)
                                  (mapcar
                                   (lambda (entry) (cons (cadr entry) date))
                                   (cdr o)))
                            old-list)))
               (new-archives
								;; existing package added to a different repository
                (setf old-entry
                      (append
                       (mapcar (lambda (archive) (cons archive date))
                               new-archives)
                       old-entry
                       nil))))))
          archives)
		;; Save to file, one package per line
    (with-temp-file my-package-list-file
      (insert "("
              (mapconcat #'prin1-to-string
                         old-list
                         "\n")
              ")"))
    old-list))
;; my-update-package-list ends here

;; [[file:emacs-news-code.org::my-get-current-packages][my-get-current-packages]]
(defun my-get-current-packages (date)
  "Return a list of package symbols with the package archive and DATE.
Example entry: `(ack (ack \"gnu\" \"2023-09-03\"))`"
  (seq-group-by 'car
                (seq-mapcat (lambda (f)
                              (let ((base (file-name-base f)))
                                (mapcar
                                 (lambda (entry)
                                   (list (car entry) base date))
                                 (cdr
                                  (with-temp-buffer
                                    (insert-file-contents
                                     (expand-file-name "archive-contents" f))
                                    (goto-char (point-min))
                                    (read (current-buffer)))))))
                            (directory-files
                             (expand-file-name "archives" package-user-dir) t
                             directory-files-no-dot-files-regexp))))
;; my-get-current-packages ends here

;; [[file:emacs-news-code.org::my-list-new-packages][my-list-new-packages]]
(defun my-packages-between (from-date &optional to-date)
	(seq-filter
   (lambda (o)
     (and
			(or (not from-date) (not (string< (cdar (cadr o)) from-date)))
			(or (not to-date) (string< (cdar (cadr o)) to-date))))
   (my-read-sexp-from-file my-package-list-file)))

(defun my-list-new-packages (&optional date)
  (interactive)
  (let ((packages
         (my-describe-packages
          (seq-filter
           (lambda (o)
             (seq-remove (lambda (archive) (string< (cdr archive) date))
                         (cadr o)))
           (my-read-sexp-from-file my-package-list-file)))))
    (if (called-interactively-p 'any)
        (insert packages)
      packages)))
;; my-list-new-packages ends here

;; [[file:emacs-news-code.org::my-describe-packages][my-describe-packages]]
(defun my-describe-packages (list)
  "Return an Org list of package descriptions for LIST."
  (mapconcat
   (lambda (entry)
     (let* ((symbol (car entry))
            (package-desc (assoc symbol package-archive-contents)))
       (if package-desc
           (format "  - %s: %s (%s)"
                   (org-link-make-string (concat "package:" (symbol-name symbol))
                                         (symbol-name symbol))
                   (package-desc-summary (cadr package-desc))
                   (mapconcat
                    (lambda (archive)
                      (pcase (car archive)
                        ("gnu" "GNU ELPA")
                        ("nongnu" "NonGNU ELPA")
                        ("melpa" "MELPA")))
                    (cadr entry)
                    ", "))
         "")))
   list
   "\n"))
;; my-describe-packages ends here

;; [[file:emacs-news-code.org::packages][packages]]
  (defun my-org-package-open (package-name)
    (interactive "MPackage name: ")
    (describe-package (intern package-name)))

(defun my-org-package-export (link description format &optional arg)
  (let* ((package-info (car (assoc-default (intern link) package-archive-contents)))
         (package-source (and package-info (package-desc-archive package-info)))
         (path (format
                (cond
								 ((null package-source) link)
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "https://melpa.org/#/%s")
                 ((string= package-source "nongnu") "https://elpa.nongnu.org/nongnu/%s.html")
                 (t (error 'unknown-source)))
                link))
         (desc (or description link)))
		(if package-source
				(cond
				 ((eq format '11ty) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
				 ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
				 ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
				 ((eq format 'ascii) (format "%s <%s>" desc path))
				 (t path))
			desc)))

  (org-link-set-parameters "package" :follow 'my-org-package-open :export 'my-org-package-export)

  (ert-deftest my-org-package-export ()
    (should
     (string=
      (my-org-package-export "transcribe" "transcribe" 'html)
      "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
      ))
    (should
     (string=
      (my-org-package-export "fireplace" "fireplace" 'html)
      "<a target=\"_blank\" href=\"https://melpa.org/#/fireplace\">fireplace</a>"
      )))
;; packages ends here

;; [[file:emacs-news-code.org::my-announce-elpa-package][my-announce-elpa-package]]
(defun my-announce-elpa-package (package-name)
	"Compose an announcement for PACKAGE-NAME for info-gnu-emacs."
  (interactive (let* ((guess (or (function-called-at-point)
                                 (symbol-at-point))))
                 (require 'finder-inf nil t)
                 ;; Load the package list if necessary (but don't activate them).
                 (unless package--initialized
                   (package-initialize t))
                 (let ((packages
                        (mapcar #'car
                                (seq-filter
                                 (lambda (p)
																	 (seq-find (lambda (entry)
																							 (string= (package-desc-archive entry)
																												"gnu"))
																						 (cdr p)))
                                 package-archive-contents))))
                   (unless (memq guess packages)
                     (setq guess nil))
                   (setq packages (mapcar #'symbol-name packages))
                   (let ((val
                          (completing-read (format-prompt "Describe package" guess)
                                           packages nil t nil nil (when guess
                                                                    (symbol-name guess)))))
                     (list (and (> (length val) 0) (intern val)))))))
  (let ((package (car (assoc-default package-name package-archive-contents))))
    (compose-mail "info-gnu-emacs@gnu.org"
                  (format "New GNU ELPA package: %s - %s"
                          (package-desc-name package)
                          (package-desc-summary package)))
    (message-goto-body)
    (describe-package-1 package-name)
    (message-goto-body)
    (delete-region (point)
                   (progn (re-search-forward " *Summary:") (match-beginning 0)))
    (save-excursion
			(goto-char (point-min))
      (when (re-search-forward "Maintainer: \\(.+\\)" nil t)
        (message-add-header (concat "Reply-To: " user-mail-address ", " (match-string 1))
                            (concat "Mail-Followup-To: " user-mail-address ", " (match-string 1)))))))
;; my-announce-elpa-package ends here

;; [[file:emacs-news-code.org::link-description][link-description]]
(defun my-emacs-news-escape-link-description (s)
  (let ((replace-map '(("\\[" . "")
                       ("\\]" . ":"))))
    (mapc (lambda (rule)
            (setq s (replace-regexp-in-string (car rule) (cdr rule) s)))
          replace-map)
    s))
(use-package xml-rpc)

(defun my-org-list-from-rss (url from-date &optional to-date)
  "Convert URL to an Org list. Return entries between FROM-DATE and TO-DATE.
FROM-DATE and TO-DATE should be strings of the form YYYY-MM-DD."
	(condition-case nil
			(with-current-buffer (url-retrieve-synchronously url)
				(set-buffer-multibyte t)
				(goto-char (point-min))
				(re-search-forward "<\\?xml")
				(goto-char (match-beginning 0))
				(let* ((feed (xml-parse-region (point) (point-max)))
							 (from-time (org-read-date nil t from-date))
							 (to-time (if to-date (org-read-date nil t to-date)))
							 (is-rss (> (length (xml-get-children (car feed) 'entry)) 0)))
					(mapconcat (lambda (link)
											 (format "- %s\n"
															 (org-link-make-string (car link) (replace-regexp-in-string "&amp;" "&" (cdr link)))))
										 (if is-rss
												 (mapcar
													(lambda (entry)
														(cons
														 (xml-get-attribute (car
																								 (or
																									(seq-filter (lambda (x) (string= (xml-get-attribute x 'rel) "alternate"))
																															(xml-get-children entry 'link))
																									(xml-get-children entry 'link))) 'href)
														 (elt (car (xml-get-children entry 'title)) 2)))
													(-filter (lambda (entry)
																		 (let ((entry-date (elt (car (xml-get-children entry 'updated)) 2)))
																			 (and
																				(org-string<= from-date entry-date)
																				(or (null to-date) (string< entry-date to-date)))))
																	 (xml-get-children (car feed) 'entry)))
											 (mapcar (lambda (entry)
																 (cons
																	(caddr (car (xml-get-children entry 'link)))
																	(caddr (car (xml-get-children entry 'title)))))
															 (-filter (lambda (entry)
																					(let ((entry-time (date-to-time (elt (car (xml-get-children entry 'pubDate)) 2))))
																						(and
																						 (not (time-less-p entry-time from-time))
																						 (or (null to-time) (time-less-p entry-time to-time)))))
																				(xml-get-children (car (xml-get-children (car feed) 'channel)) 'item))))
										 "")))
		(error nil)))
;; link-description ends here

;; [[file:emacs-news-code.org::git-news][git-news]]
  (defun my-insert-emacs-news-from-git (date &optional directory)
    (interactive (list (org-read-date)))
    (let ((result
           (shell-command-to-string (format "cd ~/vendor/emacs; git pull > /dev/null; git log --pretty=oneline --after=%s etc/NEWS" (substring date 0 10)))))
      (with-temp-buffer
        (insert result)
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9a-f]+\\) \\(.+\\)$" nil t)
          (replace-match "  - [[https://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=\\1][\\2]]"))
        (setq result (buffer-string)))
      (if (called-interactively-p 'any) (insert result) result)))
(defun my-insert-org-news-from-git (date &optional directory)
    (interactive (list (org-read-date)))
    (let ((result
           (shell-command-to-string (format "cd ~/vendor/org-mode; git pull > /dev/null; git log --pretty=oneline --after=%s etc/ORG-NEWS" (substring date 0 10)))))
      (with-temp-buffer
        (insert result)
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9a-f]+\\) \\(.+\\)$" nil t)
          (replace-match "    - [[https://git.savannah.gnu.org/cgit/emacs/org-mode.git/commit/etc/ORG-NEWS?id=\\1][\\2]]"))
        (setq result (buffer-string)))
      (if (called-interactively-p 'any) (insert result) result)))
;; git-news ends here

;; [[file:emacs-news-code.org::share-news][share-news]]
(defun my-emacs-news-commit-and-push (&optional title)
	(interactive)
	(when (magit-anything-unstaged-p)
		(my-magit-stage-all-and-commit (or title "update")))
	(unwind-protect (magit-push-current-to-pushremote (magit-push-arguments))))

(defvar my-emacs-news-target "info-gnu-emacs@gnu.org")
(defvar my-emacs-news-headers `(("Reply-To" . ,user-mail-address)
																("Mail-Followup-To" . ,user-mail-address)
																("Mail-Reply-To" . ,user-mail-address)))
(defvar my-emacs-news-send-immediately nil "Non-nil means send the e-mail without waiting.")

(defun my-emacs-news-email (&optional target send-immediately)
	(interactive)
	(setq target (or target my-emacs-news-target))
  ;; Draft article
	(let ((org-export-html-preamble nil)
				(org-html-toplevel-hlevel 3)
				(title (org-get-heading))
				text
				output)
		(save-restriction
			(org-narrow-to-subtree)
			(setq text (buffer-substring (point-min) (point-max)))
			(setq output
						(format
						 "<#multipart type=alternative>
<#part type=\"text/plain\" disposition=inline>
%s

This is the plain-text version. There's also an HTML version that might be easier to read. Depending on your mail client, you might have a command, button, or link that can show you the HTML version. If you're in Emacs, look for something that mentions text/html. More info: https://sachachua.com/topic/emacs-news/
<#/part>
<#part type=\"text/html\" disposition=inline>
%s
<#/part>
<#/multipart>
<#part type=\"text/x-org\" disposition=attachment name=\"emacs-news-%s.org\">
%s
<#/part>
"
						 (org-export-string-as text 'my-plain-text t)
						 (org-export-string-as text 'html t)
						 (substring title 0 10)
						 (org-export-string-as text 'org t))))
		(compose-mail target title my-emacs-news-headers)
		(message-goto-body)
		(insert output)
		(when send-immediately
			(message-send-and-exit))))

(defun my-share-emacs-news ()
  "Prepare emacs-tangents e-mail of post, and commit to Git."
  (interactive)
	(require 'magit)
	(my-emacs-news-post-as-recent)
	(let ((html (org-export-as 'html))
				(text (org-export-as 'my-plain-text nil nil t))
				(org-export-html-preamble nil)
				(org-html-toplevel-hlevel 3)
				(title (org-get-heading)))
		(with-temp-file "index.html"
			(insert html))
		(with-temp-file "index.txt"
			(insert text))
		(my-emacs-news-commit-and-push title)
		(my-emacs-news-email)))


(defun my-plain-text-link (link contents info)
  "Custom link transcoder: 'description URL' format."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (description (or contents
													(and (string= type "fuzzy") path)
													path))
         (url (cond
               ((member type '("http" "https"))
                (concat type ":" path))
               ((string= type "file") path)
               (t raw-link))))
		(cond
     ((org-export-custom-protocol-maybe link description 'my-plain-text info))
     (t
			(if description
					(format "%s %s" description url)
				url)))))

(defun my-plain-text-item (item contents info)
  "Transcode an ITEM element with 4-space indentation.
CONTENTS is the contents of the item.
INFO is a plist used as a communication channel."
	(replace-regexp-in-string "^\\(  \\)+" "\\1\\1" (org-ascii-item item contents info)))

;; Define the custom backend
(org-export-define-derived-backend 'my-plain-text 'ascii
  :translate-alist '((link . my-plain-text-link)
                     (item . my-plain-text-item))
  :menu-entry '(?p "Export to Custom Plain Text"
									 ((?p "As plain text buffer" my-plain-text-export-to-buffer))))

(defun my-plain-text-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text buffer."
  (interactive)
  (org-export-to-buffer 'my-plain-text "*My Plain Text Export*"
    async subtreep visible-only body-only ext-plist))

(defun my-plain-text-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text file."
  (interactive)
  (let ((file (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'my-plain-text file
      async subtreep visible-only body-only ext-plist)))
(add-to-list 'org-export-backends 'my-plain-text)
;; share-news ends here

;; [[file:emacs-news-code.org::prepare-news][prepare-news]]
(defcustom emacs-news-rss-feeds nil "List of Emacs feeds"
  :type '(repeat string)
  :group 'emacs-news)
(defvar my-emacs-calendar-csv "~/sync/emacs-calendar/events.csv")

(defun my-emacs-calendar-list ()
  (let ((before-limit (time-add (current-time) (seconds-to-time (* 14 24 60 60)))))
    (mapconcat 'cdr
               (sort
                (delq nil (mapcar
                           (lambda (o)
                             (let ((time (encode-time (parse-time-string
                                                       (if (string-match
                                                            ":"
                                                            (alist-get 'DTSTART o))
                                                           (alist-get 'DTSTART o)
                                                         (concat (alist-get 'DTSTART o) " 0:00"))))))
                               (and (time-less-p (current-time) time)
                                    (time-less-p time before-limit)
                                    (cons time (alist-get 'TEXT o)))))
                           (my-org-table-as-alist (pcsv-parse-file my-emacs-calendar-csv))))
                (lambda (a b) (time-less-p (car a) (car b))))
               "\n")))

(defun my-prepare-emacs-news (date parts)
  (interactive (list (org-read-date nil nil "-mon 0:00") '(refresh-packages reddit rss)))
  (setq date (substring date 0 10))
  (let* ((start-date date)
         (end-date (substring (org-read-date nil nil "++1" nil (org-read-date nil t "-sun 0:00")) 0 10))
         (year (substring end-date 0 4))
         (month (substring end-date 5 7)))
    (when (member 'refresh-packages parts)
      (package-refresh-contents)
      (my-update-package-list date)
      )
    (concat
     (format
      "** %s Emacs news
:PROPERTIES:
:EXPORT_ELEVENTY_PERMALINK: /blog/%s/%s/%s-emacs-news/
:EXPORT_ELEVENTY_FILE_NAME: blog/%s/%s/%s-emacs-news/
:EXPORT_DATE: %s
:END:\n\n"
      end-date
      year
      month
      end-date
      year
      month
      end-date
      (format-time-string "%Y-%m-%dT%T%z"))
     (if (member 'events parts)
         (concat
          "- Upcoming events ([[https://emacslife.com/calendar/emacs-calendar.ics][iCal file]], [[https://emacslife.com/calendar/][Org]]):\n"
          (replace-regexp-in-string "^" "  " (my-emacs-calendar-list))
          "\n")
       "")
     (if (member 'git parts)
         (concat
          "- Emacs development:\n"
          (my-insert-emacs-news-from-git date)
          "\n"
          )
       "")
     (if (member 'git-org parts)
         (concat
          "- Org Mode:\n  - Org development:\n"
          (my-insert-org-news-from-git date)
          "\n"
          ))
     "- Other:\n"
     (if (member 'worg parts)
         (my-org-list-from-rss "https://tracker.orgmode.org/news.rss" (substring date 0 10))
       "")
     (if (member 'rss parts)
         (mapconcat (lambda (feed)
                      (my-org-list-from-rss feed (substring date 0 1)))
                    emacs-news-rss-feeds
                    "")
       "")

     (if (member 'rss-es parts)
         (my-org-list-from-rss "https://planet.emacs-es.org/atom.xml" (substring date 0 10))
       "")
     "\n"
     (if (member 'reddit parts) (my-reddit-list-upvoted (substring date 0 10)) "")
     (if (member 'youtube parts)
         (my-emacs-news-youtube-playlist my-emacs-news-playlist start-date))
		 (if (member 'librehacker parts)
				 (my-emacs-news-parse-gem-librehacker-com date)
			 "")
     "- New packages:\n"
     (my-list-new-packages date)
     "\n\nLinks from "
     (mapconcat (lambda (x) (org-link-make-string (car x) (cdr x)))
                '(("https://www.reddit.com/r/emacs" . "reddit.com/r/emacs")
                  ("https://www.reddit.com/r/orgmode" . "r/orgmode")
                  ("https://www.reddit.com/r/spacemacs" . "r/spacemacs")
;                  ("https://www.reddit.com/r/planetemacs" . "r/planetemacs")
									("https://mastodon.social/tags/emacs" . "Mastodon #emacs")
									("https://bsky.app/hashtag/emacs" . "Bluesky #emacs")
                  ("https://hn.algolia.com/?query=emacs&sort=byDate&prefix&page=0&dateRange=all&type=story" . "Hacker News")
                  ("https://lobste.rs/search?q=emacs&what=stories&order=newest" . "lobste.rs")
									("https://programming.dev/c/emacs?dataType=Post&page=1&sort=New" . "programming.dev")
									("https://lemmy.world/c/emacs" . "lemmy.world")
									("https://lemmy.ml/c/emacs?dataType=Post&page=1&sort=New" . "lemmy.ml")
                  ("https://planet.emacslife.com" . "planet.emacslife.com")
                  ("https://www.youtube.com/playlist?list=PL4th0AZixyREOtvxDpdxC9oMuX7Ar7Sdt" . "YouTube")
                  ("http://git.savannah.gnu.org/cgit/emacs.git/log/etc/NEWS" . "the Emacs NEWS file")
                  ("https://emacslife.com/calendar/" . "Emacs Calendar"))
                ", ")
     ", and "
     (org-link-make-string (concat "https://lists.gnu.org/archive/html/emacs-devel/" (format-time-string "%Y-%m"))
                           "emacs-devel")
     ". Thanks to Andrés Ramírez for emacs-devel links. Do you have an Emacs-related link or announcement? Please e-mail me at [[mailto:sacha@sachachua.com][sacha@sachachua.com]]. Thank you!")))
;; prepare-news ends here

;; [[file:emacs-news-code.org::collect-news][collect-news]]
  (defun my-emacs-news-collect-entries (&optional category-filter)
    "Collect Emacs News by category and put them in another buffer."
    (interactive (list (when current-prefix-arg (read-string "Category: "))))
    (let ((parsed (org-element-parse-buffer))
          category
          result)
      (setq result
            (mapconcat
             (lambda (x)
               (concat "* " (car x) "\n\n"
                       (mapconcat (lambda (y) (concat "- " (cdr y))) (cdr x) "")
                       "\n"))
             (sort
              (seq-filter
               (lambda (o)
                 (if category-filter
                     (and (car o) (string-match category-filter (car o)))
                   'identity))
               (seq-group-by
                'car
                (delq nil
                      (org-element-map parsed '(paragraph)
                        (lambda (x)
                          (let ((contents (org-element-interpret-data x)))
                            (cond
                             ((string-match "^\\(.*\\):[ \t\n]*$" contents)
                              (setq category (match-string-no-properties 1 contents))
                              nil)
                             ((string-match "^\\[" contents)
                              (cons category (substring-no-properties contents))))))))))
              (lambda (a b) (string< (car a) (car b))))
             ""))
      (switch-to-buffer (get-buffer-create "*Emacs News*"))
      (erase-buffer)
      (insert result)
      (org-mode)
      (goto-char (point-min))))
;; collect-news ends here

;; [[file:emacs-news-code.org::detect-dupes][detect-dupes]]
(defvar my-emacs-news-check-duplicates-display 'overlay "*minibuffer or posframe or overlay")
(defvar my-emacs-news-duplicate-overlay nil)
(defun my-emacs-news-check-duplicates ()
  (interactive)
  (let ((end (save-excursion (org-end-of-subtree)))
        (prompt-buffer (when (eq my-emacs-news-check-duplicates-display 'posframe) (get-buffer-create "*Duplicate*")))
        description
        (search-fn (lambda (point search description)
                     (save-excursion
                       (goto-char point)
                       (let (found)
                         (if (re-search-forward (regexp-quote search) nil t)
                             (setq found (point))
                           ;; Does this look like a package name?
                           (let ((case-fold-search nil))
                             (when (string-match "^\\([-\\.a-z0-9]+?\\)\\(:\\|\\.el\\) " description)
                               (when (re-search-forward (concat "\\W" (regexp-quote (match-string 1 description))) nil t)
                                 (setq found (point))))))
                         found))))
        search start
        found
        done
        pos
        context)
    (unwind-protect
        (while (and (not done) (re-search-forward "- \\[\\[\\([^[]+\\)\\]\\[\\([^[]+\\)\\]" end t))
          (setq found nil search (match-string 1) start (match-beginning 0) description (match-string 2))
          (goto-char (match-end 0))
          (let ((p (point)) prompt input)
            ;; Search for the URL
            (setq found (funcall search-fn (match-end 0) search description))
            (while found
              (goto-char found)
              (setq context
                    (concat
                     (string-join (org-get-outline-path t) " > ") "\n"
                     (buffer-substring (line-beginning-position) (line-end-position))))
              (push-mark found)
              (goto-char start)
              (setq prompt (concat context "\n(d)elete, (k)eep, (z)ap next one, e(x)change mark, (n)ext match, (q)quit?"))
							(pcase my-emacs-news-check-duplicates-display
								('posframe
								 ;; (when (eq my-emacs-news-check-duplicates-display 'posframe) (posframe-hide prompt-buffer))
								 (posframe-show prompt-buffer
																:string prompt
																:internal-border-width 2
																:internal-border-color "white"
																:position (point))
								 (setq input (read-char)))
								('minibuffer
								 (setq input (read-char prompt)))
								('overlay
								 (if (overlayp my-emacs-news-duplicate-overlay)
										 (move-overlay my-emacs-news-duplicate-overlay (line-beginning-position) (line-end-position))
									 (setq my-emacs-news-duplicate-overlay (make-overlay (line-beginning-position) (line-end-position))))
								 (overlay-put my-emacs-news-duplicate-overlay 'after-string
															(propertize
															 (concat "\n" prompt)
															 'face '(:box t)))
								 (recenter-top-bottom)
								 (setq input (read-char))))
              (pcase input
								(?k
                 (forward-line 1)
                 (setq found nil))
								(?x
                 (goto-char found)
                 (setq done t found nil))
								(?z
                 (goto-char found)
                 (delete-region (point-at-bol)
																(progn (forward-line 1) (point)))
                 (goto-char start)
                 (setq found nil))
                (?i
                 (my-emacs-news-process-irreal-link))
								(?d
                 (delete-region (point-at-bol)
																(progn (forward-line 1) (point)))
                 (setq found nil))
								(?n
                 ;; Look for the next match
                 (setq found (funcall search-fn (save-excursion (goto-char found) (line-end-position))
																			search description)))
								(?q
                 (setq done t found nil)))
              (setq pos (point))
              (undo-boundary))))
			(pcase my-emacs-news-check-duplicates-display
				('posframe (posframe-delete prompt-buffer))
				('overlay (delete-overlay my-emacs-news-duplicate-overlay))))
    (goto-char pos)))
;; detect-dupes ends here

;; [[file:emacs-news-code.org::menu][menu]]
(defvar my-org-categorize-emacs-news-menu
  '(("0" . "Other")
    ("1" . "Emacs Lisp")
    ("2" . "Emacs development")
    ("3" . "Emacs configuration")
    ("4" . "Appearance")
    ("5" . "Navigation")
    ("6" . "Org Mode")
    ("7" . "Coding")
    ("8" . "Community")
    ("9" . "Spacemacs")))

(defun my-org-get-list-categories ()
  "Return a list of (category indent matching-regexp sample).
List categories are items that don't contain links."
  (let ((list (org-list-struct)) last-category results)
    (save-excursion
      (mapc
       (lambda (x)
         (goto-char (car x))
         (let ((current-item
                (buffer-substring-no-properties
                 (+ (point)
                    (elt x 1)
                    (length (elt x 2)))
                 (line-end-position))))
           (if (string-match
                org-any-link-re
                (buffer-substring-no-properties
                 (point)
                 (line-end-position)))
               ;; Link - update the last category
               (when last-category
                 (if (< (elt x 1) (elt last-category 1))
                     (setq results
                           (cons (append last-category
                                         (list
                                          (match-string-no-properties
                                           3
                                           (buffer-substring-no-properties
                                            (point)
                                            (line-end-position)))))
                                 (cdr results))))
                 (setq last-category nil))
             ;; Category
             (setq results
                   (cons
                    (setq last-category
                          (list
                           current-item
                           (elt x 1)
                           (concat "^"
                                   (make-string (elt x 1) ?\ )
                                   (regexp-quote
                                    (concat (elt x 2)
                                            current-item))
                                   "$")))
                    results)))))
       list))
    (append '(("x" 2 "^$" nil)) results)))

(defun my-org-move-current-item-to-category (category)
  "Move current list item under CATEGORY earlier in the list.
  CATEGORY can be a string or a list of the form (text indent regexp).
  Point should be on the next line to process, even if a new category
  has been inserted."
  (interactive (list (completing-read "Category: " (my-org-get-list-categories))))
  (when category
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (string (org-trim (buffer-substring-no-properties beg end)))
           (category-text (if (stringp category) category (elt category 0)))
           (category-indent (if (stringp category) 2 (+ 2 (elt category 1))))
           (category-regexp (if (stringp category) category (elt category 2)))
           (pos (point))
           s)
      (delete-region beg (min (1+ end) (point-max)))
      (unless (string= category-text "x")
        (if (re-search-backward category-regexp nil t)
            (forward-line 1)
          (setq s (concat "- " category-text "\n"))
          (insert s)
          (setq pos (+ (length s) pos)))
        (insert (make-string category-indent ?\ )
                string "\n")
        (goto-char (+ pos (length string) category-indent 1))
        (recenter)))))
(defun my-spookfox-browse ()
	(interactive)
	(save-excursion
    (re-search-forward "\\[\\[")
		(let ((browse-url-browser-function 'my-spookfox-background-tab)
          (browse-url-handlers nil))
			(org-open-at-point))))

(eval
 `(defhydra my-org-categorize-emacs-news (:foreign-keys nil)
    "
Default: %(my-emacs-news-guess-category)"
    ,@(mapcar
       (lambda (x)
         `(,(car x)
           (lambda () (interactive) (my-org-move-current-item-to-category ,(concat (cdr x) ":")))
           ,(cdr x)))
       my-org-categorize-emacs-news-menu)
    ("RET"
     (my-org-move-current-item-to-category (concat (my-emacs-news-guess-category) ":")))
    (","
     (lambda () (interactive)
       (my-org-move-current-item-to-category
        (completing-read
         "Category: "
         (append (mapcar (lambda (o) (concat o ":")) (my-emacs-news-sort-order-headings))
                 (mapcar 'car (my-org-get-list-categories))))))
     "By string")
		("l" my-spookfox-open-link-from-page "Open link")
		("L" my-spookfox-insert-link-from-page "Insert link")
    ("r" my-emacs-news-replace-reddit-link "Reddit")
		("u" my-spookfox-insert-link-to-tab "Insert tab")
    ("/" my-spookfox-browse "Open bg")
    ("?" (lambda () (interactive)
           (save-excursion
             (re-search-forward "\\[\\[")
						 (org-open-at-point)))
     "Open fg")
    ("*"
     (lambda () (interactive)
       (if (string= (buffer-name) "*Messages*")
           (bury-buffer)
         (save-excursion
           (re-search-forward org-link-bracket-re)
           (message (match-string 1)))
         (switch-to-buffer "*Messages*")))
     "Show URL")
    ("-" kill-whole-line "Kill")
    ("c"
     (save-excursion
       (re-search-forward "\\[\\[")
       (elfeed-tube-fetch (org-element-property :raw-link (org-element-context)))
       ;; (my-caption-show (org-element-property :raw-link (org-element-context)))
       )
     "Caption")
    ("p" org-next-link "Previous link")
    ("n" org-next-link "Next link")
		("v" my-spookfox-scroll-down "Scroll FF down")
		("V" my-spookfox-scroll-up "Scroll FF up")
    ("C-M-v" scroll-other-window :hint nil)
    ("C-M-S-v" scroll-other-window-down :hint nil)
    ("h" (lambda () (interactive) (my-org-update-link-description "HN")) "Link HN")
    ("i" my-emacs-news-process-irreal-link "Irreal")
    ("." nil "Done")))
;; menu ends here

;; [[file:emacs-news-code.org::my-emacs-news-replace-reddit-link][my-emacs-news-replace-reddit-link]]
(defun my-emacs-news-replace-reddit-link ()
  "Replace the current link with a better link and move the Reddit link to parentheses."
  (interactive)
  (save-excursion
    (unless (eq (org-element-type (org-element-context)) 'link)
      (re-search-forward org-any-link-re (line-end-position))
      (goto-char (match-beginning 0)))

    (let* ((elem (org-element-context))
           (link (org-element-property :raw-link elem))
           new-link)
      (if (string-match "redd\\.?it" link)
          (progn
            ;; Change the location if needed
            (when (string-match "gallery\\|redd\\.it" link)
              (setq link (spookfox-eval-js-in-active-tab
                          "window.location.href"
                          t)))
            (setq new-link (my-spookfox-complete-link))
            (delete-region (org-element-begin elem)
                           (org-element-end elem))
            (insert (org-link-make-string new-link
                                          (my-page-title new-link))
                    " ("
                    (org-link-make-string link "Reddit")
                    ")"))
        (message "Not a Reddit link.")))))
;; my-emacs-news-replace-reddit-link ends here

;; [[file:emacs-news-code.org::export-most-recent][export-most-recent]]
(defun my-emacs-news-post-as-recent ()
	(interactive)
	(let ((formats '((ascii . "txt")
									 (html . "html")
									 (org . "org"))))
		(dolist (format formats)
			(let ((data (org-export-as (car format) t nil (eq (car format) 'ascii))))
				(with-temp-file (concat "most-recent." (cdr format))
					(insert data))))))
;; export-most-recent ends here

;; [[file:emacs-news-code.org::twitter][twitter]]
(defvar my-t-executable "t" "Twitter command-line tool")
(defun my-tweet-emacs-news ()
  (interactive)
  (let ((text (concat (org-get-heading t t t t)
											" https://sachachua.com"
											(org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
											" #emacs #EmacsNews")))
    (my-mastodon-toot-public-string text)
    (kill-new text)
		(browse-url "https://www.twitter.com")
    (browse-url "https://bsky.app")))
;; twitter ends here

;; [[file:emacs-news-code.org::*Spookfox][Spookfox:1]]
(defun my-emacs-news-spookfox-insert-current-link ()
	(interactive)
	(let ((info (spookfox-js-injection-eval-in-active-tab
							 "[window.location.href, document.querySelector('title').textContent]" t)))
		(insert "- " (org-link-make-string (aref info 0) (aref info 1)) "\n")))
;; Spookfox:1 ends here

;; [[file:emacs-news-code.org::librehacker][librehacker]]
(defun my-emacs-news-parse-gem-librehacker-com (date)
	(with-current-buffer (url-retrieve-synchronously "http://gem.librehacker.com/gemlog/starlog/")
		(goto-char (point-min))
		(re-search-forward "^$")
		(let* ((dom (libxml-parse-html-region (point) (point-max)))
					 (links (dom-by-tag dom 'a)))
			(mapconcat (lambda (o)
									 (or
										(when (dom-attr o 'href)
											(when (and (string-match "emacs" (dom-text o))
																 (string-match "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" (dom-text o))
																 (not (string< (match-string 0 (dom-text o)) date)))
												(concat "- " (org-link-make-string
																			(concat "http://gem.librehacker.com" (dom-attr o 'href))
																			(dom-text o))
																"\n")))
										""))
								 links ""))))
;; librehacker ends here

;; [[file:emacs-news-code.org::*Insert string][Insert string:1]]
(defun my-insert-string (s)
	(interactive "MString: ")
	(insert s))
;; Insert string:1 ends here

;; [[file:emacs-news-code.org::bluesky][bluesky]]
(defun emacs-news-bluesky-emacs (date)
	(interactive (list (org-read-date nil nil nil "Since: " nil "-mon")))
	(with-current-buffer (get-buffer-create "*bluesky*")
		(erase-buffer)
		(let* ((base-url "https://public.api.bsky.app/xrpc/app.bsky.feed.searchPosts?q=emacs&tag=emacs&sort=latest&limit=100")
					 (url base-url)
					 (json-object-type 'alist)
					 (json-array-type 'list)
					 data
					 continue
					 link)
			(while url
				(setq data (plz 'get url :as #'json-read)
							continue nil)
				(dolist (entry (alist-get 'posts data))
					(let-alist entry
						(when (string< date .record.createdAt)
							(setq continue t)
							(setq link
										(if (string-match "^at://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)" .uri)
												(format "https://bsky.app/profile/%s/post/%s"
																(match-string 1 .uri)
																(match-string 3 .uri))
											""))
							(insert "* " (org-link-make-string link (concat "@" .author.handle)) "\n"
											"  #+begin_quote\n" (org-ascii--indent-string .record.text 2)
											"\n  #+end_quote\n"
											(if .record.embed.external.uri
													(concat .record.embed.external.uri "\n")
												"")
											"\n"))))
				(setq url
							(if (and continue (plist-get 'cursor data))
									(concat base-url "&cursor=" (plist-get 'cursor data))
								nil))))
		(org-mode)
		(switch-to-buffer (current-buffer))))
;; bluesky ends here

;; [[file:emacs-news-code.org::*Looking up Andres's notes][Looking up Andres's notes:1]]
(defun emacs-news-find-notes-for-url ()
	(interactive)
	(goto-char (point-min))
	(re-search-forward
	 (regexp-quote
		(spookfox-js-injection-eval-in-active-tab "window.location.href" t))))
;; Looking up Andres's notes:1 ends here
