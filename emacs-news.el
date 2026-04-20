;; [[file:emacs-news-code.org::export-ascii][export-ascii]]
(setq org-ascii-links-to-notes nil)
;; export-ascii ends here

;; [[file:emacs-news-code.org::reddit][reddit]]
(defvar emacs-news-reddit-upvoted-json "https://www.reddit.com/user/emacsnews/upvoted.json" "JSON for upvoted posts.")
(require 'helm-utils)

(defvar emacs-news-reddit-client-id "your-client-id")
(defvar emacs-news-reddit-client-secret "your-client-secret")
(defvar emacs-news-reddit-username "your-username")
(defvar emacs-news-reddit-password "your-password")
(defvar emacs-news-reddit-user-agent "EmacsNews/1.0 by u/sachac")
(defvar emacs-news-reddit-access-token nil)
(defvar emacs-news-reddit-token-expires nil)

(defun emacs-news-reddit-authenticate ()
  "Authenticate with Reddit using OAuth password grant"
  (let* ((auth-string (concat emacs-news-reddit-client-id ":" emacs-news-reddit-client-secret))
         (encoded-auth (base64-encode-string auth-string))
         (url-request-method "POST")
         (url-request-data
          (concat "grant_type=password"
                  "&username=" (url-hexify-string emacs-news-reddit-username)
                  "&password=" (url-hexify-string emacs-news-reddit-password)))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " encoded-auth))
            ("Content-Type" . "application/x-www-form-urlencoded")
            ("User-Agent" . ,emacs-news-reddit-user-agent))))
    (with-current-buffer
        (url-retrieve-synchronously "https://www.reddit.com/api/v1/access_token")
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (token (cdr (assoc 'access_token response)))
             (expires-in (cdr (assoc 'expires_in response))))
        (setq emacs-news-reddit-access-token token)
        (setq emacs-news-reddit-token-expires (+ (float-time) expires-in))
        (message "Reddit authentication successful")))))

(defun emacs-news-reddit-get-upvoted-posts (&optional limit after)
  "Get your upvoted posts using OAuth
LIMIT: number of posts to retrieve (default 25, max 100)
AFTER: fullname of a thing for pagination (e.g., 't3_abc123')"
  (unless (and emacs-news-reddit-access-token
               (< (float-time) emacs-news-reddit-token-expires))
    (emacs-news-reddit-authenticate))
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " emacs-news-reddit-access-token))
            ("User-Agent" . ,emacs-news-reddit-user-agent)))
         (query-params
          (concat "?limit=" (number-to-string (or limit 25))
                  (when after (concat "&after=" (url-hexify-string after)))))
         (url (concat "https://oauth.reddit.com/user/" emacs-news-reddit-username "/upvoted" query-params)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun emacs-news-reddit-list-upvoted (date)
  (interactive (list (org-read-date)))
  (let ((threshold (org-read-date nil t (concat (substring date 0 (min (length date) 10)) " 0:00")))
				after
				page
				data
				page
        results)
		(catch 'done
			(while t
				(setq data (assoc-default "data" (emacs-news-reddit-get-upvoted-posts 50 after) 'string=))
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
    (when (called-interactively-p 'any) (insert results))
    results))
;;  (emacs-news-reddit-list-upvoted "-mon")
;; reddit ends here

;; [[file:emacs-news-code.org::youtube][youtube]]
(defvar emacs-news-playlist "https://www.youtube.com/playlist?list=PL4th0AZixyREOtvxDpdxC9oMuX7Ar7Sdt"
  "Public playlist with recent videos to include in Emacs News.")

(defun emacs-news-get-videos (playlist)
  (json-parse-string
       (concat "["
               (mapconcat
                'identity
                (split-string
                 (shell-command-to-string
                  (concat "yt-dlp -j --flat-playlist " (shell-quote-argument emacs-news-playlist) " 2> /dev/null"))
                 "\n" t)
                ",")
               "]")
       :object-type 'plist))

(defun emacs-news-save-new-items (file alist)
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

(defun emacs-news-youtube-playlist (playlist &optional start-date end-date)
  (let* ((videos (emacs-news-get-videos playlist))
         (combined (emacs-news-save-new-items "video-list.el"
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
(defun emacs-news-org-sort-list-in-custom-order (order)
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

(defvar emacs-news-sort-order
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
(defun emacs-news-org-sort-order-headings ()
  (mapcar (lambda (o) (if (stringp o) o (car o))) emacs-news-sort-order))
(defun emacs-news-sort-list ()
  (interactive)
  (goto-char (org-list-get-top-point (org-list-struct)))
  (emacs-news-org-sort-list-in-custom-order (emacs-news-org-sort-order-headings)))
;; sort ends here

;; [[file:emacs-news-code.org::#code-experimenting-with-using-llms-for-link-categorization][Experimenting with using LLMs for link categorization:1]]
(defvar emacs-news-link-categories nil
  "List of (url category).")

(defun emacs-news-read-categories ()
  (interactive)
  (setq emacs-news-link-categories (json-read)))

(defun emacs-news-get-categories-from-spookfox ()
  (interactive)
  (setq emacs-news-link-categories (learn-lang-spookfox-ai-parse-latest-json)))

(defvar emacs-news-categorize-prompt
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

(defun emacs-news-spookfox-ai-categorize (&optional read-answer)
  (interactive (list current-prefix-arg))
  (setq emacs-news-link-categories
        (if read-answer
            (learn-lang-spookfox-ai-parse-latest-json)
          (learn-lang-spookfox-ai-send-text-and-get-json
           (concat emacs-news-categorize-prompt
                   "\n\n###\n\n"
                   (my-org-subtree-text))))))

(defun emacs-news-guess-category ()
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
                        emacs-news-sort-order))
         (car (assoc-default url emacs-news-link-categories 'string=)))))))
;; Experimenting with using LLMs for link categorization:1 ends here

;; [[file:emacs-news-code.org::#code-experimenting-with-using-llms-for-link-categorization][Experimenting with using LLMs for link categorization:2]]
(defun emacs-news-auto-categorize ()
  (interactive)
  (while (and (looking-at "^- \\[\\[") (not (looking-at "^- New package")))
    (let ((category (emacs-news-guess-category)))
      (if category
          (sacha-org-move-current-item-to-category (concat category ":"))
        (forward-line 1)
        (goto-char (line-beginning-position))))))
;; Experimenting with using LLMs for link categorization:2 ends here

;; [[file:emacs-news-code.org::emacs-news-update-package-list][emacs-news-update-package-list]]
(defvar emacs-news-package-list-file "~/sync/emacs-news/package-list.el")

(defun emacs-news-read-sexp-from-file (filename)
  (with-temp-buffer (insert-file-contents filename)
                    (goto-char (point-min)) (read (current-buffer))))

(defun emacs-news-update-package-list (&optional date)
	"Update the list of packages. Mark new packages with DATE."
  (interactive (list (format-time-string "%Y-%m-%d")))
	(setq date (or date (format-time-string "%Y-%m-%d")))
  (let* ((archives (emacs-news-get-current-packages date))
         (old-list (emacs-news-read-sexp-from-file emacs-news-package-list-file)))
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
    (with-temp-file emacs-news-package-list-file
      (insert "("
              (mapconcat #'prin1-to-string
                         old-list
                         "\n")
              ")"))
    old-list))
;; emacs-news-update-package-list ends here

;; [[file:emacs-news-code.org::emacs-news-get-current-packages][emacs-news-get-current-packages]]
(defun emacs-news-get-current-packages (date)
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
;; emacs-news-get-current-packages ends here

;; [[file:emacs-news-code.org::emacs-news-list-new-packages][emacs-news-list-new-packages]]
(defun emacs-news-packages-between (from-date &optional to-date)
	(seq-filter
   (lambda (o)
     (and
			(or (not from-date) (not (string< (cdar (cadr o)) from-date)))
			(or (not to-date) (string< (cdar (cadr o)) to-date))))
   (emacs-news-read-sexp-from-file emacs-news-package-list-file)))

(defun emacs-news-list-new-packages (&optional date)
  (interactive)
  (let ((packages
         (emacs-news-describe-packages
          (seq-filter
           (lambda (o)
             (seq-remove (lambda (archive) (string< (cdr archive) date))
                         (cadr o)))
           (emacs-news-read-sexp-from-file emacs-news-package-list-file)))))
    (if (called-interactively-p 'any)
        (insert packages)
      packages)))
;; emacs-news-list-new-packages ends here

;; [[file:emacs-news-code.org::emacs-news-describe-packages][emacs-news-describe-packages]]
(defun emacs-news-describe-packages (list)
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
;; emacs-news-describe-packages ends here

;; [[file:emacs-news-code.org::packages][packages]]
(defun sacha-org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(defun sacha-org-package-export (link description format &optional arg)
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

  (ert-deftest sacha-org-package-export ()
    (should
     (string=
      (sacha-org-package-export "transcribe" "transcribe" 'html)
      "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
      ))
    (should
     (string=
      (sacha-org-package-export "fireplace" "fireplace" 'html)
      "<a target=\"_blank\" href=\"https://melpa.org/#/fireplace\">fireplace</a>"
      )))
;; packages ends here

;; [[file:emacs-news-code.org::#formatting-packages][Formatting new package entries:3]]
(org-link-set-parameters "package" :follow 'sacha-org-package-open :export 'sacha-org-package-export)
;; Formatting new package entries:3 ends here

;; [[file:emacs-news-code.org::emacs-news-announce-elpa-package][emacs-news-announce-elpa-package]]
(defun emacs-news-announce-elpa-package (package-name)
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
;; emacs-news-announce-elpa-package ends here

;; [[file:emacs-news-code.org::link-description][link-description]]
(defun emacs-news-escape-link-description (s)
  (let ((replace-map '(("\\[" . "")
                       ("\\]" . ":"))))
    (mapc (lambda (rule)
            (setq s (replace-regexp-in-string (car rule) (cdr rule) s)))
          replace-map)
    s))
(use-package xml-rpc)

(defun emacs-news-org-list-from-rss (url from-date &optional to-date)
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
  (defun emacs-news-insert-emacs-news-from-git (date &optional directory)
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
(defun emacs-news-insert-org-news-from-git (date &optional directory)
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
(defun emacs-news-commit-and-push (&optional title)
	(interactive)
	(when (magit-anything-unstaged-p)
		(sacha-magit-stage-all-and-commit (or title "update")))
	(unwind-protect (magit-push-current-to-pushremote (magit-push-arguments))))

(defvar emacs-news-target "info-gnu-emacs@gnu.org")
(defvar emacs-news-headers `(("Reply-To" . ,user-mail-address)
																("Mail-Followup-To" . ,user-mail-address)
																("Mail-Reply-To" . ,user-mail-address)))
(defvar emacs-news-send-immediately nil "Non-nil means send the e-mail without waiting.")

(defun emacs-news-email (&optional target send-immediately)
	(interactive)
	(setq target (or target emacs-news-target))
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
						 (org-export-string-as text 'emacs-news-plain-text t)
						 (org-export-string-as text 'html t)
						 (substring title 0 10)
						 (org-export-string-as text 'org t))))
		(compose-mail target title emacs-news-headers)
		(message-goto-body)
		(insert output)
		(when send-immediately
			(message-send-and-exit))))

(defun emacs-news-share ()
  "Prepare emacs-tangents e-mail of post, and commit to Git."
  (interactive)
	(require 'magit)
	(emacs-news-post-as-recent)
	(let ((html (org-export-as 'html))
				(text (org-export-as 'emacs-news-plain-text nil nil t))
				(org-export-html-preamble nil)
				(org-html-toplevel-hlevel 3)
				(title (org-get-heading)))
		(with-temp-file "index.html"
			(insert html))
		(with-temp-file "index.txt"
			(insert text))
		(emacs-news-commit-and-push title)
		(emacs-news-email)))


(defun emacs-news-plain-text-link (link contents info)
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
     ((org-export-custom-protocol-maybe link description 'emacs-news-plain-text info))
     (t
			(if description
					(format "%s %s" description url)
				url)))))

(defun emacs-news-plain-text-item (item contents info)
  "Transcode an ITEM element with 4-space indentation.
CONTENTS is the contents of the item.
INFO is a plist used as a communication channel."
	(replace-regexp-in-string "^\\(  \\)+" "\\1\\1" (org-ascii-item item contents info)))

;; Define the custom backend
(org-export-define-derived-backend 'emacs-news-plain-text 'ascii
  :translate-alist '((link . emacs-news-plain-text-link)
                     (item . emacs-news-plain-text-item))
  :menu-entry '(?p "Export to Custom Plain Text"
									 ((?p "As plain text buffer" emacs-news-plain-text-export-to-buffer))))

(defun emacs-news-plain-text-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text buffer."
  (interactive)
  (org-export-to-buffer 'emacs-news-plain-text "*My Plain Text Export*"
    async subtreep visible-only body-only ext-plist))

(defun emacs-news-plain-text-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to plain text file."
  (interactive)
  (let ((file (org-export-output-file-name ".txt" subtreep)))
    (org-export-to-file 'emacs-news-plain-text file
      async subtreep visible-only body-only ext-plist)))
(add-to-list 'org-export-backends 'emacs-news-plain-text)
;; share-news ends here

;; [[file:emacs-news-code.org::prepare-news][prepare-news]]
(defcustom emacs-news-rss-feeds nil "List of Emacs feeds"
  :type '(repeat string)
  :group 'emacs-news)
(defvar emacs-news-emacs-calendar-csv "~/sync/emacs-calendar/events.csv")

(defun emacs-news-emacs-calendar-list ()
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
                           (sacha-org-table-as-alist (pcsv-parse-file emacs-news-emacs-calendar-csv))))
                (lambda (a b) (time-less-p (car a) (car b))))
               "\n")))

(defun emacs-news-prepare (date parts)
  (interactive (list (org-read-date nil nil "-mon 0:00") '(refresh-packages reddit rss)))
  (setq date (substring date 0 10))
  (let* ((start-date date)
         (end-date (substring (org-read-date nil nil "++1" nil (org-read-date nil t "-sun 0:00")) 0 10))
         (year (substring end-date 0 4))
         (month (substring end-date 5 7)))
    (when (member 'refresh-packages parts)
      (package-refresh-contents)
      (emacs-news-update-package-list date)
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
          (replace-regexp-in-string "^" "  " (emacs-news-emacs-calendar-list))
          "\n")
       "")
     (if (member 'git parts)
         (concat
          "- Emacs development:\n"
          (emacs-news-insert-emacs-news-from-git date)
          "\n"
          )
       "")
     (if (member 'git-org parts)
         (concat
          "- Org Mode:\n  - Org development:\n"
          (emacs-news-insert-org-news-from-git date)
          "\n"
          ))
     "- Other:\n"
     (if (member 'worg parts)
         (emacs-news-org-list-from-rss "https://tracker.orgmode.org/news.rss" (substring date 0 10))
       "")
     (if (member 'rss parts)
         (mapconcat (lambda (feed)
                      (emacs-news-org-list-from-rss feed (substring date 0 1)))
                    emacs-news-rss-feeds
                    "")
       "")

     (if (member 'rss-es parts)
         (emacs-news-org-list-from-rss "https://planet.emacs-es.org/atom.xml" (substring date 0 10))
       "")
     "\n"
     (if (member 'reddit parts) (emacs-news-reddit-list-upvoted (substring date 0 10)) "")
     (if (member 'youtube parts)
         (emacs-news-youtube-playlist emacs-news-playlist start-date))
		 (if (member 'librehacker parts)
				 (emacs-news-parse-gem-librehacker-com date)
			 "")
     "- New packages:\n"
     (emacs-news-list-new-packages date)
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
  (defun emacs-news-collect-entries (&optional category-filter)
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
(defvar emacs-news-check-duplicates-display 'overlay "*minibuffer or posframe or overlay")
(defvar emacs-news-duplicate-overlay nil)
(defun emacs-news-check-duplicates ()
  (interactive)
  (let ((end (save-excursion (org-end-of-subtree)))
        (prompt-buffer (when (eq emacs-news-check-duplicates-display 'posframe) (get-buffer-create "*Duplicate*")))
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
							(pcase emacs-news-check-duplicates-display
								('posframe
								 ;; (when (eq emacs-news-check-duplicates-display 'posframe) (posframe-hide prompt-buffer))
								 (posframe-show prompt-buffer
																:string prompt
																:internal-border-width 2
																:internal-border-color "white"
																:position (point))
								 (setq input (read-char)))
								('minibuffer
								 (setq input (read-char prompt)))
								('overlay
								 (if (overlayp emacs-news-duplicate-overlay)
										 (move-overlay emacs-news-duplicate-overlay (line-beginning-position) (line-end-position))
									 (setq emacs-news-duplicate-overlay (make-overlay (line-beginning-position) (line-end-position))))
								 (overlay-put emacs-news-duplicate-overlay 'after-string
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
                 (emacs-news-process-irreal-link))
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
			(pcase emacs-news-check-duplicates-display
				('posframe (posframe-delete prompt-buffer))
				('overlay (delete-overlay emacs-news-duplicate-overlay))))
    (goto-char pos)))
;; detect-dupes ends here

;; [[file:emacs-news-code.org::menu][menu]]
(defvar emacs-news-org-categorize-menu
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

(defun sacha-org-get-list-categories ()
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

(defun sacha-org-move-current-item-to-category (category)
  "Move current list item under CATEGORY earlier in the list.
  CATEGORY can be a string or a list of the form (text indent regexp).
  Point should be on the next line to process, even if a new category
  has been inserted."
  (interactive (list (completing-read "Category: " (sacha-org-get-list-categories))))
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
(defun emacs-news-spookfox-browse ()
	(interactive)
	(save-excursion
    (re-search-forward "\\[\\[")
		(let ((browse-url-browser-function 'sacha-spookfox-background-tab)
          (browse-url-handlers nil))
			(org-open-at-point))))

(eval
 `(defhydra emacs-news-org-categorize (:foreign-keys nil)
    "
Default: %(emacs-news-guess-category)"
    ,@(mapcar
       (lambda (x)
         `(,(car x)
           (lambda () (interactive) (sacha-org-move-current-item-to-category ,(concat (cdr x) ":")))
           ,(cdr x)))
       emacs-news-org-categorize-menu)
    ("RET"
     (sacha-org-move-current-item-to-category (concat (emacs-news-guess-category) ":")))
    (","
     (lambda () (interactive)
       (sacha-org-move-current-item-to-category
        (completing-read
         "Category: "
         (append (mapcar (lambda (o) (concat o ":")) (emacs-news-org-sort-order-headings))
                 (mapcar 'car (sacha-org-get-list-categories))))))
     "By string")
		("l" sacha-spookfox-open-link-from-page "Open link")
		("L" sacha-spookfox-insert-link-from-page "Insert link")
    ("r" emacs-news-replace-reddit-link "Reddit")
		("u" sacha-spookfox-insert-link-to-tab "Insert tab")
    ("/" emacs-news-spookfox-browse "Open bg")
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
       ;; (emacs-news-caption-show (org-element-property :raw-link (org-element-context)))
       )
     "Caption")
    ("p" org-next-link "Previous link")
    ("n" org-next-link "Next link")
		("v" sacha-spookfox-scroll-down "Scroll FF down")
		("V" sacha-spookfox-scroll-up "Scroll FF up")
    ("C-M-v" scroll-other-window :hint nil)
    ("C-M-S-v" scroll-other-window-down :hint nil)
    ("h" (lambda () (interactive) (sacha-org-update-link-description "HN")) "Link HN")
    ("i" emacs-news-process-irreal-link "Irreal")
    ("." nil "Done")))
;; menu ends here

;; [[file:emacs-news-code.org::emacs-news-replace-reddit-link][emacs-news-replace-reddit-link]]
(defun emacs-news-replace-reddit-link ()
  "Replace the current link with a better link and move the Reddit link to parentheses."
  (interactive)
  (save-excursion
    (unless (eq (org-element-type (org-element-context)) 'link)
      (re-search-forward org-any-link-re (line-end-position))
      (goto-char (match-beginning 0)))
    (let* ((elem (org-element-context))
           (old-title
            (buffer-substring
             (org-element-contents-begin elem)
             (org-element-contents-end elem)))
           (link (org-element-property :raw-link elem))
           new-link)
      (if (string-match "redd\\.?it" link)
          (progn
            ;; Change the location if needed
            (when (string-match "gallery\\|redd\\.it" link)
              (setq link (spookfox-eval-js-in-active-tab
                          "window.location.href"
                          t)))
            (setq new-link (sacha-spookfox-complete-link))
            (delete-region (org-element-begin elem)
                           (org-element-end elem))
            (insert (org-link-make-string
                     new-link
                     old-title)
                    (if (save-excursion
                          (re-search-forward
                           "\\[Reddit\\]" (line-end-position) t))
                        " "
                      (concat
                       " ("
                       (org-link-make-string link "Reddit")
                       ")"))))
        (message "Not a Reddit link.")))))
;; emacs-news-replace-reddit-link ends here

;; [[file:emacs-news-code.org::export-most-recent][export-most-recent]]
(defun emacs-news-post-as-recent ()
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
(defvar emacs-news-t-executable "t" "Twitter command-line tool")
(defun emacs-news-tweet-emacs-news ()
  (interactive)
  (let ((text (concat (org-get-heading t t t t)
											" https://sachachua.com"
											(org-entry-get (point) "EXPORT_ELEVENTY_PERMALINK")
											" #emacs #EmacsNews")))
    (emacs-news-mastodon-toot-public-string text)
    (kill-new text)
		(browse-url "https://www.twitter.com")
    (browse-url "https://bsky.app")))
;; twitter ends here

;; [[file:emacs-news-code.org::#code-spookfox][Spookfox:1]]
(defun emacs-news-spookfox-insert-current-link ()
	(interactive)
	(let ((info (spookfox-js-injection-eval-in-active-tab
							 "[window.location.href, document.querySelector('title').textContent]" t)))
		(insert "- " (org-link-make-string (aref info 0) (aref info 1)) "\n")))
;; Spookfox:1 ends here

;; [[file:emacs-news-code.org::librehacker][librehacker]]
(defun emacs-news-parse-gem-librehacker-com (date)
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

;; [[file:emacs-news-code.org::#code-insert-string][Insert string:1]]
(defun emacs-news-insert-string (s)
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

;; [[file:emacs-news-code.org::#code-looking-up-andres-s-notes][Looking up Andres's notes:1]]
(defun emacs-news-find-notes-for-url ()
	(interactive)
	(goto-char (point-min))
	(re-search-forward
	 (regexp-quote
		(spookfox-js-injection-eval-in-active-tab "window.location.href" t))))
;; Looking up Andres's notes:1 ends here

;; [[file:emacs-news-code.org::#categorizing-emacs-news-items-by-voice-in-org-mode-the-code-so-far][The code so far:1]]
(defun emacs-news-categorize-with-voice (&optional skip-browse)
  (interactive (list current-prefix-arg))
  (unless skip-browse
    (emacs-news-spookfox-browse))
  (speech-input-cancel-recording)
  (let (continue)
    (speech-input-multiple-from-list
     nil
     '(("Org Mode" "Org" "Org Mode")
       "Other"
       "Emacs Lisp"
       "Coding"
       ("Emacs configuration" "Config" "Configuration")
       ("Emacs development" "Emacs development" "Development")
       ("Appearance" "Appearance")
       ("Default" "Okay" "Default")
       "Community"
       "AI"
       "Beginner"
       "Writing"
       ("Reddit" "Read it" "Reddit")
       ("Shells" "Shells" "Shell")
       "Navigation"
       "Fun"
       ("Dired" "Directory" "Dir ed")
       ("Mail, news, and chat" "News" "Mail" "Chat")
       "Web"
       "Multimedia"
       "Scroll down"
       "Scroll up"
       "Web"
       "Denote"
       "TRAMP"
       "Delete"
       "Skip"
       "Undo"
       ("Quit" "Quit" "Cancel" "All done"))
     (lambda (result text)
       (message "Recognized %s original %s" result text)
       (pcase result
         ("Undo"
          (undo)
          (setq continue 'skip))
         ("Skip"
          (forward-line)
          (setq continue 'open))
         ("Quit"
          (message "All done.")
          (speech-input-cancel-recording))
         ("Reddit"
          (emacs-news-replace-reddit-link)
          (setq continue 'skip))
         ("Scroll down"
          (sacha-spookfox-scroll-down)
          (setq continue 'skip))
         ("Scroll up"
          (emacs-news-spookfox-scroll-up)
          (setq continue 'skip))
         ("Delete"
          (delete-line)
          (undo-boundary)
          (setq continue 'open))
         ("Default"
          (sacha-org-move-current-item-to-category
           (concat default ":"))
          (undo-boundary)
          (setq continue 'open))
         ('nil
          (message "Skipping unrecognized speech")
          (setq continue 'open))
         (_
          (sacha-org-move-current-item-to-category
           (concat result ":"))
          (undo-boundary)
          (setq continue 'open))))
     (lambda (commands text)
       ;; after everything
       (cond
        ((null (car commands))
         (message "Skipping: %s" text)
         (emacs-news-categorize-with-voice (eq continue 'skip)))
        (continue
         (emacs-news-categorize-with-voice (eq continue 'skip))))
       ))))
;; The code so far:1 ends here

;; [[file:emacs-news-code.org::#categorizing-emacs-news-items-by-voice-in-org-mode-moving-irreal-links-to-parenthetical-notes][Moving Irreal links to parenthetical notes:1]]
(defun emacs-news-process-irreal-link ()
  "Process an irreal.org link to extract referenced URLs and add them to Org notes."
  (interactive)
  (let* ((url (progn
                (unless (org-in-regexp org-link-bracket-re 1)
                  (re-search-forward org-link-bracket-re (line-end-position)))
                (org-element-property :raw-link (org-element-context)))))
    (when (string-match "irreal.org" url)
      (let ((body (plz 'get url))
            dom article links regexp found)
        (with-temp-buffer
          (insert body)
          (setq dom (libxml-parse-html-region (point-min) (point-max)))
          (setq article (dom-search dom (lambda (o)
                                          (and (eq (dom-tag o) 'div)
                                               (string= (dom-attr o 'class) "entry-content")))))
          (setq links
                (mapcar (lambda (o) (dom-attr o 'href))
                        (dom-by-tag article 'a)))
          (setq regexp (regexp-opt links)))
        (if (save-excursion (re-search-forward regexp nil t))
            (progn
              (delete-region (line-beginning-position) (1+ (line-end-position)))
              (re-search-forward regexp nil t)
              (if (re-search-forward " *(\\([^)]*\\)) *$" (line-end-position) t) ; existing parens
                  (progn
                    (goto-char (match-end 1))
                    (insert ", " (org-link-make-string url "Irreal")))
                (goto-char (line-end-position))
                (insert " (" (org-link-make-string url "Irreal") ")")))
          (message "No matching link."))))))
;; Moving Irreal links to parenthetical notes:1 ends here

;; [[file:emacs-news-code.org::#categorizing-emacs-news-items-by-voice-in-org-mode-summarize-mastodon][Summarize Mastodon:1]]
(defun emacs-news-mastodon-get-note-info ()
	"Return (:handle ... :url ... :links ... :text) for the current subtree."
	(let ((url (let ((title (org-entry-get (point) "ITEM")))
							 (if (string-match org-link-any-re title)
									 (or
										(match-string 7 title)
										(match-string 2 title)))))
				beg end
				handle)
		(save-excursion
			(org-back-to-heading)
			(org-end-of-meta-data)
			(setq beg (point))
			(setq end (org-end-of-subtree))
      (unless url
        (goto-char beg)
        (when (re-search-forward org-any-link-re end t)
          (setq url (org-element-property :raw-link (org-element-context)))))
			(cond
       ((string-match "\\[\\[https://bsky\\.app/.+?\\]\\[\\(.+\\)\\]\\]" url)
				(setq handle (match-string 1 url)))
			 ((string-match "https://\\(.+?\\)/\\(@.+?\\)/" url)
				(setq handle (concat
											(match-string 2 url) "@" (match-string 1 url))))
			 ((string-match "https://\\(.+?\\)/\\(.+?\\)/p/[0-9]+\\.[0-9]+" url)
				(setq handle (concat
											"@" (match-string 2 url) "@" (match-string 1 url)))))
			(list
			 :handle handle
			 :url (if (string-match org-link-bracket-re url) (match-string 1 url) url)
			 :links (reverse (mapcar (lambda (o) (org-element-property :raw-link o))
															 (emacs-news-org-get-links-in-region beg end)))
			 :text (string-trim (buffer-substring-no-properties beg end))))))

(defun emacs-news-org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

(defun emacs-news-page-title (url)
	"Get the page title for URL. Simplify some titles."
	(condition-case nil
			(pcase url
				((rx "reddit.com") "Reddit")
				((rx "news.ycombinator.com") "HN")
				((rx "lobste.rs") "lobste.rs")
				(_
				 (with-current-buffer (url-retrieve-synchronously url)
					 (string-trim
						(replace-regexp-in-string
						 "[ \n]+" " "
						 (replace-regexp-in-string
							"\\(^Github - \\|:: Sacha Chua\\)" ""
							(or
							 (dom-texts (car
													 (dom-by-tag (libxml-parse-html-region
																				(point-min)
																				(point-max))
																			 'title)))
							 "")))))))
		(error nil)))

(defun emacs-news-summarize-mastodon-items ()
	(interactive)
	(while (not (eobp))
		(let* ((info (emacs-news-mastodon-get-note-info))
					 (title (when (car (plist-get info :links))
										(emacs-news-page-title (car (plist-get info :links)))))
					 (summary (read-string
										 (if title
												 (format "Summary (%s): " title)
											 "Summary: ")
										 title)))
			(org-cut-subtree)
			(unless (string= summary "")
				(insert "- " (org-link-make-string
											(or (car (plist-get info :links))
													(plist-get info :url))
											summary)
								(if (and (car (plist-get info :links))
												 (plist-get info :handle))
										(concat " (" (org-link-make-string (plist-get info :url)
																											 (plist-get info :handle))
														")")
									"")
								"\n")))
    (undo-boundary)))
;; Summarize Mastodon:1 ends here
