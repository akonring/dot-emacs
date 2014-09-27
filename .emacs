;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin .emacs file
;; Initialization file for the emacs editor
;; environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)				
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))
(setq el-get-user-package-directory "~/.emacs.d/package-init")

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>")     'buf-mojive-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name multiple-cursors
          :after (progn 
                   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit				; git meet emacs, and a binding
	  :after (progn
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name paredit
          :after (progn (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
                        (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
                        (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
                        (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
                        (add-hook 'scheme-mode-hook           #'enable-paredit-mode))paren)))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   escreen            			; screen for emacs, C-\ C-h
   php-mode-improved			; if you're into php...
   switch-window			; takes over C-x o
   auto-complete			; complete as you type with overlays
   undo-tree
   yasnippet 				; powerful snippet mode
   zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding
   color-theme		                ; nice looking emacs
   color-theme-tango))	                ; check out color-theme-solarized


;; Some recipes require extra tools to be installed
;; CVS install
(when (ignore-errors (el-get-executable-find "cvs"))
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (ignore-errors (el-get-executable-find "svn"))
  (loop for p in '(psvn    		; M-x svn-status
		   )
	do (add-to-list 'my:el-get-packages p)))

;; All packages and initialisation in one bag
(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-10"))

(global-hl-line-mode)			; highlight current line
(global-linum-mode 1)			; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; Auto-reload buffer
(global-auto-revert-mode 1)

;; Shellmode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; use ido for minibuffer completion
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; have vertical ido completion lists
(setq ido-decorations
      '("\n-> " "" "\n   " "\n   ..." "[" "]"
	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(show-paren-mode t)
(tooltip-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Find file from home
(setq default-directory "~/")

;; Stop spreading the *~ files all over
;; keep the temporary files in a temp-directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Load .emacs in one line
(defun load-dot-emacs ()
  "Loading dot emacs file"
  (interactive)
  (load-file "~/.emacs"))

(setq tramp-default-method "ssh")

;; Configuration of Multi Cursor package
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Undo and redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) 
(global-set-key (kbd "C-M-z") 'redo)

;; Set personal scratch message and dont show emacs tips
(setq initial-scratch-message 
      (let ((awesome-message
             '("Hack away, Incredible"
               "Choose your code wisely, my friend"
               "I am an awesome lisp hacker"))
            (random-int (random 3)))
        (cond
         ((eq random-int 1) (car awesome-message))
         ((eq random-int 2) (cadr awesome-message))
         (t (caddr awesome-message)))))

(setq inhibit-startup-message t)

;; Google this mode
;; (google-this-mode 1)

;; ;; Auto-complete in every mode
;; (or global-auto-complete-mode (global-auto-complete-mode))

;; ;; Is it neccesary to tell emacs to use ac
;; ;; in different modes
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (ac-ispell-setup)))

;; (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'mail-mode-hook 'ac-ispell-ac-setupa)


;; ;; Emacs browser
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; ;; optional keyboard short-cut
;; (global-set-key "\C-xm" 'w3m-browse-url)

;; ;; Auto-complete for org-mode
;; (require 'org-ac)
;; (org-ac/config-default)

;; ;; Org mode
;; (setq org-default-notes-file "~/Dropbox/org/notes.org")
;; (define-key global-map "\C-cc" 'org-capture)

;; (global-set-key (kbd "C-c a") 'org-agenda)

;; (setq org-agenda-files '("~/Dropbox/org/"))

;; (setq org-directory "~/Dropbox/org/")

;; (setq org-indent-mode 1)

;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

;; (defun org-dir ()
;;   "Open dired-mode in the org directory"
;;   (interactive)
;;   (find-file "/Users/anderskonring/Dropbox/org/"))

;; (setq org-capture-templates                                                      
;;       '(("m" "TODO from Mail" entry (file+headline "~/Dropbox/org/todo.org" "Inbox") "* TODO %?, Link: %a")))


;; ;; Write html-mails with org-mime if you are into that sort of stuff
;; (require 'org-mime)

;; ;; Setup  Twitter mode
;; (require 'twittering-mode)

;; ;; ERC autojoin and open
;; (setq erc-auto-reconnect 1)
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "#wiki" "#nethack" "#clojure" "#clojurescript" "##math" "##crypto")))

;; (defun erc-start-or-switch ()
;;   "Connect to ERC, or switch to last active buffer"
;;   (interactive)
;;   (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
;;       (erc-track-switch-buffer 1) ;; switch to last active
;;     (when (y-or-n-p "Start ERC? ") ;; maybe start ERC
;;       (erc :server "irc.freenode.net" :port 6667 :nick "akonring")
;;       (erc :server "irc.gimp.org" :port 6667 :nick "foo" :full-name "bar"))))


;; (require 'osx-browse)
;; (osx-browse-mode 1)
;; (setq browse-url-dwim-always-confirm-extraction nil)
;; (global-set-key (kbd "s-b") 'browse-url-of-buffer)

;; (global-set-key (kbd "s-i") 'browse-url)
;; (global-set-key (kbd "s-/") 'google-this)


;; (global-set-key (kbd "C-s-b") 'osx-browse-url)

;; (require 'openwith)
;; (openwith-mode 1)


;; ;; Set default directory for wget to Downloads
;; (setq wget-download-directory "~/Downloads")


;; ;; Just open the files already.
;; ;; (setq openwith-associations
;; ;;       '(("\\.pdf\\'" "open" (file))
;; ;;         ("\\.mp3\\'" "open" (file))
;; ;;         ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "open" ("-idx" file))
;; ;;         ("\\.\\(?:jp?g\\|png\\)\\'" "open" (file))))

;; ;; Spelling

;; ;; This section contains things to help me spell in Emacs.
;; ;;
;; ;; Requires:
;; ;;  - ispell-mode (contained in standard GNU Emacs)
;; ;;  - flyspell-mode (Is probably standard too)
;; ;;    http://www-sop.inria.fr/mimosa/Manuel.Serrano/flyspell/flyspell.html
;; ;;  - aspell
;; ;;    http://aspell.net/
;; ;;
;; ;; The file also contains a convenient function to automatically guess
;; ;; the language of a buffer when opened or manually guess an open
;; ;; buffer with: 
;; ;; M-x guess-language RET

;; ;; Set aspell as spell program
;; (setq ispell-program-name "aspell")

;; ;; Speed up aspell: ultra | fast | normal
;; (setq ispell-extra-args '("--sug-mode=normal"))

;; ;; Remove Flyspell from some sub modes of text mode
;; (dolist (hook '(change-log-mode-hook 
;; 		log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; ;;; if emacs does not set up a dictionary automatically this is what
;; ;;; is needed.
;; ;;
;; ;; ;; Add the danish dictionary
;; (require 'ispell)
;; (add-to-list 'ispell-dictionary-alist
;; 	     '("dansk"
;; 	       "[a-zA-Z\346\370\345\306\330\305]" 
;; 	       "[^a-zA-Z\346\370\345\306\330\305]"
;; 	       "[']" t ("-C" "-d" "dansk") "~latin1" iso-8859-1))
;; (defun my-ispell-danish-dictionary ()
;;   "Switch to the Danish dictionary."
;;   (interactive)
;;   (ispell-change-dictionary "dansk"))
;; (require 'easymenu)
;; (easy-menu-add-item  
;;  nil 
;;  '("tools" "spell")
;;  ["Select Danish Dict" my-ispell-danish-dictionary t])

;; ;;; Language guesser stuff
;; ;;  See: http://www.emacswiki.org/cgi-bin/wiki/GuessBufferLanguage
;; ;;
;; ;; The following adds guess-language function. Very helpful when using
;; ;; several languages in Emacs.

;; ;; Language pattern rules, just names some common used words of the
;; ;; languages that will be guessed upon.
;; (defvar guess-language-rules
;;   '(("american" ;; "english"
;;      . "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>")
;;     ("dansk"
;;      . "\\<\\(og\\|den\\|det\\|der\\|vi\\|eller\\|nogle\\|kun\\)\\>")))

;; ;; main guess method
;; (defun guess-buffer-language ()
;;   "Guess language in the current buffer."
;;   (save-excursion 
;;     (goto-char (point-min))
;;     (let* ((count (map 'list (lambda (x)
;;                                (let ((count (count-matches (cdr x))))
;;                                  (cons (if (stringp count)
;;                                            (string-to-number count) ;; emacs v21
;;                                            count)                   ;; emacs v22
;;                                        (car x))))
;;                        guess-language-rules))
;;            (key (car (sort (map 'list 'car count) '>))))
;;       (if (number-or-marker-p key)
;;           (cdr (assoc key count))
;;           ;; return first language as default
;;           (car (car guess-language-rules))))))

;; ;; hook the language guesser on to the flyspell mode
;; (add-hook 'flyspell-mode-hook
;; 	  (lambda () (ispell-change-dictionary
;; 		      (guess-buffer-language))))

;; ;;; interactive wrapper
;; (defun guess-language ()
;;   "Set the language of this buffer by guess."
;;   (interactive)
;;   (ispell-change-dictionary (guess-buffer-language))
;;   (setq flyspell-mode 1)
;;   (flyspell-buffer))

;; ;; Handling latex documents
;; (setq TeX-view-program-list '(("Preview" "open %o")))
;; (setq TeX-view-program-selection '((output-pdf "Preview")))
;; (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
;; (setq TeX-PDF-mode t)


;; (require 'bbdb)
;; (add-hook 'message-mode-hook
;;           (function (lambda() 
;;                       (local-set-key (kbd "<tab>") 'bbdb-complete-name)
;;                       )))
;; (setq bbdb-complete-name-full-completion t)
;; (setq bbdb-completion-type 'primary-or-name)
;; (setq bbdb-complete-name-allow-cycling t)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(global-edit-server-edit-mode t)
;;  '(openwith-associations (quote (("\\.pdf\\'" "open" (file)) ("\\.mp3\\'" "open" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "open" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "open" (file)))))
;;  '(openwith-mode t)
;;  '(org-agenda-files (quote ("~/Dropbox/akonring/org/index.org" "~/Dropbox/education/eth/eth.org" "~/Dropbox/org/contacts.org" "~/Dropbox/org/misc.org" "/Users/anderskonring/Dropbox/org/u16.org" "/Users/anderskonring/Dropbox/org/habits.org" "/Users/anderskonring/Dropbox/org/mustrun.org" "/Users/anderskonring/Dropbox/org/notes.org")))
;;  '(org-agenda-ndays 14)
;;  '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . "open") ("\\.pdf\\'" . "open"))))
;;  '(org-modules (quote (org-bbdb org-bibtex org-crypt org-ctags org-docview org-gnus org-id org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mac-link-grabber))))


;; ;; Shortcut to show and hide different headings
;; (defun ded/org-show-next-heading-tidily ()
;;   "Show next entry, keeping other entries closed."
;;   (if (save-excursion (end-of-line) (outline-invisible-p))
;;       (progn (org-show-entry) (show-children))
;;     (outline-next-heading)
;;     (unless (and (bolp) (org-on-heading-p))
;;       (org-up-heading-safe)
;;       (hide-subtree)
;;       (error "Boundary reached"))
;;     (org-overview)
;;     (org-reveal t)
;;     (org-show-entry)
;;     (show-children)))

;; (defun ded/org-show-previous-heading-tidily ()
;;   "Show previous entry, keeping other entries closed."
;;   (let ((pos (point)))
;;     (outline-previous-heading)
;;     (unless (and (< (point) pos) (bolp) (org-on-heading-p))
;;       (goto-char pos)
;;       (hide-subtree)
;;       (error "Boundary reached"))
;;     (org-overview)
;;     (org-reveal t)
;;     (org-show-entry)
;;     (show-children)))

;; (setq org-use-speed-commands t)
;; (add-to-list 'org-speed-commands-user
;;              '("n" ded/org-show-next-heading-tidily))
;; (add-to-list 'org-speed-commands-user
;;              '("p" ded/org-show-previous-heading-tidily))

;; ;; Calendar setup
;; (setq org-icalendar-use-scheduled '(todo-start event-if-todo))


;; ;; Editing hledger stuff in emacs
;; (load-file "hledger-mode.el")
;; (require 'hledger-mode)
;; (setq org-file-apps
;;       org-file-apps-defaults-macosx)

;; ;; Org publishing
;; (setq org-publish-project-alist
;;       '(("akonring-pages"
;;          :base-directory "~/Dropbox/akonring/org/"
;;          :html-extension "html"
;;          :base-extension "org"
;;          :publishing-directory "~/Dropbox/akonring/public_html"
;;          :publishing-function (org-html-publish-to-html)
;;          :auto-sitemap t
;;          :recursive t
;;          :makeindex t
;;          :preserve-breaks nil
;;          :sitemap-sort-files chronologically
;;          :with-tasks nil
;;          :section-numbers nil
;;          :with-toc nil
;;          :html-head-extra
;;          "<link rel=\"stylesheet\" href=\"u/bootstrap.min.css\" />
;; <link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />
;;     <script src=\"http://www.google.com/jsapi\" type=\"text/javascript\"></script>
;;     <script type=\"text/javascript\">
;;       google.load(\"jquery\", \"1.3.1\");
;;     </script>"
;;          :html-preamble ,html-preamble
;;          :htmlized-source t
;;          :html-postamble ,html-postamble)
;;         ("akonring-articles"
;;          :base-directory "~/Dropbox/akonring/org/"
;;          :publishing-directory "~/Dropbox/akonring/public_html/"
;;          :base-extension "org"
;;          :html-extension "html"
;;          :recursive t
;;          :makeindex t
;;          :preserve-breaks nil
;;          :sitemap-sort-files chronologically         
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 3
;;          :section-numbers nil
;;          :with-toc nil
;;          :htmlized-source t
;;          :html-postamble t
;;          :html-preamble t)
;;         ("akonring-articles-source"
;;          :base-directory "~/Dropbox/akonring/org/"
;;          :base-extension "org"
;;          :publishing-directory "~/Dropbox/public_html/"
;;          :publishing-function (org-org-publish-to-org)
;;          :recursive t
;;          :with-tasks nil
;;          :htmlized-source t)
;;         ("akonring-rss"
;;           :base-directory "~/Dropbox/akonring/org/"
;;           :base-extension "org"
;;           :publishing-directory "~/Dropbox/akonring/public_html/"
;;           :publishing-function (org-rss-publish-to-rss)
;;           :html-link-home "~/Dropbox/akonring/public_html/"
;;           :html-link-use-abs-url t
;;           :exclude ".*"
;;           :include ("blog.org"))
;;         ("akonring-images"
;;          :base-directory "~/Dropbox/akonring/org/images/"
;;          :base-extension "jpg\\|gif\\|png"
;;          :publishing-directory "~/Dropbox/akonring/public_html/images/"
;;          :publishing-function org-publish-attachment)
;;         ("akonring-css"
;;          :base-directory "~/Dropbox/akonring/org/stylesheets/"
;;          :base-extension "css"
;;          :publishing-directory "~/Dropbox/akonring/public_html/stylesheets/"
;;          :publishing-function org-publish-attachment)
;;         ("website" :components ("akonring-articles" "akonring-rss"))))

;; (setq org-feed-alist
;;       '(("article-feed"
;;          "file:///Users/anderskonring/Dropbox/akonring/public_html/akonring-blog.xml"
;;          "/Users/anderskonring/Dropbox/akonring/org/index.org" "Articles")))

;; (global-set-key (kbd "C-h C-f") 'find-function)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (require 'ox-rss)
;; (setq ns-pop-up-frames nil)
