;; -*- mode: lisp; indent-tabs-mode: nil -*-

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
   (:name google-this
          :after (progn
                   (setq google-this-mode 1)
                   (global-set-key (kbd "A-g") 'google-this)))
   (:name paredit
          :after (progn (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
                        (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
                        (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
                        (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
                        (add-hook 'scheme-mode-hook           #'enable-paredit-mode)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				
   escreen            			
   twittering-mode
   switch-window			
   company-mode
   clojure-mode
   twitter
   cider
   color-theme-solarized
   org-ac
   lorem-ipsum
   ido-ubiquitous
   inf-ruby
   openwith
   ledger-mode
   org-mode
   undo-tree
   browse-kill-ring
   yasnippet
   json-mode
   color-theme		                
   color-theme-tango))	                


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

;; visual settings
(setq inhibit-splash-screen t)		; no splash screen, thanks
(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drawn, don't have it empty
  (menu-bar-mode -1))
(color-theme-solarized-dark)            ; pick my favorite color-theme

;; choose your own fonts, in a system dependend way
(if (and (string-match "apple-darwin" system-configuration) window-system)
    (set-face-font 'default "Monaco-12")
  (set-face-font 'default "Monospace-9"))

(global-hl-line-mode 1)			; highlight current line
(global-linum-mode 1)			; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; under mac, have Command as Meta and keep Option for localized input
(when (string-match "apple-darwin" system-configuration)
  (setq mac-allow-anti-aliasing t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

;; use the clipboard, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; stop windmoving when in Org-mode
(setq org-replace-disputed-keys t)
(setq paredit-replace-disputed-keys t)

;; Awesome. winner-mode has a undo mechanism on window layouts
;; winner-mode provides C-<left> to get back to previous window layout
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
;; See guide by @mickeynp at
;; http://www.masteringemacs.org/article/running-shells-in-emacs-overview

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
(setq org-completion-use-ido t)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
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
(global-undo-tree-mode)
(defalias 'redo 'undo-tree-redo)
(defalias 'undo 'undo-tree-undo)
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

;; persistent scratch buffer
(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) "~/.emacs-persistent-scratch")))

(defun load-persistent-scratch ()
  "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p "~/.emacs-persistent-scratch")
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents "~/.emacs-persistent-scratch"))))
(push #'load-persistent-scratch after-init-hook)
(push #'save-persistent-scratch kill-emacs-hook)
(run-with-idle-timer 300 t 'save-persistent-scratch)

;; Globally enable company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Org mode
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/Dropbox/org/"))
(setq org-indent-mode 1)

(defun org-dir ()
  "Open dired-mode in the org directory"
  (interactive)
  (find-file org-agenda-files))

;; ERC autojoin and open
(setq erc-auto-reconnect 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#wiki" "#nethack" "#clojure" "#clojurescript" "##math" "##crypto")))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; switch to last active
    (when (y-or-n-p "Start ERC? ") ;; maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "akonring"))))

;; Spelling

(setq ispell-personal-dictionary "~/.ispell")

;; Remove flyspell from unrelated modes
(dolist (hook '(change-log-mode-hook 
 		log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(defun my-ispell-danish-dictionary ()
  "Switch to the Danish dictionary."
  (interactive)
  (ispell-change-dictionary "dansk"))

(require 'easymenu)
(easy-menu-add-item  
 nil 
 '("tools" "spell")
 ["Select Danish Dict" my-ispell-danish-dictionary t])

;; Language pattern rules, just names some common used words of the
;; languages that will be guessed upon.
(defvar guess-language-rules
  '(("american" ;; "english"
     . "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>")
    ("dansk"
     . "\\<\\(og\\|den\\|det\\|der\\|vi\\|eller\\|nogle\\|kun\\)\\>")))

;; main guess method
(defun guess-buffer-language ()
  "Guess language in the current buffer."
  (save-excursion 
    (goto-char (point-min))
    (let* ((count (map 'list (lambda (x)
                               (let ((count (count-matches (cdr x))))
                                 (cons (if (stringp count)
                                           (string-to-number count) ;; emacs v21
                                         count)                   ;; emacs v22
                                       (car x))))
                       guess-language-rules))
           (key (car (sort (map 'list 'car count) '>))))
      (if (number-or-marker-p key)
          (cdr (assoc key count))
        ;; return first language as default
        (car (car guess-language-rules))))))

;; hook the language guesser on to the flyspell mode
(add-hook 'flyspell-mode-hook
	  (lambda () (ispell-change-dictionary
		      (guess-buffer-language))))

;; interactive wrapper
(defun guess-language ()
  "Set the language of this buffer by guess."
  (interactive)
  (ispell-change-dictionary (guess-buffer-language))
  (setq flyspell-mode 1)
  (flyspell-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-binary-path "/Users/anderskonring/.cabal/bin/hledger")
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "open"
      (file))
     ("\\.mp3\\'" "open"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "open"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "open"
      (file)))))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/projects.org" "~/Dropbox/org/logs.org" "~/Dropbox/education/eth/ethp.org" "/Users/anderskonring/Dropbox/org/notes.org" "~/Dropbox/akonring/org/index.org" "~/Dropbox/org/contacts.org" "/Users/anderskonring/Dropbox/org/mustrun.org")))
 '(org-agenda-ndays 14)
 '(org-capture-templates
   (quote
    (("m" "TODO from Mail" entry
      (file+headline "~/Dropbox/org/notes.org" "Inbox")
      "* TODO %?
  Link: %A")
     ("e" "Reference to mail" plain
      (file+headline "~/Dropbox/education/eth/ethp.org" "References")
      "Reference: %? %A"))))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "open %s")
     ("\\.pdf\\'" . "open %s"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-crypt org-ctags org-docview org-id org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-beamer org-mu4e)))
 '(tex-dvi-view-command
   (quote
    (cond
     ((eq window-system
	  (quote x))
      "xdvi")
     ((eq window-system
	  (quote w32))
      "yap")
     (t "dvi2tty * | cat -s"))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-log-done 'time)

;; Org publishing
(setq org-publish-project-alist
      '(("akonring-pages"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/"
         :html-extension "html"
         :base-extension "org"
         :publishing-directory "/Users/anderskonring/Dropbox/akonring/public_html/"
         :publishing-function (org-html-publish-to-html)
         :recursive t
         :auto-sitemap t
         :makeindex t
         :preserve-breaks nil
         :sitemap-sort-files chronologically
         :with-tasks nil
         :section-numbers nil
         :with-toc nil
         :html-head-extra
         "<link rel=\"stylesheet\" href=\"u/bootstrap.min.css\" />
<link rel=\"stylesheet\" href=\"index.css\" type=\"text/css\" />
    <script src=\"http://www.google.com/jsapi\" type=\"text/javascript\"></script>
    <script type=\"text/javascript\">
      google.load(\"jquery\", \"1.3.1\");
    </script>"
         :html-preamble ,html-preamble
         :htmlized-source t
         :html-postamble ,html-postamble)
        ("akonring-articles"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/articles/"
         :publishing-directory "/Users/anderskonring/Dropbox/akonring/public_html/articles"
         :base-extension "org"
         :html-extension "html"
         :recursive t
         :makeindex t
         :preserve-breaks nil
         :sitemap-sort-files chronologically         
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :htmlized-source t
         :html-postamble t
         :html-preamble t)
        ("akonring-articles-source"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/articles/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/public_html/"
         :publishing-function (org-org-publish-to-org)
         :recursive t
         :with-tasks nil
         :htmlized-source t)
        ("akonring-rss"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/akonring/public_html/"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "/Users/anderskonring/Dropbox/akonring/public_html/"
         :html-link-use-abs-url t
         :exclude ".*"
         :include ("akonring-blog.org"))
        ("akonring-images"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/Dropbox/akonring/public_html/images/"
         :publishing-function org-publish-attachment)
        ("akonring-css"
         :base-directory "/Users/anderskonring/Dropbox/akonring/org/stylesheets/"
         :base-extension "css"
         :publishing-directory "/Users/anderskonring/Dropbox/akonring/public_html/stylesheets/"
         :publishing-function org-publish-attachment)
        ("website" :components ("akonring-pages" "akonring-articles" "akonring-rss"))))

(global-set-key (kbd "C-h C-f") 'find-function)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(global-set-key (kbd "A-u") 'revert-buffer)

(defun substi-danish-characters () 
  "Substitute all danish characters"
  (interactive)
  (progn
    (replace-string "æ" "ae")
    (replace-string "ø" "oe")
    (replace-string "å" "aa"))) 

(add-hook 'latex-mode-hook '(lambda()
                              (defun tex-view ()
                                (interactive)
                                (tex-send-command "open"
                                                  (tex-append tex-print-file ".pdf")))))

(require 'mm-util)
(add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)
(global-set-key "\M- " 'hippie-expand)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")

(defun my-interactive-eval-to-repl (form)
  (let ((buffer nrepl-nrepl-buffer))
    (nrepl-send-string form (nrepl-handler buffer) nrepl-buffer-ns)))

(defun my-eval-last-expression-to-repl ()
  (interactive)
  (my-interactive-eval-to-repl (nrepl-last-expression)))

(eval-after-load 'nrepl
  '(progn 
     (define-key nrepl-interaction-mode-map (kbd "C-x C-e") 'my-eval-last-expression-to-repl)))

(setq fill-column 20)

(setq org-startup-truncated t)

(require 'uniquify)
(setq org-indent-indentation-per-level 2)
(put 'downcase-region 'disabled nil)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq org-columns-default-format "%20ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(when (fboundp 'eww)
  (progn
    (defun xah-rename-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer "eww" t))
    (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))

(setq org-agenda-skip-scheduled-if-done t)
(setq org-icalendar-combined-agenda-file "~/Dropbox/org/org.ics")
(setq org-icalendar-include-todo '(all))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))

;; this hook saves an ics file once an org-buffer is saved
(run-with-idle-timer 300 t 'org-icalendar-combine-agenda-files)

(add-to-list 'load-path "~/.emacs.d/el-get/mu/mu4e")

(require 'mu4e)

(setq mu4e-maildir "~/.mail/akonring")
(setq mu4e-mu-binary  "/Users/anderskonring/.emacs.d/el-get/mu/mu/mu")


(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/INBOX" . ?i)
       ("/drafts" . ?d)
       ("/archive" . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "anders.konring@gmail.com"
   user-full-name  "Anders Konring"
   mu4e-compose-signature "anders konring")

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "anders.konring@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Turn off sounds
(setq visible-bell t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Get size of folders
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                  (match-string 1))))))

 (define-key dired-mode-map (kbd "?") 'dired-get-size)
