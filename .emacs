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
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)
                   ))
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
			(add-hook 'clojure-mode-hook       #'enable-paredit-mode)
                        (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
                        (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
                        (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
                        (add-hook 'scheme-mode-hook           #'enable-paredit-mode)))))

(setq
 my:el-get-packages
 '(el-get				
   escreen            			
   twittering-mode
   switch-window			
   company-mode
   clojure-mode
   color-theme-solarized
   org-ac
   gnuplot-mode
   cider
   spinner
   lorem-ipsum
   ido-ubiquitous
   inf-ruby
   projectile
   openwith
   ledger-mode
   ess
   org-mode
   exec-path-from-shell
   undo-tree
   browse-kill-ring
   yasnippet
   ;;json-mode
   ;; mu4e (build script corrupted)
   color-theme
   nodejs-repl
   js2-mode
   web-mode
   color-theme-tango
   idomenu))	                


;; Some recipes require extra tools to be installed
;; CVS install
(when (ignore-errors (el-get-executable-find "cvs"))  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

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

;;(color-theme-solarized-dark)            ; pick my favorite color-theme
					;(load-theme 'cyberpunk t)

;; choose your own fonts, in a system dependend way
(when (eq system-type 'darwin)
  (set-default-font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))

(global-hl-line-mode 1)			; highlight current line
;; (global-linum-mode 0)			; add line numbers on the left

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

(setq paredit-replace-disputed-keys t)

;; Awesome. winner-mode has a undo mechanism on window layouts
;; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; Auto-reload buffer
(global-auto-revert-mode 1)

;; Shellmode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(savehist-mode) 

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

;; Setting environment for shell and term
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "C-x C-b") 'ido-switch-bufferxo)
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
(setq ns-pop-up-frames nil)

(show-paren-mode t)
(tooltip-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Find file from home
(setq default-directory "~/")

(projectile-global-mode)

;; Stop spreading the *~ files all over
;; keep the temporary files in a temp-directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

;; Load .emacs in one line
(defun load-dot-emacs ()
  "Loading dot emacs file"
  (interactive)
  (load-file "~/.emacs"))

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
    (write-region (point-min) (point-max) "~/.emacs.d/.emacs-persistent-scratch")))

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
(require 'org)
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/Dropbox/org/"))
(setq org-indent-mode 1)

(defun org-dir ()
  "Open dired-mode in the org directory"
  (interactive)
  (find-file "/Users/akonring/Dropbox/org"))

;; ERC autojoin and open
(setq erc-auto-reconnect 1)
(setq erc-join-buffer 'bury)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#org-mode" "#emacs" "#nethack" "#clojure" "#clojurescript" "##math" "##crypto" "#bitcoin" "#bitcoinj") ("oftc.net" "#zcash")))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; switch to last active
    (when (y-or-n-p "Start ERC? ") ;; maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "akonring")
      (erc :server "irc.oftc.net" :port 6667 :nick "akonring"))))

;; Spelling

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-list-command "--list")


;; Add flyspell to related modes
(dolist (hook '(text-mode-hook
                org-mode-hook
                magit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Remove flyspell from unrelated modes
(dolist (hook '(change-log-mode-hook 
 		log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes
   (quote
    ("71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(fci-rule-color "#383838")
 '(magit-branch-arguments (quote ("--track")))
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "open"
      (file))
     ("\\.\\(?:mpeg\\|avi\\|wmv\\)\\'" "open"
      ("-idx" file))
     ("\\.\\(?:jpg\\|png\\|jpeg\\)\\'" "open"
      (file)))))
 '(openwith-mode t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/chainalysis.org" "~/Dropbox/org/read.org" "~/Dropbox/org/akonring.org" "~/Dropbox/edu/thesis/agenda.org" "/Users/akonring/Dropbox/org/notes.org")))
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
    (org-bbdb org-bibtex org-crypt org-ctags org-docview org-id org-info org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mu4e ox-bibtex)))
 '(send-mail-function (quote smtpmail-send-it))
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

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(require 'dash)
(defun parse-layout (layout)
  (let ((string (get-string-from-file layout)))
    (->> string
	 (replace-regexp-in-string "\\(\"\\).*\\'" "\"")
	 (replace-regexp-in-string "\\(\%\\).*\\'" "%%"))))

(setq org-html-preamble (parse-layout "/Users/akonring/Dropbox/akonring/public_html/layout.pre"))
(setq org-export-html-style-include-default "")

(setq org-latex-pdf-process 
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf  %f"))

;; Org publishing
(setq org-publish-project-alist
      '(("pages"
         :base-directory "/Users/akonring/Dropbox/akonring/org/"
         :html-extension "html"
         :base-extension "org"
         :publishing-directory "/Users/akonring/Dropbox/akonring/public_html/"
         :publishing-function (org-html-publish-to-html)
         :recursive t
         :auto-sitemap t
         :makeindex t
         :preserve-breaks nil
         :sitemap-sort-files chronologically
         :with-tasks nil
         :section-numbers nil
         :with-toc nil
         :html-head-extra"<meta charset=\"utf-8\">
    <meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\">
    <title>akonring</title>
    <link href=\"stylesheets/styles.css\" media=\"all\" rel=\"stylesheet\" type=\"text/css\">
<link href=\"https://fonts.googleapis.com/css?family=Lato:300italic,700italic,300,700\" media=\"all\" rel=\"stylesheet\" type=\"text/css\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no\">"
         :htmlized-source t
         :html-postamble  "</div>")
	("homepage-css"
	 :base-directory "~/Dropbox/akonring/org/"
	 :base-extension "css"
	 :publishing-directory "~/Dropbox/public_html"
	 :publishing-function org-publish-attachment)))

(defun my-save-then-publish ()
  (interactive)
  (save-buffer)
  (load-dot-emacs)
  (org-save-all-org-buffers)
  (let (org-export-html-style-default)
    (setq org-export-html-style-default "")
    (org-publish "pages")))

(global-set-key (kbd "C-h C-f") 'find-function)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(ido-everywhere t)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



(global-set-key (kbd "A-u") 'revert-buffer)

(defun substi-danish-characters () 
  "Substitute all danish characters"
  (interactive)
  (progn
    (replace-string "æ" "ae")
    (replace-string "ø" "oe")
    (replace-string "å" "aa"))) 

;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;; (add-hook 'LaTeX-mode-hook (lambda ()
;; 			     (push
;; 			      '("latexmk" "latexmk -pdflatex='pdflatex -shell-escape' -pdf -bibtex -f  %s" TeX-run-TeX nil t
;; 				:help "Run latexmk on file")
;; 			      TeX-command-list))
;; 	  (push
;; 	   '("thesis" "make thesis" TeX-run-TeX nil t
;; 	     :help "Run latexmk on file")
;; 	   TeX-command-list)

;; (push
;;  '("thesis-quick" "make latex" TeX-run-TeX nil t
;;    :help "Run latexmk on file")
;;  TeX-command-list)

;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(require 'mm-util)
(add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)
(global-set-key "\M- " 'hippie-expand)

;; The actual expansion function
(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))

  ;; english-words.txt is the fallback dicitonary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
			 'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
	  (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
	(if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)
    ))

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-by-dict))

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")

(setq fill-column 20)

(setq org-startup-truncated t)

(require 'uniquify)
(setq org-indent-indentation-per-level 2)
(put 'downcase-region 'disabled nil)
(setq org-list-allow-alphabetical t)
(add-hook 'org-mode-hook (lambda ()
                           (setq truncate-lines nil)))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO" "DONE")))

(defadvice org-display-inline-images
    (around handle-openwith
	    (&optional include-linked refresh beg end) activate compile)
  (if openwith-mode
      (progn
        (openwith-mode -1)
        ad-do-it
        (openwith-mode 1))
    ad-do-it))
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

(setq load-path (append (list (expand-file-name "~/emacs/icalendar")) load-path))
(require 'icalendar)

;; this hook saves an ics file once an org-buffer is saved
(run-with-idle-timer 10000 t 'org-icalendar-combine-agenda-files)

(add-to-list 'load-path "~/.emacs.d/el-get/mu4e/mu4e")
(require 'mu4e)

(defun no-auto-fill ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1))

(add-hook 'mu4e-compose-mode-hook #'no-auto-fill)
(setq mu4e-attachment-dir  "~/Downloads")
(setq mu4e-maildir "~/.mail/personal")
(setq mu4e-mu-binary  "~/.emacs.d/el-get/mu4e/mu/mu")

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Mail")
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
      '(("/INBOX" . ?i)
        ("/Drafts" . ?d)
        ("/Sent Mail" . ?s)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about me
(let ((mail "anders.konring@gmail.com"))
  (setq
   mu4e-user-mail-address-list (list mail "ako@cs.au.dk")
   user-mail-address mail
   user-full-name  "Anders Konring"
   mu4e-compose-signature "anders konring"
   mu4e-compose-dont-reply-to-self t))

(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "anders.konring@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Turn off sounds
(setq visible-bell t)

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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Way too slow with big json files
;;(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
;;(add-to-list 'auto-mode-alist '("\\.ts$" . js-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)

(setq org-export-before-processing-hook nil)

(require 'ob-plantuml)

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8024/plantuml.8024.jar")

(setq bookmark-default-file "~/Dropbox/common/emacs/bookmarks.bmk" bookmark-save-flag 1)

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4
				  tab-width 4
				  indent-tabs-mode nil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)


(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(setq org-latex-caption-above nil)
(eval-after-load 'org
  (progn
    (define-key org-mode-map (kbd "<C-S-up>") nil)
    (define-key org-mode-map (kbd "<C-S-left>") nil)
    (define-key org-mode-map (kbd "<C-S-right>") nil)
    (define-key org-mode-map (kbd "<C-S-down>") nil)))


;; SSH functions
(defun chainalysis-anders
    (interactive)
  (find-file "/ssh:root@anders.chainalysis.com:/home/"))
(setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p)

(define-key global-map (kbd "A-s") 'shell)
(setenv "ESHELL" "bash")
