;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Begin .emacs file
;; Initialization file for the emacs editor
;; environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get all the good stuff from
;; Standard lisp libraries
;; Marmelade: http://marmalade-repo.org/
;; MELPA: http://melpa.milkbox.net/
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; List of all my beloved emacs packages
(setq list-of-awesome-packages '(auto-complete 
                                 bash-completion
                                 clojure-mode
                                 clojurescript-mode))

;; Remember to load installed packages before the check
(package-initialize)

;; Mapc is only used for the sideeffects
;; and will not return a seqence i.e. will return nil
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 list-of-awesome-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize the look and feel of Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To maximize size of frame
;; take a nap and then maximize
(run-with-idle-timer 0.1 nil 'maximize-frame)

;; Some small configuration
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode t)
(scroll-bar-mode -1)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Emacs Enviroment Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default js-basic-offset 2)
(windmove-default-keybindings)

;; Show recent files and bind to keys
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Getting the most important stuff on my emacs path
(setq my-paths '( "/usr/local/share/npm/bin"
                  "/Users/anderskonring/.rbenv/shims"
                  "/usr/local/heroku/bin"
                  "/usr/local/pgsql/bin"
                  "/usr/local/bin"
                  "/usr/bin"
                  "/bin"
                  "/usr/sbin"
                  "/sbin"
                  "/opt/X11/bin"
                  "/usr/texbin"
                  "/bin"
                  ))
;; Setting Paths
(when (string-equal system-type "darwin")
  (setenv "PATH" (mapconcat 'identity my-paths ":"))
  (setq exec-path (append my-paths (list "." exec-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major and Minor mode Hooks and Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using compass instead to compile my css
(setq scss-compile-at-save nil)

;; Load .emacs in oneliner
(defun load-dot-emacs ()
  "Loading dot emacs file"
  (interactive)
  (load-file "~/.emacs"))
(load-file "~/.emacs.d/proofgeneral/generic/proof-site.el")






(require 'tramp)
(setq tramp-default-method "ssh")
(setq default-directory "~/")


