;; Get all the good stuff from
;; Standard lisp libraries
;; Marmelade: http://marmalade-repo.org/
;; MELPA: http://melpa.milkbox.net/
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(run-with-idle-timer 0.1 nil 'maximize-frame)

;; Using compass instead to compile my css
(setq scss-compile-at-save nil)

;; Load .emacs in oneliner
(defun load-dot-emacs ()
  "Loading dot emacs file"
  (interactive)
  (load-file "~/.emacs"))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Aesthetics is honey for the soul
(load-theme 'tango-dark)

;; Remove the menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)


;; Show recent files and bind to keys
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; RUBY configuration
;; Setting rbenv path to use ruby global version
(when (string-equal system-type "darwin")
  (let (
        (mypaths
         '(
           ":/.rbenv/shims"
           "/.rbenv/bin"
           "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/mysql/bin"
           "/usr/local/Cellar/emacs/HEAD/share/emacs/24.3.50"
           ) 
         )
        )
    (setenv "PATH" (mapconcat 'identity mypaths ":") )
    (setq exec-path (append mypaths (list "" exec-directory)) )
    ) 
  )
