;;;
;;; string-prefix-p is not defined in Emacs 23.1
;;;
;(defun requesti-testing-string-prefix-p (prefix str &optional ignore-case)
;  (let ((case-fold-search ignore-case))
;    (string-match-p (format "^%s" (regexp-quote prefix)) str)))

(setq debug-on-error t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/todo.org")))
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "Gohu" :family "GohuFont")))))

(require 'ido)
(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
;; Tabs are evil
(setq-default indent-tabs-mode nil)
(icomplete-mode 99)
;; Move around with shift+arrow
(windmove-default-keybindings)
;; Braces + Indentation
(setq c-default-style "linux"
          c-basic-offset 3)
;; Disable the toolbar
(tool-bar-mode -1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; Show which function you're in
(which-func-mode t)

;; Colors!
;(add-to-list 'load-path "/users/aboagye/.emacs.d/emacs-color-theme-solarized/")
(setq font-lock-maximum-decoration t)
;(require 'color-theme-solarized)
(require 'color-theme)
(eval-after-load "color-theme"
   '(progn
      (color-theme-initialize)
      (color-theme-dark-laptop)))


;;;
;;; Org-Mode setup
;;;
(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-font-lock-mode 1)

(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)
   )
)

(require 'remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map [(control meta ?r)] 'remember)

(custom-set-variables
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-default-notes-file "~/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
	   ("c" todo "DONE|DEFERRED|CANCELLED" nil)
	   ("w" todo "WAITING" nil)
	   ("W" agenda "" ((org-agenda-ndays 21)))
	   ("A" agenda ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
	     (org-agenda-ndays 1)
	     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	   ("u" alltodo ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					  (quote regexp) "\n]+>")))
	     (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/todo.org" "Tasks")
	   (110 "* %u %?" "~/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

(setq org-todo-keywords
  '((sequence "TODO" "STARTED" "WAITING"  "|" "DELEGATED" "DONE" "CANCELLED")
    (sequence "ASSIGNED" "INVESTIGATING" "ROOT CAUSE" "REVIEW" "|" "PUSHED")))

;;
;; MELPA
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 

;;
;; ace-jump-mode
;;
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)