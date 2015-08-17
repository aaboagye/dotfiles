;;; Comentary:

;;; Code:
;;; -- Interactive Do mode.
(require 'ido)
(ido-mode t)

;; Keybindings
; Git grep.
(global-set-key (kbd "<f3>") 'vc-git-grep)

;; always revert files that change outside of the editor
(global-auto-revert-mode t)

;; autocomplete
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;; Ace Jump mode;;
;; ace jump mode major function
;;
(add-to-list 'load-path "~/.emacs.d/ace-jump-mode/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(add-to-list 'load-path "~/.emacs.d/column-enforce-mode")
(require 'column-enforce-mode)
(global-column-enforce-mode t)

; column marking
(require 'column-marker)

;; Fill column 80
(setq-default fill-column 80)

;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; show which function you're in
(which-func-mode t)

;; parens
(show-paren-mode t)
;; flex
'(ido-enable-flex-matching t)
'(save-place t nil (saveplace))

;; whitespace mode(require 'whitespace)
; (setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Colors
(setq font-lock-maximum-decoration t)
;; (require 'deep-thought-theme)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dark-laptop)))

;; disable toolbar
(tool-bar-mode -1)

;; Move around with shift+arrow
(windmove-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "xos4" :family "Terminus"))))
 '(column-marker-1 ((t (:background "red")))))

;; C default style
(setq c-default-style "linux")

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Show column marker
(column-number-mode t)

;; Tabs!
(setq-default indent-tabs-mode t)
(setq c-default-style "linux")

;; Refresh all buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; transparency
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha value))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;(provide '.emacs)
;;; .emacs ends here
