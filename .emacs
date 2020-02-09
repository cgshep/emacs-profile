(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (treemacs company direx-grep jedi jedi-core org yaml-mode w3m virtualenv tidy smart-mode-line sly scheme-complete scala-mode2 ruby-compilation quack python-mode python pymacs pyflakes pydoc powerline ox-reveal ox-ioslide ox-html5slide org-pdfview org-gnome org-board org-beautify-theme org-alert org-ac nose markdown-mode magit lua-mode iedit idomenu ido-ubiquitous iasm-mode helm haskell-mode guile-scheme geiser fuzzy fullscreen-mode flymake-python-pyflakes flymake-hlint flymake-haskell-multi flymake flycheck flx-ido eshell-prompt-extras eshell-manual eshell-did-you-mean emacs-cl ecb dired+ color-theme c-eldoc auto-indent-mode auto-complete-auctex auto-auto-indent auctex atom-dark-theme ac-js2 ac-ispell ac-inf-ruby))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(background-color "#111111")
 '(flx-ido-mode t)
 '(ido-enable-flex-matching t)
 '(ido-ubiquitous-mode t))

;; Enable global line wrapping
(global-visual-line-mode 1)
(set-face-attribute 'vertical-border
                    nil
                    :foreground "#282a2e") 
;; Activate Marmalade and Melpa
(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Remove toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Fullscreen mode
(global-set-key (kbd "<f11>") 'fullscreen-mode-fullscreen-toggle)

;; Blank scratch message
(setq initial-scratch-message nil)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Set up agenda files
(require 'org)
(define-key global-map "\C-ca" 'org-agenda)

;; Ido mode
(require 'ido)
(ido-mode t)

;; Remap window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Remap paragraph movement
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-x p") 'eval-buffer)

;; Helm
(require 'helm-config)

;; Direx
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

(global-auto-complete-mode t)

(setq erc-nick "cgshep")

(require 'magit)

;(global-linum-mode 1)

(global-company-mode 1)

(require 'treemacs)
(global-set-key (kbd "C-x t") 'treemacs)

;; Move region down
(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<down>") 'move-region-down)
(global-set-key (kbd "M-<up>") 'move-region-up)


