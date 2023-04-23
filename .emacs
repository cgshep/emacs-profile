; Enable global line wrapping
(global-visual-line-mode 1)
(set-face-attribute 'vertical-border
                    nil
                    :foreground "#282a2e")

;; Activate Marmalade and Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Remove toolbar and scrollbar
(tool-bar-mode -1) 
(scroll-bar-mode -1)
(menu-bar-mode -1)

(electric-pair-mode 1)

;; Ido mode
(ido-mode 1)

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
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-u") #'helm-find-files)

;; Direx
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
;(global-auto-complete-mode t)

(setq erc-nick "cgshep")

(require 'magit)
(global-linum-mode t)
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

(setq python-indent-offset 4)
;(elpy-enable)
;(setq elpy-rpc-python-command "python3")

(setq make-backup-files nil) ; stop creating ~ files

(defun carltons-org-tex-headers ()
  "Add Latex headers (fancyhdr, footer etc.) for a basic doc theme"
  (interactive)
  (let ((rhul-headers '("#+TITLE:"
			"#+AUTHOR: Carlton Shepherd"
			"#+OPTIONS: toc:nil, num:nil, email:t"
			"#+LATEX_HEADER_EXTRA: \\renewcommand{\\familydefault}{\\sfdefault}"
			"#+LATEX_HEADER_EXTRA: \\usepackage[a4paper, total={6in, 8in}]{geometry}"
			"#+LATEX_HEADER_EXTRA: \\usepackage{fancyhdr}"
			"#+LATEX_HEADER_EXTRA: \\usepackage[export]{adjustbox}"
			"#+LATEX_HEADER_EXTRA: \\renewcommand{\\rmdefault}{ppl}"
			"#+LATEX_HEADER_EXTRA: \\pagestyle{fancy}"
			"#+LATEX_HEADER_EXTRA: \\fancyhf{}"
			"#+LATEX_HEADER_EXTRA: \\rhead{Carlton Shepherd}"
			"#+LATEX_HEADER_EXTRA: \\lhead{DOC NAME}"
			"#+LATEX_HEADER_EXTRA: \\rfoot{Page \\thepage}"
			;; We can add a logo in the bottom left using			"#+LATEX_HEADER_EXTRA: \\lfoot{\\includegraphics[width=2.6cm,valign=c]{/home/carlton/rhul-small.jpg}}"
			)))
    (mapc (lambda (line)
	    (insert line) (newline))
	  rhul-headers)))

;;
;; Web search functions
;;
(setq search-engine "duckduckgo.com/")

(defun launch-new-tab (url)
  "Launch a new Firefox tab with a given URL."
  (shell-command
   (concat "firefox --new-tab " url)))

(defun search-web-at-point ()
  "Search the web for a given search engine and search term."
  (interactive)
  (shell-command
   (launch-new-tab
    (concat search-engine
	    (replace-regexp-in-string
	     "\s" "%20"
	     (thing-at-point 'line))))))

(defun open-url ()
  "Open a new URL on the current line."
  (interactive)
  (launch-new-tab
   (thing-at-point 'line)))


(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
; Change C indentation. Default is 2 spaces; not enough!
(setq-default c-basic-offset 4)

;(add-to-list 'load-path "~/projects/encrypt-region/")
;(add-to-list 'load-path "~/projects/fancy-reading-mode/")
;(add-to-list 'load-path "~/.emacs.d/elpa/package-lint-0.16")
;(require 'package-lint)
;(require 'encrypt-region)
;(require 'fancy-reading-mode)
;(setq encrypt-region--key "616461746120646e6d20726f20656164")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426E" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default)))
 '(package-selected-packages
   (quote
    (rust-mode ## helm-tramp ox-gfm treemacs powerline org markdown-mode magit helm gruvbox-theme elpy direx color-theme-sanityinc-tomorrow auto-complete)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828")))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Mono" :foundry "DAMA" :slant normal :weight normal :height 125 :width normal)))))
