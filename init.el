;; Path
(push "/usr/local/bin" exec-path)

;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      exec-path-from-shell
                      projectile
                      editorconfig
                      clojure-mode
                      coffee-mode
                      web-mode
                      js2-mode
                      scala-mode2
                      ensime
                      lua-mode
                      yaml-mode
                      markdown-mode
                      scss-mode
                      less-css-mode
                      emmet-mode
                      rainbow-mode
                      ;; enh-ruby-mode
                      robe
                      rbenv
                      cider
                      evil
                      evil-leader
                      company
                      neotree
                      flx-ido
                      ido-vertical-mode
                      ido-ubiquitous
                      ag
                      smex
                      guide-key
                      paredit
                      smartparens
                      anzu
                      ace-jump-mode
                      hideshowvis
                      magit
                      ctags
                      etags-select
                      expand-region
                      multiple-cursors
                      indent-guide
                      git-timemachine
                      git-gutter+
                      highlight-symbol
                      jazz-theme
                      twilight-theme
                      afternoon-theme
                      cyberpunk-theme
                      distinguished-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Common
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq visible-bell nil)
(global-linum-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(delete-selection-mode 1)

;; Frame
(defconst title "新しい自分に生まれ変わったら きっと、もっと美しい明日がやってくる")
(set-frame-name title)
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (select-frame frame)
                                        (set-frame-name title)))
(setq default-frame-alist '((top . 50) (left . 100)
                            (width . 130) (height . 55)))

;; Font
(set-face-attribute 'default nil :font "Monaco-14")

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'dichromacy t)
;; (load-theme 'tomorrow-day t)
;; (load-theme 'afternoon t)
(load-theme 'jazz t)
;; (load-theme 'cyberpunk t)
;; (load-theme 'twilight t)
;; (load-theme 'distinguished t)

;; Mode Line
(setq-default mode-line-format
  (list
     (propertize "%I " 'face 'font-lock-constant-face) ;; size

    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%l" 'face 'font-lock-type-face) ","
      (propertize "%c" 'face 'font-lock-type-face)
    ") "

    ;; the current major mode for the buffer.
    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (propertize " (Mod)"
                          'face 'font-lock-warning-face
                          'help-echo "Buffer has been modified")))

    '(:eval vc-mode)

    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; '(:eval minor-mode-alist)
    ))

;; Editorconfig
(load "editorconfig")

;; Evil
(setq-default evil-want-C-u-scroll t)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "u" 'universal-argument)
(evil-leader/set-key "," 'evil-buffer)
(evil-mode 1)
(add-hook 'temp-buffer-window-show-hook 'turn-off-evil-mode)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(define-key evil-insert-state-map "\C-c" 'evil-normal-state)
(define-key evil-insert-state-map "\C-g" 'evil-normal-state)

;; Git Gutter +
(global-git-gutter+-mode 1)

;; Global Key Binding
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)
(global-set-key (kbd "C-s") 'save-buffer)
(evil-leader/set-key "k" 'kill-buffer-and-window)
(evil-leader/set-key "q" 'kill-buffer-and-window)
(define-key evil-normal-state-map "\C-e" 'move-end-of-line)
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)
(define-key evil-normal-state-map "\C-l" 'other-window)
(define-key evil-normal-state-map "\C-j" 'evil-scroll-down)
(define-key evil-normal-state-map "\C-k" 'evil-scroll-up)
(define-key evil-normal-state-map "\C-\M-f" 'toggle-frame-fullscreen)
(define-key evil-normal-state-map "\C-o" 'pop-tag-mark)
(global-set-key (kbd "s-<up>") 'scroll-other-window-down)
(global-set-key (kbd "s-<down>") 'scroll-other-window)
(global-set-key "\M-h" 'paredit-backward-slurp-sexp)
(global-set-key "\M-j" 'paredit-backward-barf-sexp)
(global-set-key "\M-k" 'paredit-forward-barf-sexp)
(global-set-key "\M-l" 'paredit-forward-slurp-sexp)
(defun insert-erb-tag ()
  (interactive)
  (insert "<% %>")
  (evil-backward-char)
  (evil-backward-char))
(define-key evil-insert-state-map "\C-k" 'insert-erb-tag)
(define-key evil-insert-state-map "\C-l" "binding.pry")

;; Ctags
(require 'ctags)
(setq tags-revert-without-query t)
(setq tags-add-tables t)
(define-key evil-normal-state-map "\C-]" 'etags-select-find-tag-at-point)

;; Recentf
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(defun recentf-interactive-complete ()
  "find a file in the recently open file using ido for completion"
  (interactive)
  (let* ((all-files recentf-list)
         (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
         (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
         (ido-make-buffer-list-hook
          (lambda ()
            (setq ido-temp-list filename-list)))
         (filename (ido-read-buffer "Find Recent File: "))
         (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
         (result-length (length result-list)))
    (find-file
     (cond
      ((= result-length 0) filename)
      ((= result-length 1) (car result-list))
      ( t
        (let ( (ido-make-buffer-list-hook
                (lambda ()
                  (setq ido-temp-list result-list))))
          (ido-read-buffer (format "%d matches:" result-length))))))))
(define-key evil-normal-state-map (kbd "C-t") 'recentf-interactive-complete)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Company Mode
(global-company-mode 1)
(define-key evil-insert-state-map "\C-i" 'company-ispell)

;; Rainbow Mode
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'less-mode-hook 'rainbow-mode)

;; Magit Mode
(evil-leader/set-key "ms" 'magit-status)
(evil-leader/set-key "ml" 'magit-log)
(evil-leader/set-key "mc" 'magit-commit)
(evil-leader/set-key "mp" 'magit-push)
(evil-leader/set-key "md" 'magit-diff-unstaged)

;; Indent Guide
;; (indent-guide-global-mode)

;; Anzu
(global-anzu-mode 1)
(defun my-anzu-update-func (here total)
  (propertize (format "<%d/%d> " here total)
              'face 'font-lock-type-face))
(setq anzu-mode-line-update-function 'my-anzu-update-func)

;; Flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; Expand Region
(global-set-key (kbd "M-w") 'er/expand-region)

;; Hideshowvis
(add-hook 'clojure-mode-hook 'hideshowvis-enable)
(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)
;; (add-hook 'enh-ruby-mode-hook 'hideshowvis-enable)
(add-hook 'python-mode-hook 'hideshowvis-enable)
(add-hook 'lua-mode-hook 'hideshowvis-enable)
(add-hook 'js2-mode-hook 'hideshowvis-enable)
(add-hook 'coffee-mode-hook 'hideshowvis-enable)
(add-hook 'scala-mode-hook 'hideshowvis-enable)
(hideshowvis-symbols)
(define-key evil-normal-state-map "za" 'hs-hide-level)
(define-key evil-normal-state-map "zb" 'hs-show-all)

;; Highlight Chars
;; (require 'highlight-chars)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; Highlight Symbol
(defun highlight-symbol-nav-mode-on () (highlight-symbol-nav-mode 1))
(define-globalized-minor-mode global-highlight-symbol-nav-mode highlight-symbol-nav-mode highlight-symbol-nav-mode-on)
(global-highlight-symbol-nav-mode 1)
;; (global-set-key "\M-n" 'highlight-symbol-next)
;; (global-set-key "\M-p" 'highlight-symbol-prev)

;; Ace Jump Mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; Projectile
(projectile-global-mode)
(define-key evil-normal-state-map "\C-p" 'projectile-find-file)
(define-key evil-normal-state-map "\C-\M-p" 'projectile-find-file-other-window)
(define-key evil-normal-state-map "\C-b" 'projectile-switch-to-buffer)
(define-key evil-normal-state-map "\C-y" 'projectile-recentf)

;; Guide Key
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;; Smartparens
;; (require 'smartparens-config)
(require 'smartparens-ruby)
(add-hook 'ruby-mode-hook 'smartparens-mode)
;; (show-smartparens-global-mode t)
;; (smartparens-global-mode t)
;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; Paredit
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
;; (add-hook 'ruby-mode-hook 'enable-paredit-mode)
;; (add-hook 'js2-mode-hook 'enable-paredit-mode)
;; (add-hook 'coffee-mode-hook 'enable-paredit-mode)
;; (add-hook 'web-mode-hook 'enable-paredit-mode)
;; (add-hook 'scala-mode-hook 'enable-paredit-mode)

;; Rbenv
(global-rbenv-mode 1)
(push 'company-robe company-backends)

;; Robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; Seeing Is Believing
(defun seeing-is-believing ()
  "Replace the current region (or the whole buffer, if none) with the output
of seeing_is_believing."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region beg end "seeing_is_believing" nil 'replace)))

;; Ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(define-key evil-normal-state-map "\M-." 'ensime-edit-definition)
(define-key evil-normal-state-map "\M-m" 'ensime-show-uses-of-symbol-at-point)
;; (add-hook 'ensime-popup-buffer-mode-hook 'turn-off-evil-mode)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache?\\'" . web-mode))

(add-hook 'web-mode-hook (lambda ()
                           (define-key evil-normal-state-map "za" 'web-mode-fold-or-unfold)))

;;Emmet Mode
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'clojure-mode-hook 'emmet-mode)

;; Js2 Mode
;; (add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
(add-hook 'js2-mode-hook 'js2-mode-hide-warnings-and-errors)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))

;; Coffee Mode
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))

;; Scss Mode
(setq scss-compile-at-save nil)

;; Whitespace Mode
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Comment toggling
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(define-key evil-normal-state-map "gc" 'toggle-comment-on-line)
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)

;; Ido
(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(ido-ubiquitous-mode 1)

;; Neotree
(setq neo-window-width 32
      neo-banner-message nil
      neo-auto-indent-point t
      neo-persist-show nil
      neo-dont-be-alone t)

(evil-leader/set-key "l" 'neotree-toggle)
(evil-leader/set-key "." 'neotree-projectile-action)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "|") 'neotree-enter-vertical-split)
            (define-key evil-normal-state-local-map (kbd "-") 'neotree-enter-horizontal-split)
            (define-key evil-normal-state-local-map (kbd "i") 'neotree-preview)
            (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)))

;; Use OS X Color Picker in Emacs
(defun custom-color--choose-action (widget &optional _event) ; this function is only needed if you want to use color-picker in Easy Customization
  "customize `widget-color--chose-action' to not split the screen"
  (list-colors-display
   nil nil
   `(lambda (color)
      (when (buffer-live-p ,(current-buffer))
  (widget-value-set ',(widget-get widget :parent) color)
        (pop-to-buffer ,(current-buffer))))))

(defun nscolor2hex (color)
  "Converts colors from `NSColor' format to hex values"
  (concat "#"                           ; Add # at the front
          (mapconcat 'identity          ; concate the list
                     (mapcar '(lambda (x) ;returns ("hex" "hex" "hex")
                                (let ((col (lsh (string-to-int x) -8)))
                                  (if (< col 16)
                                      (format "0%x" col)
                                    (format "%x" col))))
                             (split-string (s-replace "\"" "" color) ",")) "")))

(defun color-picker (&optional list buffer-name callback)
  "Calls OS X color picker and insert the chosen color. It is really messy because of applyscript"
  (interactive)
  (let ((result
         (do-applescript "tell application \"Finder\"
  activate
set result to \"\"
set x to (choose color)
set result to item 1 of x as string
set result to result & \",\"
set result to result & item 2 of x as string
set result to result & \",\"
set result to result & item 3 of x as string
return result
end tell")))
    (if callback ; For Easy Customization
        (funcall callback (nscolor2hex result))
      (insert (nscolor2hex result)))
    (do-applescript "tell application \"Emacs\" to activate")))

(defalias 'list-colors-display 'color-picker)
(defalias 'widget-color--choose-action 'custom-color--choose-action)

;; Start server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(frame-resize-pixelwise t)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-mode-show-parse-errors t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(neo-theme (quote ascii))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" "node_modules" ".ensime_cache")))
 '(ruby-insert-encoding-magic-comment nil)
 '(web-mode-enable-auto-pairing nil)
 '(web-mode-markup-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "selectedKnobColor"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "highlightColor"))))
 '(company-scrollbar-fg ((t (:background "controlDarkShadowColor"))))
 '(company-tooltip ((t (:background "Black" :foreground "highlightColor"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "keyboardFocusIndicatorColor" :weight bold))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "alternateSelectedControlColor"))))
 '(company-tooltip-selection ((t (:inherit company-tooltip))))
 '(guide-key/key-face ((t (:foreground "#6a6765"))))
 '(guide-key/prefix-command-face ((t (:foreground "#139842"))))
 '(hl-line ((t nil)))
 '(hs-face ((t (:foreground "keyboardFocusIndicatorColor"))))
 '(markdown-header-face ((t (:inherit default :foreground "#3869c9"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 300))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 220))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 140))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 60))))
 '(neo-dir-link-face ((t (:foreground "#3869c9"))))
 '(neo-expand-btn-face ((t (:foreground "#3869c9"))))
 '(neo-file-link-face ((t (:foreground "#7b7b7b")))))
