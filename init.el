;; Path
(push "/usr/local/bin" exec-path)

;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      editorconfig
                      clojure-mode
                      coffee-mode
                      web-mode
                      js2-mode
                      scala-mode2
                      lua-mode
                      yaml-mode
                      markdown-mode
                      scss-mode
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
                      ;; smartparens
                      paredit
                      anzu
                      ace-jump-mode
                      hideshowvis
                      magit
                      git-timemachine
                      git-gutter+
                      jazz-theme
                      twilight-theme
                      afternoon-theme
                      cyberpunk-theme
                      distinguished-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Common
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(global-linum-mode 1)
(global-hl-line-mode 1)
(recentf-mode 1)

;; Frame
(defconst title "新しい自分に生まれ変わったら きっと、もっと美しい明日がやってくる")
(set-frame-name title)
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (select-frame frame)
                                        (set-frame-name title)))
(setq default-frame-alist '((top . 50) (left . 100)
                            (width . 130) (height . 35)))

;; Font
(set-face-attribute 'default nil :font "Monaco-14")

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'dichromacy t)
;; (load-theme 'tomorrow-day t)
;; (load-theme 'afternoon t)
;; (load-theme 'jazz t)
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

;; Git Gutter +
(global-git-gutter+-mode 1)

;; Global Key Binding
(define-key evil-normal-state-map "\C-e" 'move-end-of-line)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Companym Mode
(global-company-mode 1)

;; Anzu
(global-anzu-mode 1)
(defun my-anzu-update-func (here total)
  (propertize (format "<%d/%d> " here total)
              'face 'font-lock-type-face))
(setq anzu-mode-line-update-function 'my-anzu-update-func)

;; Flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; Hideshowvis
(add-hook 'clojure-mode-hook 'hideshowvis-enable)
(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)
(add-hook 'ruby-mode-hook 'hideshowvis-enable)
(add-hook 'python-mode-hook 'hideshowvis-enable)
(add-hook 'lua-mode-hook 'hideshowvis-enable)
(add-hook 'web-mode-hook 'hideshowvis-enable)
(add-hook 'js2-mode-hook 'hideshowvis-enable)
(add-hook 'coffee-mode-hook 'hideshowvis-enable)
(add-hook 'scala-mode-hook 'hideshowvis-enable)
(hideshowvis-symbols)
(custom-set-faces '(hs-face ((t (:foreground "keyboardFocusIndicatorColor")))))
(define-key evil-normal-state-map "zA" 'hs-hide-all)
(define-key evil-normal-state-map "zB" 'hs-show-all)

;; Highlight Chars
;; (require 'highlight-chars)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; Ace Jump Mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; Projectile
(projectile-global-mode)
(define-key evil-normal-state-map "\C-p" 'projectile-find-file)
(define-key evil-normal-state-map "\C-b" 'projectile-switch-to-buffer)

;; Guide Key
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

;; Smartparens
;; (require 'smartparens-config)
;; (smartparens-global-mode t)
;; (show-smartparens-global-mode t)
;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; Paredit
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'ruby-mode-hook 'enable-paredit-mode)
(add-hook 'js2-mode-hook 'enable-paredit-mode)
(add-hook 'coffee-mode-hook 'enable-paredit-mode)
(add-hook 'web-mode-hook 'enable-paredit-mode)
(add-hook 'scala-mode-hook 'enable-paredit-mode)

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Js2 Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t))

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
(custom-set-variables '(neo-theme (quote ascii)))
(evil-leader/set-key "l" 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)))

;; Start server
(server-start)
