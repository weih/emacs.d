(push "/usr/local/bin" exec-path)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
		      projectile
                      clojure-mode
                      cider
                      evil
                      company
                      neotree
		      flx-ido
		      ido-vertical-mode
                      ido-ubiquitous
                      ag
                      smex
		      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)

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
    '(:evalk (when (buffer-modified-p)
              (propertize "[Mod] "
                          'face 'font-lock-warning-face
                          'help-echo "Buffer has been modified")))

    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; '(:eval minor-mode-alist)
    ))

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-linum-mode 1)

(projectile-global-mode)

(evil-mode 1)

(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
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

(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(ido-ubiquitous-mode 1)

(set-face-attribute 'default nil :font "Monaco-14")

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'dichromacy t)
