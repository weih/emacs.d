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
		      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(fset 'yes-or-no-p 'y-or-n-p)

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

(set-face-attribute 'default nil :font "Monaco-14")

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'dichromacy t)
