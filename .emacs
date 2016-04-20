;; *PACKAGE INITIALIZATION

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-list  '(ac-cider
                      auto-complete
                      cider
                      clojure-cheatsheet
                      clojure-mode
                      color-theme-sanityinc-tomorrow
                      company
                      dash
                      epl
                      git-commit
                      js2-mode
                      magit
                      magit-popup
                      multiple-cursors
                      paredit
                      pkg-info
                      popup
                      queue
                      seq
                      spinner
                      with-editor))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; *GLOBAL MODES

(global-auto-complete-mode 1)
(electric-pair-mode 1)

;; *VARIABLES

(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice "~/")
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(line-number-mode nil)
 '(tab-width 4))

;; *UI CONFIG

(tool-bar-mode -1) ; Hide toolbar
(menu-bar-mode -1) ; No need for menu either 
(scroll-bar-mode -1) ; Nor pesky scrollbars
(load-theme 'sanityinc-tomorrow-night)

;; *COMMANDS

(defun horizontal-align-center ()
  "Roughly horizontally centers the text in the current buffer."
  (interactive)
  (set-window-fringes (selected-window) 350 0))

(defun horizontal-align-left ()
  "Horizontally aligns the text in the current buffer to left position."
  (interactive)
  (set-window-fringes (selected-window) 0 0 ))

(defun fringe-set-color-bg ()
  "Sets the color of the fringes to that of the buffer background."
  (interactive)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(defun fringe-set-color-default ()
  "Sets the color of the fringes to their default."
  (interactive)
  (set-face-attribute 'fringe nil
                      :foreground "#c5c8c6"
                      :background "#373b41"))

;; Credit to Steve Yegge
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn   (rename-file name new-name 1)   (rename-buffer new-name)    (set-visited-file-name new-name)    (set-buffer-modified-p nil))))))

;; Credit to Tikhon Jelvis
(defun quit ()
  "Prompts to save unsaved buffers and then kills the emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Credit to Tikhon Jelvis
(defun todo-comment ()
  "Inserts an empty TODO comment or makes an existing comment
into a TODO."
  (interactive)
  (when (not (region-active-p))
    (comment-dwim nil)
    (unless (equal (current-word) "TODO") (insert "TODO: "))))
(global-set-key (kbd "C-c t") 'todo-comment)

(defun hack-comment ()
  "Inserts an empty HACK comment or makes an existing comment
into a HACK."
  (interactive)
  (when (not (region-active-p))
    (comment-dwim nil)
    (unless (equal (current-word) "HACK") (insert "HACK: "))))
(global-set-key (kbd "C-c h") 'hack-comment)

(defun note-comment ()
  "Inserts an empty NOTE comment or makes an existing comment
into a NOTE."
  (interactive)
  (when (not (region-active-p))
    (comment-dwim nil)
    (unless (equal (current-word) "NOTE") (insert "NOTE: "))))
(global-set-key (kbd "C-c n") 'note-comment)

;; Window manager
;; Credit to Marius Eriksen 
;; emacsd-tile.el -- tiling windows for emacs
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(global-set-key (kbd "C-M-J") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-K") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)

(provide 'emacsd-tile)

;; *LANG SPECIFIC CONFIG

;; CSS
(setq css-indent-offset 2)

;; Clojure
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'cider-mode-hook #'eldoc-mode)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))

;; *MAC stuff

(setq mac-command-modifier 'meta)

;; *AUTOSAVE/BACKUP CONFIG

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(custom-set-faces)

