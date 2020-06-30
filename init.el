(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(defun require-package (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       (unless (package-installed-p package)
	 (package-install package)))
     packages)
    )


(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
              '(progn ,@body))))
(require-package 'use-package)
(require 'use-package)


(when (display-graphic-p)
  ;; Setting English Font
  (set-default-font "Monaco-12")
  (set-frame-font "Monaco-12")
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font)
					  charset			  
					  (font-spec :family "Hiragino Sans GB" :size 13)))
  (setq default-frame-alist '((height . 40) (width . 120)))
  )

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(setq default-directory "~")
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)
(setq backup-directory-alist (quote (("." . "~/.emacs-backups"))))
(require 'whitespace)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-echo-area-message t)
(setq-default cursor-type 'box)
(setq display-time-24hr-format t)
(setq ring-bell-function (lambda () (message "BEEP BEEP !!!")))
(toggle-indicate-empty-lines)
(electric-pair-mode)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))


(require 'dired-x)
(setq dired-omit-files "^\\.?#\\|^\\..*")
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
		(dired-omit-mode)))

(defun shell (name)
  "默认只能打开一个eshell，调用这里的neweshell可以打开多个eshell
这里会先询问你新的eshell的标题名，即打开一个新的eshell，命名为不同的名称"
  (interactive "sShell's name: ")
  (with-selected-window
   (selected-window)
   (if (> (string-width name) 0)
	   (if (get-buffer name)
		   (pop-to-buffer name)
		 (progn
		   (eshell "new")
		   (rename-buffer name)))
	 (eshell))))

(defun show-file-path ()
  (interactive)
  (if (buffer-file-name)
      (message (buffer-file-name))
    (message  (buffer-name))
    )
  )

(defun yam/open-and-select-buffer-list ()
  "打开 buffer list 并选中"
  (interactive)
  (push-mark (save-excursion
			   (list-buffers)
			   (pop-to-buffer "*Buffer List*")
			   ;;(isearch-mode t nil nil t)
			   (isearch-forward)
			   )))
(global-set-key (kbd "C-x C-b") 'yam/open-and-select-buffer-list)

(global-set-key (kbd "C-A") 'yam/auto-beginning-of-line)
(defun yam/auto-beginning-of-line ()
  "跳转到当前行的最开始或者第一个非空字符处"
  (interactive)
  (let ((crt-point (point))
        (line-begin-point (progn (beginning-of-line) (point)))
        (text-begin-point (progn (beginning-of-line-text) (point))))
    (if (equal crt-point text-begin-point)
        (goto-char line-begin-point))))

(defun yam/kill-buffer-head-tail-blank-lines ()
  "去除当前buffer的首尾空行"
  (interactive)
  (push-mark
   (save-excursion
     (goto-char (point-min)) (delete-blank-lines)
     (goto-char (point-max)) (delete-blank-lines)
     (point)) t))

(defun yam/indent-buffer ()
  "缩进当前buffer的全部内容"
  (interactive)
  (push-mark
   (save-excursion
     (indent-region (point-min) (point-max))
     (kill-buffer-head-tail-blanklines)
     (point)) t)
  (message "The whole buffer was indented"))

;; copy from
;; https://emacs.stackexchange.com/a/320
(defun yam/switch-window-split-mode ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun yam/buffer-name-without-extension ()
  (substring (buffer-name) 0
             (- (length (buffer-name))
				(length (file-name-extension (buffer-name)))
				1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
	(all-the-icons-dired all-the-icons all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich neotree yasnippet exec-path-from-shell go-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; installed 3rd package
;; exec-path-from-shell
;; go-mode
;; yasnippet
;; all-the-icons*
;; neotree

(setq yas-snippet-dirs '("~/.emacs.d/yas-snippets/"))
(require 'yasnippet)
(yas-global-mode 1)


(require-package 'all-the-icons)
(require 'all-the-icons)
(require-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


(setq inhibit-compacting-font-caches t)
;; M-x all-the-icons-install-fonts

;; https://github.com/jaypei/emacs-neotree
;; n next line, p previous line。
;; SPC or RET or TAB Open current item if it is a file. Fold/Unfold current item if it is a directory.
;; U Go up a directory
;; g Refresh
;; A Maximize/Minimize the NeoTree Window
;; H Toggle display hidden files
;; O Recursively open a directory
;; C-c C-n Create a file or create a directory if filename ends with a ‘/’
;; C-c C-d Delete a file or a directory.
;; C-c C-r Rename a file or a directory.
;; C-c C-c Change the root directory.
;; C-c C-p Copy a file or a directory.
(require-package 'neotree)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'arrow))
;; Every time when the neotree window is opened, let it find current file and jump to node.
(setq neo-smart-open t)
(setq neo-window-fixed-size nil)
;; Set the neo-window-width to the current width of the
  ;; neotree window, to trick neotree into resetting the
  ;; width back to the actual window width.
  ;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
  (eval-after-load "neotree"
    '(add-to-list 'window-size-change-functions
                  (lambda (frame)
                    (let ((neo-window (neo-global--get-window)))
                      (unless (null neo-window)
                        (setq neo-window-width (window-width neo-window)))))))

;;(global-set-key [f8] 'neotree-toggle)


