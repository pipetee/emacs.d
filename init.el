(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)


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

(require 'dired-x)
(setq dired-omit-files "^\\.?#\\|^\\..*")
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
		(dired-omit-mode)))


(defun new-shell (name)
  "默认只能打开一个shell，调用这里的newshell可以打开多个shell
这里会先询问你新的shell的标题名，即打开一个新的shell，命名为不同的名称"
  (interactive "sBuffer name: ")
  (shell name))

(defun localshell ()
  "打开一个名为 shell 的 shell"
  (interactive)
  (shell "shell"))

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
 '(package-selected-packages (quote (yasnippet exec-path-from-shell go-mode)))
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

(setq yas-snippet-dirs '("~/.emacs.d/yas-snippets/"))
(require 'yasnippet)
(yas-global-mode 1)


