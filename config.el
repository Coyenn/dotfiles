;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; theme
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; relative | nil
(setq display-line-numbers-type t)

;; Set my name and email
(setq user-full-name "Tim Ritter"
      user-mail-address "t-ritter-mail@web.de")

;; enable auto save by default
(setq auto-save-default t)

;; disable exit confirmation
(setq confirm-kill-emacs nil)

;; maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set Font
(setq doom-font (font-spec :family "Fira Code" :size 15 :slant 'normal :weight 'normal))

;; open minimap
(minimap-mode)

;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Open Treemacs on startup
(treemacs)

;; switch to dashboard instead of staying in neotree buffer
(switch-to-buffer-other-window "*scratch*")

;; Treemacs shall follow me around!
(treemacs-follow-mode)
