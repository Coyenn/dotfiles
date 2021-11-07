;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;             __                  __
;            ( _)                ( _)
;           / / \\              / /\_\_
;          / /   \\            / / | \ \
;         / /     \\          / /  |\ \ \
;        /  /   ,  \ ,       / /   /|  \ \
;       /  /    |\_ /|      / /   / \   \_\
;      /  /  |\/ _ '_| \   / /   /   \    \\
;     |  /   |/  0 \0\    / |    |    \    \\
;     |    |\|      \_\_ /  /    |     \    \\
;     |  | |/    \.\ o\o)  /      \     |    \\
;     \    |     /\\`v-v  /        |    |     \\
;      | \/    /_| \\_|  /         |    | \    \\
;      | |    /__/_ `-` /   _____  |    |  \    \\
;      \|    [__]  \_/  |_________  \   |   \    ()
;       /    [___] (    \         \  |\ |   |   //
;      |    [___]                  |\| \|   /  |/
;     /|    [____]                  \  |/\ / / ||
;    (  \   [____ /     ) _\      \  \    \| | ||
;     \  \  [_____|    / /     __/    \   / / //
;     |   \ [_____/   / /        \    |   \/ //
;     |   /  '----|   /=\____   _/    |   / //
;  __ /  /        |  /   ___/  _/\    \  | ||
; (/-(/-\)       /   \  (/\/\)/  |    /  | /
;               (/\/\)           /   /   //
;                      _________/   /    /
;                     \____________/    (

;----------------------------------------------------------------
;----------------------------------------------------------------
;
; My Doom Emacs config.
;
; To reload the config, just restart or M-x doom/reload.
; Update packages according to the config using doom sync.
;
;----------------------------------------------------------------
;----------------------------------------------------------------

;; theme
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; relative | nil
(setq display-line-numbers-type t)

;; Set my name and email
(setq user-full-name "Tim Ritter"
      user-mail-address "t-ritter-mail@web.de")

;; enable auto save by default
(setq auto-save-default t)

;; disable exit confirmation
(setq confirm-kill-emacs nil)

;; disable kill processes confirmation
(setq confirm-kill-processes nil)

;; maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set Font
(setq doom-font (font-spec :family "Fira Code" :size 15 :slant 'normal :weight 'normal))

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Add keybinding for treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "o e" #'treemacs)

;; SX Search
(map! :leader
      :desc "StackOverflow search"
      "o s" #'sx-search)

;; LSP
(map! :leader
      :desc "Goto Definition"
      "o D" #'evil-goto-definition)

(map!
 (:prefix "g"
   (:prefix "s"
     :nv "l" #'avy-goto-word-0)))
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
