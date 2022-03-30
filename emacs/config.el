;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;----------------------------------------------------------------
;----------------------------------------------------------------
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

;----------------------------------------------------------------
; PERSONAL
;----------------------------------------------------------------

;; Set my name and email
(setq user-full-name "Tim Ritter"
      user-mail-address "t-ritter-mail@web.de")

;----------------------------------------------------------------
; BEHAVIOUR
;----------------------------------------------------------------

;; Org stores its files in this directory
(setq org-directory "~/.org/")

;; Enable auto save by default
(setq auto-save-default t)

;; Disable various kill confirmations
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Remove *scratch* from buffer after the mode has been set
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Remove *messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Remove *Completions* buffer
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;----------------------------------------------------------------
; VISUAL
;----------------------------------------------------------------

;; Theme
(setq doom-theme 'doom-dark+)

;; Font
(setq doom-font (font-spec :family "Fira Code" :size 16 :slant 'normal :weight 'normal))

;----------------------------------------------------------------
; KEYBINDINGS
;----------------------------------------------------------------

;; Add keybinding for treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "o e" #'treemacs)

;; SX Search
(map! :leader
      :desc "StackOverflow search"
      "o s" #'sx-search)

;; Goto definition
(map! :leader
      :desc "Goto Definition"
      "o D" #'evil-goto-definition)

;; jj to get back into normal mode
(setq-default evil-escape-key-sequence "jj")

;----------------------------------------------------------------
; PACKAGES
;----------------------------------------------------------------

;; TailwindCSS
(use-package! lsp-tailwindcss)
