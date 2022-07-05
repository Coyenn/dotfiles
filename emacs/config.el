;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
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
; To reload the config, just restart or M-x doom/restart.
; Update packages according to the config using doom sync
; or M-x doom/reload and then M-x doom/restart.
;
;----------------------------------------------------------------
;----------------------------------------------------------------

;----------------------------------------------------------------
; PERSONAL
;----------------------------------------------------------------

; Set my name and email
(setq user-full-name "Tim Ritter"
      user-mail-address "t-ritter-mail@web.de")

;----------------------------------------------------------------
; BEHAVIOUR
;----------------------------------------------------------------

; Much of this is taken from:
; https://tecosaur.github.io/emacs-config/config.html#rudimentary-configuration

(setq-default
 delete-by-moving-to-trash t    ; Delete files to trash
 window-combination-resize t    ; take new window space from all other windows (not just current)
 x-stretch-cursor t)            ; Stretch cursor to the glyph width

(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t               ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"      ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil         ; I can trust my computers ... can't I?
      scroll-margin 2)                  ; It's nice to maintain a little margin

; Org stores its files in this directory
(setq org-directory "~/.org/")

; Disable various kill confirmations
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

; No delay for displaying auto completions
(setq company-idle-delay 0)

; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

; Enable time in the mode-line
(display-time-mode 1)

; Iterate through CamelCase words
(global-subword-mode 1)

;----------------------------------------------------------------
; VISUAL
;----------------------------------------------------------------

; Theme
(setq doom-theme 'doom-tokyo-night)

; Font
(setq doom-font (font-spec :family "Fira Code" :size 16 :slant 'normal :weight 'normal))

; Zen Mode text scale
(setq +zen-text-scale 1)

; Centaur Tabs
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 40
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer))

;----------------------------------------------------------------
; KEYBINDINGS
;----------------------------------------------------------------

; Add keybinding for treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "o e" #'treemacs)

; SX Search
(map! :leader
      :desc "StackOverflow search"
      "o s" #'sx-search)

; Goto definition
(map! :leader
      :desc "Goto Definition"
      "o D" #'evil-goto-definition)

; Scroll half a page down
(map! "C-j" #'evil-scroll-down)

; Scroll half a page up
(map! "C-k" #'evil-scroll-up)

; jj to get back into normal mode
(setq-default evil-escape-key-sequence "jj")

;----------------------------------------------------------------
; PACKAGES
;----------------------------------------------------------------

; TailwindCSS
(use-package! lsp-tailwindcss)

; Tree Sitter for better syntax highlighting
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

; Ctrl-F to search
(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode))
