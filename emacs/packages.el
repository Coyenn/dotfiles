;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; StackOverflow search
(package! sx)

;;LSP for tailwind
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; Tree Sitter
(package! tree-sitter)
(package! tree-sitter-langs)

;; Ctrl-F to search
(package! ctrlf)
