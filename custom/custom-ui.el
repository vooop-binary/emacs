;;; custom-ui.el -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;;
;;; Font Setup

(defvar me/font "IBM Plex Mono")
(defvar me/font-size 140)
(defvar me/font-variable-size 160)

(set-face-attribute 'default nil :font me/font :height me/font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font me/font :height me/font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "IBM Plex Sans" :height me/font-variable-size :weight 'regular)

(provide 'custom-ui)
