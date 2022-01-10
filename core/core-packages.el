;;; core/core-packages.el -*- lexical-binding: t; -*-

;; Emacs package management is opinionated. This configuration uses `straight'
;; to create a declarative, lazy-loaded and (nominally) reproducible package
;; management system. I use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.
;;
;; What's worse, you can only get the latest version of packages through ELPA.
;; In an ecosystem that is constantly changing, this is more frustrating than
;; convenient. Straight can do rolling release, but it is opt-in.

;;
;;; Straight

(setq straight-base-dir (file-truename emacs-local-dir)
      straight-repository-branch "develop"
      straight-build-dir (format "build-%s" emacs-version))

;; Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" emacs-local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)

(with-eval-after-load 'straight
  ;; `let-alist' is built into Emacs 26 and onwards
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

(provide 'core-packages)
