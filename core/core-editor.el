;;; core-editor.el -*- lexical-binding: t; -*-

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

(dolist (mode '(prog-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq display-line-numbers-type 'relative)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
          (lambda ()
	        "Automatically create missing directories when creating new files."
	        (unless (file-remote-p buffer-file-name)
	          (let ((parent-directory (file-name-directory buffer-file-name)))
		        (and (not (file-directory-p parent-directory))
		             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
				                       parent-directory))
		             (progn (make-directory parent-directory 'parents)
			                t))))))

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat emacs-cache-dir "backup/")))
      
      tramp-backup-directory-alist backup-directory-alist)

;; Delete trailing whitespace after saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat CamelCaseSubWords as separate words in every programming mode.
(add-hook 'prog-mode-hook 'subword-mode)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Apply changes to highlighted regions
(transient-mark-mode t)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat emacs-cache-dir "autosave/")
      tramp-auto-save-directory  (concat emacs-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(add-hook 'after-save-hook
	      (lambda ()
	        "Guess major mode when saving a file in `fundamental-mode'.
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
	        (when (eq major-mode 'fundamental-mode)
	          (let ((buffer (or (buffer-base-buffer) (current-buffer))))
		        (and (buffer-file-name buffer)
		             (eq buffer (window-buffer (selected-window))) ; only visible buffers
		             (set-auto-mode))))))

;;
;;; Formatting

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)

(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; An archaic default in the age of widescreen 4k displays? I disagree. We still
;; frequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;;
;;; Extra file extensions to support

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;
;;; Built in package configuration
(use-package recentf
  :straight nil
  :init (recentf-mode)
  :commands recentf-open-files
  :custom (recentf-save-file (concat emacs-cache-dir "recentf"))
  :config
  (setq recentf-auto-cleanup nil     ; Don't.
	    recentf-max-saved-items 200) ; default is 20

  (defun recentf-file-truename-fn (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
	    (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
	  file))

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'recentf-file-truename-fn)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-hook 'write-file-functions
	        (lambda ()
		      "Bump file in recent file list when it is switched or written to."
		      (when buffer-file-name
		        (recentf-add-file buffer-file-name))
		      ;; Return nil for `write-file-functions'
		      nil))

  (add-hook 'dired-mode-hook
	        (lambda ()
		      "Add dired directories to recentf file list."
		      (recentf-add-file default-directory)))

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package savehist
  ;; persist variables across sessions
  :straight nil
  :init (savehist-mode)
  :custom (savehist-file (concat emacs-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
	    savehist-autosave-interval nil     ; save on kill only
	    savehist-additional-variables
	    '(kill-ring                        ; persist clipboard
	      register-alist                   ; persist macros
	      mark-ring global-mark-ring       ; persist marks
	      search-ring regexp-search-ring)) ; persist searches
  (add-hook 'savehist-save-hook
	        (lambda ()
	          "Remove text properties from `kill-ring' to reduce savehist cache size."
	          (setq kill-ring
		            (mapcar #'substring-no-properties
			                (cl-remove-if-not #'stringp kill-ring))
		            register-alist
		            (cl-loop for (reg . item) in register-alist
			                 if (stringp item)
			                 collect (cons reg (substring-no-properties item))
			                 else collect (cons reg item)))))
  (add-hook 'savehist-save-hook
	        (lambda ()
              "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
              ;; Save new value in the temp buffer savehist is running
              ;; `savehist-save-hook' in. We don't want to actually remove the
              ;; unserializable registers in the current session!
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist)))))


(provide 'core-editor)
