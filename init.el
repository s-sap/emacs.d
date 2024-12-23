;; [ Emacs Config ]

; list the repositories containing them
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(require 'package)
(setq package-enable-at-startup nil)

; list the packages you want
(setq package-list '(magit vertico
		      multiple-cursors dashboard
		      elfeed all-the-icons
		      all-the-icons-completion eglot company
		      dired-hacks-utils))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(org-babel-load-file (expand-file-name "~/.emacs.d/myInit.org"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-alltheicon-scale-factor 1.0)
 '(all-the-icons-completion-mode t)
 '(auth-source-save-behavior nil)
 '(blink-matching-paren t)
 '(centered-window-mode t)
 '(custom-enabled-themes '(firebelly))
 '(custom-safe-themes
   '("7d1c7ea4f3e73402f012b7011fc4be389597922fa67ad4ec417816971bca6f9d" "ecc533c16453beccb38074f331acb7101006aa6d996379d4af0a76d9a90fe526" default))
 '(cwm-centered-window-width 150)
 '(dashboard-banner-logo-title nil)
 '(dashboard-bookmarks-item-format "%s")
 '(dashboard-center-content t)
 '(dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
 '(dashboard-footer "Who the hell uses VIM anyway? Go Evil!")
 '(dashboard-icon-type nil)
 '(dashboard-set-file-icons nil)
 '(dashboard-set-footer nil)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-navigator t)
 '(dashboard-show-shortcuts nil)
 '(dired-sidebar-theme 'icons)
 '(doom-modeline-icon nil)
 '(doom-modeline-mode t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(rust-mode vterm magit swiper pyvenv bongo pdf-tools mood-line yasnippet vertico projectile org-bullets multiple-cursors elfeed dashboard company all-the-icons-completion))
 '(pdf-view-midnight-colors '("gray60" . "#101010") t)
 '(pdf-view-midnight-invert nil t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-mode t)
 '(tab-line-new-button-show nil)
 '(vertico-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-navigator ((t (:foreground "grey 30"))))
 '(elfeed-search-title-face ((t (:foreground "gray25" :underline "gray30" :height 1.3))))
 '(elfeed-search-unread-title-face ((t (:foreground "gray50" :weight bold))))
 '(flymake-warning ((t nil)))
 '(font-lock-comment-face ((t (:foreground "gray45" :slant italic :weight bold))))
 '(fringe ((t (:background "#101010"))))
 '(highlight-indent-guides-character-face ((t (:background "orange red" :foreground "red"))))
 '(tab-bar-tab ((t (:background "#151515" :foreground "#bbbbbb"))))
 '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "#303030" :foreground "gray55" :height 0.8))))
 '(vertico-current ((t (:inherit highlight :extend t :background "#303030" :foreground "SeaGreen2" :weight semi-bold))))
 '(yafolding-ellipsis-face ((t nil)) t))

