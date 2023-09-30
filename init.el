;; [ Emacs Config ]


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(org-babel-load-file (expand-file-name "~/.emacs.d/myInit.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(blink-matching-paren t)
 '(centered-window-mode t)
 '(custom-enabled-themes '(firebelly))
 '(custom-safe-themes '(default))
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
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(rust-mode vterm magit simple-modeline swiper pyvenv bongo pdf-tools dired-subtree mood-line yasnippet vertico projectile org-bullets multiple-cursors elfeed dashboard company all-the-icons-dired all-the-icons-completion))
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
 '(fringe ((t (:background "#101010"))))
 '(tab-bar-tab ((t (:background "#151515" :foreground "#bbbbbb"))))
 '(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :weight bold))))
 '(tab-bar-tab-inactive ((t (:background "#303030" :foreground "gray55" :height 0.8))))
 '(vertico-current ((t (:inherit highlight :extend t :background "#303030" :foreground "SeaGreen2" :weight semi-bold))))
 '(yafolding-ellipsis-face ((t nil)) t))


