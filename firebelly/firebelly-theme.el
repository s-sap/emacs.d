;;  [ Customize ] firebelly-theme.el
;; Original Author: startling
;; URL: https://github.com/startling/firebelly
;; Version: 1.0
;; Package-Requires: ((cl-lib "0.5"))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
               (file-name-directory load-file-name))))

(require 'cl-lib)

(deftheme firebelly)
(provide-theme 'firebelly)
(let
  ;; This beautiful palette is shamelessly stolen from chris
    ;; kempson's base16: https://github.com/chriskempson/base16
    ;; 90a959
  ((palette
    '("#c25153" ; RED aa4c4d
      "#d28445"  ;ORANGE
      "#f4bf75" ; YELLOW
      "#6798DB" ; GREEN 
      "#75b5aa" ; LIGHT BLUE
      "#699159" ; 
      "#aa759f" ; PURPLE
      "#8f5536" ; LIGHT RED
      "#d9d9d9"))
   ;; Simple grayscale palette.
   (greys
    '("#101010" "#292929" "#954141" "#555555"
      "#666666" "#6f6f6f" "#969696" "#999999" "#606060")))

  ;; Tiny functions to make getting from the palettes easier.
  (cl-flet
      ((color (n) (nth n palette))
       (grey (n) (nth n greys)))

    ;; Define some faces for our theme.
    (custom-theme-set-faces
     'firebelly
    ;;;; Styling emacs.
     `(default
       ((t (:background ,(grey 0) :foreground ,(grey 7)))))
     `(cursor
       ((t (:background , "#ffffff"))))
     `(highlight
       ((t (:background ,(grey 0)))))
     `(shadow
       ((t (:foreground ,(grey 6)))))
     `(isearch
       ((t (:background ,(grey 2) :foreground ,(color 6)))))
     `(query-replace
       ((t (:background ,(grey 2) :foreground ,(color 6)))))
     `(lazy-highlight
       ((t (:background ,(grey 1) :foreground ,(grey 3)))))
     `(minibuffer-prompt
       ((t (:foreground ,(color 1)))))
     `(trailing-whitespace
       ((t (:background ,(grey 1)))))
     `(nobreak-space
       ((t (:background ,(grey 1)))))
     `(escape-glyph
       ((t (:foreground ,(color 2)))))
     ;; Fringes are ugly.
     `(fringe
       ((t (:background ,(grey 0)))))
     ;; Highlight the border.
     `(vertical-border
       ((t (:foreground ,(grey 1)))))
     ;; Mode lines look the same but for the text.
     `(mode-line
       ((t (:background ,(grey 0) :foreground ,(grey 6) :box nil))))
     `(mode-line-inactive
       ((t (:background ,(grey 1) :foreground ,(grey 3) :box nil))))
     `(header-line
       ((t (:background ,(grey 1) :foreground ,(grey 3) :box nil))))
     `(mode-line-buffer-id
       ((t (:bold t))))
     `(mode-line-highlight
       ((t (:foreground ,(grey 7)))))
     ;; Regions are slightly lighter.
     `(region
       ((t (:background ,(grey 3)))))
     `(secondary-selection
       ((t (:background ,(grey 2)))))

    ;;;; Styling code.
     ;; Comments are lighter than their delimiters.
     `(font-lock-comment-face
       ((t (:foreground ,(grey 5) :slant italic))))
     `(font-lock-comment-delimiter-face
       ((t (:foreground ,(grey 1)))))
     ;; Docstrings are slightly lighter.
     `(font-lock-doc-face
       ((t (:foreground ,(grey 4) :background ,(grey 0)))))
     `(font-lock-function-name-face
       ((t (:foreground ,(color 3)))))
     `(font-lock-variable-name-face
       ((t (:foreground , (color 0)))))
     `(font-lock-builtin-face
       ((t (:foreground ,(color 6)))))
     `(font-lock-constant-face
       ((t (:foreground ,(color 1)))))
     `(font-lock-type-face
       ((t (:foreground ,(color 4)))))
     `(font-lock-string-face
       ((t (:foreground ,(color 5) :background ,(grey 1) :slant italic))))
     `(font-lock-keyword-face
       ((t (:foreground ,(color 6)))))

    ;;;; Styling extensions.
     ;; Erc faces.
     `(erc-notice-face
       ((t (:foreground ,(grey 2)))))
     `(erc-current-nick-face
       ((t (:foreground ,(color 3) :bold t))))
     `(erc-prompt-face
       ((t (:foreground ,(color 1) :background ,(grey 0)))))
     `(erc-input-face
       ((t (:foreground ,(grey 6)))))
     `(erc-my-nick-face
       ((t (:foreground ,(color 1)))))
     `(erc-error-face
       ((t (:foreground ,(color 0)))))
     `(erc-timestamp-face
       ((t (:foreground ,(color 3)))))
     ;; Rainbow delimiters are mostly in order.
     `(rainbow-delimiters-depth-1-face
       ((t (:foreground ,(color 7)))))
     `(rainbow-delimiters-depth-2-face
       ((t (:foreground ,(color 6)))))
     `(rainbow-delimiters-depth-3-face
       ((t (:foreground ,(color 5)))))
     `(rainbow-delimiters-depth-4-face
       ((t (:foreground ,(grey 3)))))
     `(rainbow-delimiters-depth-5-face
       ((t (:foreground ,(color 4)))))
     `(rainbow-delimiters-depth-6-face
       ((t (:foreground ,(color 3)))))
     `(rainbow-delimiters-depth-7-face
       ((t (:foreground ,(color 2)))))
     `(rainbow-delimiters-depth-8-face
       ((t (:foreground ,(color 1)))))
     `(rainbow-delimiters-depth-9-face
       ((t (:foreground ,(color 0)))))
     ;; shm faces
     `(shm-current-face
       ((t (:background ,(grey 1)))))
     `(shm-quarantine-face
       ((t (:foreground ,(color 0) :background ,(grey 1)))))
     ;; linum-relative faces
     `(linum-relative-current-face
       ((t (:background ,(grey 0) :foreground ,(color 1)))))
     ; Helm
     `(helm-selection
       ((t (:foreground, "light grey" :background ,"#303030" :bold t))))

   ;; tab-bar-mode
     `(tab-bar-tab-inactive
       ((t (:background, "#000000" :foreground, "#6e6e6e"))))
     `(tab-bar-tab
       ((t (:background, "#404040" :foreground, "#b8b8b8"))))
     `(tab-bar
       ((t (:background, "#151515" :height 1.2))))

     ; Company
     '(company-echo-common ((t (:foreground "SpringGreen3"))))
     '(company-preview-common ((t (:inherit company-preview :foreground "SpringGreen3"))))
     '(company-scrollbar-fg ((t (:background "#202020"))))
     '(company-template-field ((t (:background "gray20"))))
     '(company-tooltip ((t (:background "#151515" :foreground "gray50"))))
     '(company-tooltip-annotation ((t (:foreground "gray60"))))
     '(company-tooltip-common ((t (:foreground "white smoke"))))
     '(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
     '(company-tooltip-selection ((t (:background "#404040"))))
     
     ; Term
     '(term-color-black ((t (:foreground "#3F3F3F" :background "#2B2B2B"))))
     '(term-color-blue ((t (:foreground "#7da6ff" :background "#4C7073"))))
     '(term-color-cyan ((t (:foreground "#93E0E3" :background "#8CD0D3"))))
     '(term-color-green ((t (:foreground "#7F9F7F" :background "#9FC59F"))))
     '(term-color-magenta ((t (:foreground "#DC8CC3" :background "#CC9393"))))
     '(term-color-red ((t (:foreground "#AC7373" :background "#8C5353"))))
     '(term-color-white ((t (:foreground "#DCDCCC" :background "#656555"))))
     '(term-color-yellow ((t (:foreground "#DFAF8F" :background "#9FC59F"))))
     '(term-default-bg-color ((t (:inherit term-color-black))))
     '(term-default-fg-color ((t (:inherit term-color-white))))

     ; Org
      '(org-block ((t (:inherit (fixed-pitch shadow) :extend t :background "gray13"))))
      '(org-ellipsis ((t (:foreground "white smoke"))))

      ; Elfeed
       '(elfeed-search-feed-face ((t (:foreground "dark salmon"))))
       '(elfeed-search-tag-face ((t (:foreground "gray" :height 0.8))))
       '(elfeed-search-title-face ((t (:foreground "gray55" :underline "gray25" :height 1.3))))
       '(highlight ((t (:background "gray40"))))
       '(hl-line ((t (:inherit highlight :extend t :background "gray25" :foreground "medium sea green"))))

       
     )))

