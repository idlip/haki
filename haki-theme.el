;;; haki-theme.el --- An elegant, high-contrast dark theme in modern sense -*- lexical-binding:t -*-

;; Copyright (C) 2023 Dilip

;; Title: Haki-theme
;; Author: Dilip
;; Maintainer: Dilip
;; URL: https://github.com/idlip/haki
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, accessibility

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Haki is an elegant, high-contrast dark theme in modern sense.
;; Looks and distinguish-ability is maintained.
;; I hope you will love it ;)
;;

;;; Code:


(unless (>= emacs-major-version 27)
  (error "Haki theme requires Emacs 27.1 or later!"))

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'color)

(deftheme haki "An Elegant, tailored theme for Modern Emacs.")

(defgroup haki-theme ()
  "Haki theme options for User's preference.
Make sure to reload the theme after setting the values!"
  :group 'faces)

;; I know docstring is more than 80 should I minimize it?
(defcustom haki-region "#2e8b6d"
  "Your color choice for haki theme region background.
Usually dark variant of any color is *recommended*,
as it syncs with orderless matching and text visibility.
It will be used in region selection, vertico-current and corfu-current too.

==> Tip : You can use `haki-change-region' function.

Default value is SeaGreen (#2e8b57)

If you dont like coloured one, some better choices are :
#2b2b2b  ==> Darker
#6c7b8b  ==> Greyish
#5f9ea0  ==> Cadet Blue

Usage : (setq haki-region <#hex-value>)

-- Requires reloading the theme to display changes --"
  :group 'haki-theme
  :type 'string)

;; I know docstring is more than 80 should I minimize it?
(defun haki-change-region (color)
  "Lists COLOR to choose and set as #`haki-region.
Copies chosen hex value `kill-ring.

Default value is SeaGreen (#2e8b57)."
  (interactive
   (list
    (if (fboundp 'consult--read)
        (consult--read (defined-colors)
                       :prompt "Choose Haki's Region: "
                       :require-match t
                       :category 'color)
      (completing-read "Choose Haki's Region: " (defined-colors) nil t ))))
  (let (( region-choice
          (when-let* ((rgb (color-name-to-rgb color))
                      ;; Sets 2 digits per component.
                      (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
            hex)))
    (setq haki-region region-choice)
    (load-theme 'haki t)
    (message (concat "Add this line to init.el to persist on sessions. Hex value is in your kill-ring too.
(setq haki-region " region-choice")  ;; value in double quotes"))
    (kill-new region-choice)))

;;; --- Variables to use different fonts
(defcustom haki-code-font `unspecified' ;; we can use it for both verbatim and code face
  "Font for inline code face.
Useful in 'markdown-mode' and 'org-mode'."
  :group 'haki-theme
  :type 'string)

(defcustom haki-heading-font `unspecified'
  "Font for heading levels.
Useful everywhere with headings."
  :group 'haki-theme
  :type 'string)

(defcustom haki-sans-font `unspecified'
  "Font to define a sans font.
Useful in context having person name (Author)."
  :group 'haki-theme
  :type 'string)

(defcustom haki-title-font `unspecified'
  "Font for Titles.
Useful in everywhere with top title, Should be unique and outstanding."
  :group 'haki-theme
  :type 'string)

(defcustom haki-link-font `unspecified'
  "Font for links.
Italicize the link with pretty-design (cursive) font.
Tip: Use 'VictorMono' or 'Maple Mono'."
  :group 'haki-theme
  :type 'string)

;;; - declare optional function
(declare-function meow-insert-mode-p "ext:meow")
(declare-function meow-beacon-mode-p "ext:meow")
(declare-function meow-normal-mode-p "ext:meow")
(declare-function meow-motion-mode-p "ext:meow")
(declare-function meow-keypad-mode-p "ext:meow")

(declare-function evil-insert-state-p "ext:evil")
(declare-function evil-normal-state-p "ext:evil")
(declare-function evil-visual-state-p "ext:evil")
(declare-function evil-motion-state-p "ext:evil")
(declare-function evil-operator-state-p "ext:evil")
(declare-function evil-emacs-state-p "ext:evil")
(declare-function evil-replace-state-p "ext:evil")

;;; - Color Constants

(let ((class '((class color)))

      ;;; -- Sane defaults
      (bg-main       "#050505")
      (bg-dim        "#121212")
      (bg-inactive   "#303030")
      (fg-comment    "#b4aeae")
      (fg-main       "#FFFFFF")
      (fg-dim        "#D8DEE9")
      (fg-inactive   "#6c7b8b")
      (fg-region     haki-region) ;; Changeable via setq
      (cursor        "#ffe4e1")

      ;; --- Common logics
      (error      "#ee6363")
      (link       "#fcde69")
      (todo       "#54ff9f")
      (done       "#b4dddd")
      (code       "#77ee77")
      (verbatim   "#ee77ee")
      (clock      "#eedc82")
      (bg-tag     "#ffe1ff")
      (bracket    "#ffeeee")

      ;; --- Colours are named in order from 1 --> 5
      ;; --- 1 as pale, and progressively intense, 5 as dark

      ;; --- Rgb
      (blue-5   "#b0e0e6")
      (yellow-5 "#eee685")

      ;; -- For Code
      (c-keyword     "#00cdcd")
      (c-func        "#54ff9f")
      (c-builtin     "#ee88ee")
      (c-const       "#ff6a6a")
      (c-var         "#ffb5c5")
      (c-string      "#ffec8b")
      (c-operator    "#ee9572")
      (c-regexc      "#9bcd9b")
      (c-regexb      "#a2cd5a")
      (c-warning     "#97ffff")
      (c-property    "#EE7286")

      ;; --- For diffs
      ;; note of truth is, I borrowed it from modus-vivendi.
      ;; Credits to Protesiloas
      (bg-added           "#00601f")
      (bg-added-4         "#002313")
      (bg-added-refine    "#044f2f")
      (fg-added           "#a0e0a0")
      (fg-added-5         "#80e080")

      (bg-changed         "#362400")
      (bg-changed-4       "#2b1f00")
      (bg-changed-refine  "#4b4a10")
      (fg-changed         "#efdf80")
      (fg-changed-5       "#c0b06f")

      (bg-removed         "#5f1219")
      (bg-removed-4       "#380a0e")
      (bg-removed-refine  "#681a2f")
      (fg-removed         "#ffbfbf")
      (fg-removed-5       "#ff9095")

      (bg-diff-context    "#1a1a1a")

      ;; --- For headings
      (title        "#6ae4b9")
      (heading-1    "#ab82ff")
      (heading-2    "#ffec8b")
      (heading-3    "#3cb6df")
      (heading-4    "#9aff9a")
      (heading-5    "#97ffff")
      (heading-6    "#ffa07a")
      (heading-7    "#eeb4b4")
      (heading-8    "#ee4000")
      (heading-9    "#cd96cd")

      ;; --- Defining font to class (I'm not sure if i should give new name?)
      (haki-link-font      haki-link-font)
      (haki-heading-font   haki-heading-font)
      (haki-code-font      haki-code-font)
      (haki-sans-font      haki-sans-font)
      (haki-title-font     haki-title-font))

;;; --- change mode-line border for meow/evil states
  (defun haki-modal-mode-line ()
    "Changes mode-line border accordingly to meow/evil states
Respected Only in GUI frame"
    (when (require 'meow nil t)
      (cond
       ((meow-beacon-mode-p) (set-face-attribute 'mode-line nil :box heading-5))
       ((meow-insert-mode-p) (set-face-attribute 'mode-line nil :box heading-4))
       ((meow-normal-mode-p) (set-face-attribute 'mode-line nil :box cursor))
       ((meow-motion-mode-p) (set-face-attribute 'mode-line nil :box heading-2))
       ((meow-keypad-mode-p) (set-face-attribute 'mode-line nil :box heading-3))))
    (when (require 'evil nil t)
      (cond
       ((evil-visual-state-p) (set-face-attribute 'mode-line nil :box heading-5))
       ((evil-insert-state-p) (set-face-attribute 'mode-line nil :box heading-4))
       ((evil-normal-state-p) (set-face-attribute 'mode-line nil :box cursor))
       ((evil-motion-state-p) (set-face-attribute 'mode-line nil :box heading-2))
       ((evil-operator-state-p) (set-face-attribute 'mode-line nil :box heading-3))
       ((evil-emacs-state-p) (set-face-attribute 'mode-line nil :box heading-1))
       ((evil-replace-state-p) (set-face-attribute 'mode-line nil :box title)))))

;;; - Core Faces
  (custom-theme-set-faces
   'haki

;;; --- General Order: :family :font :width :height :weight :slant :underline :foreground :background :extend :inherit
;;; --- Do it as per what should overwrite what

   `(default                 ((,class :background ,bg-main :foreground ,fg-main)))

;;; -- Base
   `(mode-line               ((,class :background ,bg-dim :box (:line-width (2 . 1) :color ,cursor) :weight medium :height 0.9)))
   `(mode-line-inactive      ((,class :inherit mode-line :foreground ,fg-dim :background ,bg-inactive)))
   `(header-line             ((,class :inherit mode-line :foreground ,title)))

   ;; Structural
   `(bold                      ((,class :weight bold)))
   `(italic                    ((,class :slant italic)))
   `(bold-italic               ((,class :slant italic :weight bold)))
   `(underline                 ((,class :underline t)))
   `(region                    ((,class :background ,fg-region :weight semi-bold)))
   `(highlight                 ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(fixed-pitch-serif         ((,class :inherit default)))
   `(variable-pitch            ((,class :inherit default)))
   `(cursor                    ((,class :background ,cursor)))
   `(hl-line                   ((,class :extend t :background ,bg-dim)))
   `(link                      ((,class :font ,haki-link-font :slant italic :underline t :weight medium :foreground ,link :height 1.1)))
   `(button                    ((,class :inherit (bold link) :foreground ,c-operator)))
   `(separator-line            ((,class :underline ,fg-comment :extend t)))

   ;; --- Minibuffer
   `(completions-annotations         ((,class :inherit italic :foreground ,blue-5)))
   `(completions-common-part         ((,class :foreground ,heading-1)))
   `(completions-first-difference    ((,class )))

   ;; Modeline
   `(doom-modeline-bar                  ((,class :background ,haki-region)))
   `(doom-modeline-buffer-file          ((,class :inherit  (doom-modeline bold) :foreground ,heading-1)))
   `(doom-modeline-buffer-major-mode    ((,class :inherit  (doom-modeline-emphasis bold) :foreground ,heading-2)))
   `(doom-modeline-time                 ((,class :inherit doom-modeline-buffer-file :foreground ,heading-6)))
   `(doom-modeline-info                 ((,class :inherit  (doom-modeline success bold))))
   `(doom-modeline-buffer-modified      ((,class :inherit (doom-modeline bold) :foreground ,heading-4)))
   `(doom-modeline-emphasis             ((,class :inherit  (doom-modeline success bold))))

;;; --- isearch
   `(isearch                            ((,class :foreground ,c-var)))
   `(isearch-fail                       ((,class :foreground ,error)))
   `(isearch-group-1                    ((,class :foreground ,c-regexc)))
   `(isearch-group-2                    ((,class :foreground ,c-regexb)))
   `(lazy-highlight                     ((,class :inherit highlight)))
   `(match                              ((,class :inherit consult-preview-match)))
   `(query-replace                      ((,class :inherit consult-preview-match)))
;;; --- keycast
   `(keycast-command                    ((,class :inherit bold)))
   `(keycast-key                        ((,class :foreground ,bg-main)))
;;; --- line numbers
   `(line-number                        ((,class :inherit fixed-pitch :weight medium :foreground ,fg-inactive)))
   `(line-number-current-line           ((,class :inherit fixed-pitch :weight ultra-bold :background ,fg-region :foreground ,fg-main :box ,haki-region)))
   `(line-number-major-tick             ((,class :inherit line-number :foreground ,error)))
   `(line-number-minor-tick             ((,class :inherit line-number :foreground ,fg-inactive)))

;;; --- font locks
   `(font-lock-doc-face                        ((,class :foreground ,fg-dim :slant italic)))
   `(font-lock-comment-face                    ((,class :foreground ,fg-comment :slant italic)))
   `(font-lock-builtin-face                    ((,class :inherit bold :foreground ,c-builtin)))
   `(font-lock-comment-delimiter-face          ((,class :inherit font-lock-comment-face)))
   `(font-lock-constant-face                   ((,class :foreground ,c-const)))
   `(font-lock-doc-markup-face                 ((,class :inherit (slant font-lock-doc-face))))
   `(font-lock-function-name-face              ((,class :foreground ,c-func)))
   `(font-lock-keyword-face                    ((,class :inherit bold :foreground ,c-keyword)))
   `(font-lock-negation-char-face              ((,class :inherit error)))
   `(font-lock-preprocessor-face               ((,class :foreground ,c-operator)))
   `(font-lock-regexp-grouping-backslash       ((,class :inherit bold :foreground ,c-regexb)))
   `(font-lock-regexp-grouping-construct       ((,class :inherit bold :foreground ,c-regexc)))
   `(font-lock-string-face                     ((,class :foreground ,c-string :slant italic)))
   `(font-lock-type-face                       ((,class :inherit bold :foreground ,c-string)))
   `(font-lock-variable-name-face              ((,class :foreground ,c-var)))
   `(font-lock-warning-face                    ((,class :inherit bold :foreground ,c-warning)))
   `(font-lock-punctuation-face                ((,class :inherit (italic) :foreground ,bracket)))
   `(font-lock-operator-face                   ((,class :foreground ,c-operator)))
   `(font-lock-property-name-face              ((,class :foreground ,c-property)))
   ;; --- these are in emacs 29 (treesit)
   `(font-lock-bracket-face                   ((,class :inherit font-lock-punctuation-face)))
   `(font-lock-delimiter-face                 ((,class :inherit font-lock-punctuation-face)))
   `(font-lock-escape-face                    ((,class :inherit font-lock-regexp-grouping-backslash)))
   `(font-lock-function-call-face             ((,class :inherit font-lock-function-name-face)))
   `(font-lock-misc-punctuation-face          ((,class :inherit font-lock-punctuation-face)))
   `(font-lock-number-face                    ((,class)))
   `(font-lock-property-use-face              ((,class :inherit font-lock-property-name-face)))
   `(font-lock-operator-face                  ((,class)))
   `(font-lock-regexp-face                    ((,class :inherit font-lock-string-face)))
   `(font-lock-variable-use-face              ((,class :inherit font-lock-variable-name-face)))


;;; --- Org mode
   `(org-level-1                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-1 :height 1.30)))
   `(org-level-2                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-2 :height 1.25)))
   `(org-level-3                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-3 :height 1.20)))
   `(org-level-4                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-4 :height 1.15)))
   `(org-level-5                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-5 :height 1.10)))
   `(org-level-6                             ((,class :font ,haki-heading-font :weight bold :foreground ,heading-6 :height 1.10)))
   `(org-archived                            ((,class :foreground ,fg-dim)))
   `(org-block                               ((,class :background ,bg-dim)))
   `(org-block-begin-line                    ((,class  :underline (:line-width (1 . 1) :color ,cursor) :extend t :weight semi-bold :height 0.9 :foreground ,fg-dim)))
   `(org-block-end-line                      ((,class :inherit org-block-begin-line)))
   `(org-checkbox                            ((,class :foreground ,yellow-5)))
   `(org-checkbox-statistics-done            ((,class :foreground ,done)))
   `(org-checkbox-statistics-todo            ((,class :foreground ,todo)))
   `(org-clock-overlay                       ((,class :foreground ,clock)))
   `(org-code                                ((,class :font ,haki-code-font :weight medium :height 1.1 :foreground ,code)))
   `(org-column                              ((,class :foreground ,fg-main)))
   `(org-column-title                        ((,class :foreground ,fg-main)))
   `(org-date                                ((,class :foreground ,fg-main)))
   `(org-date-selected                       ((,class :foreground ,fg-main)))
   `(org-default                             ((,class :foreground ,fg-main)))
   `(org-document-info                       ((,class :font ,haki-sans-font :height 1.5 :weight bold :slant italic :foreground ,blue-5)))
   `(org-document-info-keyword               ((,class :foreground ,fg-inactive)))
   `(org-document-title                      ((,class :font ,haki-title-font :foreground ,title :height 1.7 :weight bold :width extra-expanded)))
   `(org-done                                ((,class :background ,done)))
   `(org-drawer                              ((,class :foreground ,fg-inactive)))
   `(org-ellipsis                            ((,class :foreground ,clock)))
   `(org-footnote                            ((,class :foreground ,fg-main)))
   `(org-formula                             ((,class :foreground ,fg-main)))
   `(org-headline-done                       ((,class :foreground ,fg-dim)))
   `(org-latex-and-related                   ((,class :foreground ,blue-5)))
   `(org-link                                ((,class :inherit link)))
   `(org-list-dt                             ((,class :inherit bold)))
   `(org-macro                               ((,class :foreground ,fg-main)))
   `(org-meta-line                           ((,class :foreground ,fg-inactive)))
   `(org-mode-line-clock                     ((,class :foreground ,fg-dim)))
   `(org-mode-line-clock-overrun             ((,class :foreground ,error)))
   `(org-priority                            ((,class :foreground ,bg-main)))
   `(org-property-value                      ((,class :foreground ,c-var)))
   `(org-quote                               ((,class )))
   `(org-scheduled                           ((,class :foreground ,fg-main)))
   `(org-scheduled-previously                ((,class :foreground ,fg-dim)))
   `(org-scheduled-today                     ((,class :foreground ,fg-main)))
   `(org-sexp-date                           ((,class :foreground ,bg-main)))
   `(org-special-keyword                     ((,class :foreground ,fg-inactive)))
   `(org-table                               ((,class :foreground ,c-warning)))
   `(org-table-header                        ((,class :foreground ,title :inherit (bold org-table))))
   `(org-tag                                 ((,class :width condensed :height 0.9 :weight regular :underline nil :box (:color ,cursor :line-width (1 . -3)) :background ,bg-tag :foreground ,bg-dim)))
   `(org-tag-group                           ((,class )))
   `(org-target                              ((,class :foreground ,c-operator :slant italic)))
   `(org-time-grid                           ((,class :foreground ,bg-main)))
   `(org-todo                                ((,class :foreground ,todo :background ,bg-dim)))
   `(org-upcoming-deadline                   ((,class :foreground ,error :underline t)))
   `(org-verbatim                            ((,class :inherit org-code :foreground ,verbatim)))
   `(org-verse                               ((,class )))
   `(org-warning                             ((,class :foreground ,error)))
   `(org-agenda-calendar-event               ((,class )))
   `(org-agenda-calendar-sexp                ((,class )))
   `(org-agenda-clocking                     ((,class :foreground ,clock)))
   `(org-agenda-column-dateline              ((,class :background ,bg-dim)))
   `(org-agenda-current-time                 ((,class :foreground ,blue-5)))
   `(org-agenda-date                         ((,class :inherit bold :foreground ,clock)))
   `(org-agenda-date-today                   ((,class :inherit doom-modeline-time)))
   `(org-agenda-date-weekend                 ((,class )))
   `(org-agenda-date-weekend-today           ((,class )))
   `(org-agenda-diary                        ((,class :inherit org-agenda-calendar-sexp)))
   `(org-agenda-dimmed-todo-face             ((,class :inherit shadow)))
   `(org-agenda-done                         ((,class :inherit org-done)))
   `(org-agenda-filter-category              ((,class :inherit bold :foreground ,c-keyword)))
   `(org-agenda-filter-effort                ((,class :inherit bold :foreground ,c-keyword)))
   `(org-agenda-filter-regexp                ((,class :inherit bold :foreground ,c-keyword)))
   `(org-agenda-filter-tags                  ((,class :inherit bold :foreground ,c-keyword)))
   `(org-agenda-restriction-lock             ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(org-agenda-structure                    ((,class :inherit magit-log-date)))
   `(org-agenda-structure-filter             ((,class :inherit org-agenda-structure :foreground ,c-warning)))
   `(org-agenda-structure-secondary          ((,class :foreground ,c-warning)))

;;; --- Org Modern
   `(org-modern-tag                          ((,class :inherit (org-modern-label))))
   `(org-modern-done                         ((,class :inherit (org-done org-modern-label))))
   `(org-modern-todo                         ((,class :weight semibold :inverse-video t :inherit (org-todo org-modern-label))))
   `(org-modern-label                        ((,class :width condensed :height 0.9 :weight regular :underline nil :box (:color ,cursor :line-width (1 . -3)) :background ,bg-tag :foreground ,bg-dim)))
   `(org-modern-symbol                       ((,class :inherit bold)))
   `(org-modern-priority                     ((,class :weight semibold :inverse-video t :inherit   (org-priority org-modern-label))))
   `(org-modern-block-name                   ((,class :inherit (org-block-begin-line))))
   `(org-modern-statistics                   ((,class :inherit org-modern-done)))
   `(org-modern-horizontal-rule              ((,class :strike-through ,fg-inactive :inherit org-hide)))
   `(org-modern-date-active                  ((,class :inherit org-modern-done)))
   `(org-modern-date-inactive                ((,class :foreground ,bg-inactive :background ,fg-comment :inherit 'org-modern-label)))

;;; --- Olivetti (No fringe, so it looks good)
   `(fringe          ((,class )))
   `(olivetti-fringe ((,class )))

;;; --- Rainbow delimiters (parenthesis world)
   `(rainbow-delimiters-base-face nil)
   `(rainbow-delimiters-depth-1-face              ((,class :foreground ,heading-1 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-2-face              ((,class :foreground ,heading-2 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-3-face              ((,class :foreground ,heading-3 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-4-face              ((,class :foreground ,heading-4 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-5-face              ((,class :foreground ,heading-5 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-6-face              ((,class :foreground ,heading-6 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-7-face              ((,class :foreground ,heading-7 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-8-face              ((,class :foreground ,heading-8 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-9-face              ((,class :foreground ,heading-9 :inherit 'rainbow-delimiters-base-face)))
   `(rainbow-delimiters-base-error-face           ((,class :foreground ,error :inherit 'rainbow-delimiters-base-face :underline ,link)))

;;; --- Dired
   `(dired-broken-symlink       ((,class :inherit button :foreground ,error)))
   `(dired-directory            ((,class :inherit bold :foreground ,heading-9)))
   `(dired-flagged              ((,class :foreground ,c-operator)))
   `(dired-header               ((,class :inherit bold :foreground ,c-var)))
   `(dired-ignored              ((,class :inherit shadow)))
   `(dired-mark                 ((,class :inherit bold)))
   `(dired-marked               ((,class :foreground ,c-const)))
   `(dired-perm-write           ((,class :inherit shadow)))
   `(dired-symlink              ((,class :inherit button :foreground ,c-warning :underline t)))
   `(dired-warning              ((,class :inherit warning)))
;;; --- dired-async
   `(dired-async-failures       ((,class :inherit error)))
   `(dired-async-message        ((,class :inherit bold)))
   `(dired-async-mode-message   ((,class :inherit bold)))
;;; --- dired-git
   `(dired-git-branch-else      ((,class :inherit bold :foreground ,fg-main)))
   `(dired-git-branch-master    ((,class :inherit bold :foreground ,fg-main)))
;;; --- dired-git-info
   `(dgi-commit-message-face    ((,class :foreground ,c-keyword)))
;;; --- dired-subtree
   `(dired-subtree-depth-1-face (()))
   `(dired-subtree-depth-2-face (()))
   `(dired-subtree-depth-3-face (()))
   `(dired-subtree-depth-4-face (()))
   `(dired-subtree-depth-5-face (()))
   `(dired-subtree-depth-6-face (()))

;;; --- all-the-icons
   `(all-the-icons-dired-dir-face    ((,class :inherit bold :foreground ,c-func)))
   `(all-the-icons-blue              ((,class :foreground "#00b2ee")))
   `(all-the-icons-blue-2            ((,class :foreground "#7ec0ee")))
   `(all-the-icons-cyan              ((,class :foreground "#00cdcd")))
   `(all-the-icons-cyan-2            ((,class :foreground "#00ffff")))
   `(all-the-icons-dblue             ((,class :foreground "#3a5fcd")))
   `(all-the-icons-dcyan             ((,class :foreground "#00ffff")))
   `(all-the-icons-dgreen            ((,class :foreground "#00fa9a")))
   `(all-the-icons-dmaroon           ((,class :foreground "#ee30a7")))
   `(all-the-icons-dorange           ((,class :foreground "#ee9a00")))
   `(all-the-icons-dpink             ((,class :foreground "#ffaeb9")))
   `(all-the-icons-dpurple           ((,class :foreground "#9f79ee")))
   `(all-the-icons-dred              ((,class :foreground "#ed8259")))
   `(all-the-icons-dsilver           ((,class :foreground "#b9d3ee")))
   `(all-the-icons-dyellow           ((,class :foreground "#ffec8b")))
   `(all-the-icons-green             ((,class :foreground "#90ee90")))
   `(all-the-icons-lblue             ((,class :foreground "#b2dfee")))
   `(all-the-icons-lcyan             ((,class :foreground "#d1eeee")))
   `(all-the-icons-lgreen            ((,class :foreground "#9aff9a")))
   `(all-the-icons-lmaroon           ((,class :foreground "#eeaeee")))
   `(all-the-icons-lorange           ((,class :foreground "#ee9b19")))
   `(all-the-icons-lpink             ((,class :foreground "#ffc1c1")))
   `(all-the-icons-lpurple           ((,class :foreground "#e6e6fa")))
   `(all-the-icons-lred              ((,class :foreground "#ee8262")))
   `(all-the-icons-lyellow           ((,class :foreground "#eeee00")))
   `(all-the-icons-maroon            ((,class :foreground "#ff6a6a")))
   `(all-the-icons-red               ((,class :foreground "#8b3626")))
   `(all-the-icons-red-2             ((,class :foreground "#f4a460")))
   `(all-the-icons-yellow            ((,class :foreground "#eedc82")))

;;; --- Elfeed
   `(elfeed-search-tag-face                            ((,class :weight normal :height 0.9 :foreground ,heading-7 :slant italic)))
   `(elfeed-search-date-face                           ((,class :height 0.8 :width condensed :foreground ,c-var)))
   `(elfeed-search-feed-face                           ((,class :weight medium :foreground ,heading-9)))
   `(elfeed-search-filter-face                         ((,class :foreground ,heading-6)))
   `(elfeed-search-last-update-face                    ((,class :foreground ,fg-dim)))
   `(elfeed-search-title-face                          ((,class :font ,haki-heading-font :foreground ,fg-comment :height 1.1)))
   `(elfeed-search-unread-count-face                   ((,class :foreground ,blue-5)))
   `(elfeed-search-unread-title-face                   ((,class :inherit bold :foreground ,title)))
   `(elfeed-log-date-face                              ((,class :inherit elfeed-search-date-face)))
   `(elfeed-log-info-level-face                        ((,class :inherit elfeed-search-tag-face)))
   `(elfeed-log-debug-level-face                       ((,class :foreground ,link)))
   `(elfeed-log-warn-level-face                        ((,class :foreground ,error)))
   `(elfeed-log-error-level-face                       ((,class :foreground ,fg-main)))

;;; --- The five package for modern emacs
;;; --- Vertico
   `(vertico-current           ((,class :extend t :inherit (region))))

;;; --- Corfu
   `(corfu-current                     ((,class :inherit vertico-current)))
   `(corfu-bar                         ((,class :background ,fg-dim)))
   `(corfu-border                      ((,class :background ,fg-inactive)))
   `(corfu-default                     ((,class :background ,bg-dim)))
   `(corfu-popupinfo                   ((,class :inherit corfu-default :extend t)))
   `(corfu-annotations                 ((,class :inherit completions-annotations)))

;;; --- Consult
   `(consult-async-split               ((,class :inherit error)))
   `(consult-file                      ((,class :inherit bold :foreground ,fg-comment)))
   `(consult-key                       ((,class)))
   `(consult-buffer                    ((,class :foreground ,fg-dim)))
   `(consult-imenu-prefix              ((,class :inherit shadow)))
   `(consult-line-number               ((,class :inherit shadow)))
   `(consult-line-number-prefix        ((,class :inherit shadow)))
   `(consult-preview-line              ((,class :inherit (vertico-current))))
   `(consult-preview-match             ((,class :foreground ,link :background ,bg-inactive)))

;;; --- Orderless
   `(orderless-match-face-0            ((,class :foreground ,heading-1)))
   `(orderless-match-face-1            ((,class :foreground ,heading-2)))
   `(orderless-match-face-2            ((,class :foreground ,heading-6)))
   `(orderless-match-face-3            ((,class :foreground ,heading-5)))

;;; --- Marginalia
   `(marginalia-archive                ((,class :foreground ,c-operator)))
   `(marginalia-char                   ((,class :foreground ,blue-5)))
   `(marginalia-date                   ((,class :foreground ,verbatim)))
   `(marginalia-documentation          ((,class :slant italic :foreground ,c-warning)))
   `(marginalia-file-name              (( )))
   `(marginalia-file-owner             ((,class :inherit shadow)))
   `(marginalia-file-priv-dir          ((,class :foreground ,c-regexb)))
   `(marginalia-file-priv-exec         ((,class :foreground ,c-const)))
   `(marginalia-file-priv-link         ((,class :foreground ,c-regexc)))
   `(marginalia-file-priv-no           ((,class :inherit shadow)))
   `(marginalia-file-priv-read         ((,class :foreground ,c-builtin)))
   `(marginalia-file-priv-write        ((,class :foreground ,fg-main)))
   `(marginalia-function               ((,class :foreground ,c-func)))
   `(marginalia-key                    ((,class :inherit bold :foreground ,c-var)))
   `(marginalia-lighter                ((,class :inherit shadow)))
   `(marginalia-list                   ((,class :inherit shadow)))
   `(marginalia-mode                   ((,class :foreground ,heading-2)))
   `(marginalia-modified               ((,class :inherit warning)))
   `(marginalia-null                   ((,class :inherit shadow)))
   `(marginalia-number                 ((,class :inherit consult-line-number)))
   `(marginalia-size                   ((,class :foreground ,c-string)))
   `(marginalia-string                 ((,class :foreground ,c-string)))
   `(marginalia-symbol                 ((,class :foreground ,c-builtin)))
   `(marginalia-true                   (( )))
   `(marginalia-type                   ((,class :foreground ,c-keyword)))
   `(marginalia-value                  ((,class :inherit shadow)))
   `(marginalia-version                ((,class :foreground ,c-keyword)))

;;; --- Tempel
   `(tempel-form                       ((,class :inherit tempel-default :foreground ,clock :box ,clock)))
   `(tempel-field                      ((,class :inherit tempel-default :foreground ,todo :box ,todo)))
   `(tempel-default                    ((,class :inherit (bold italic) :foreground ,c-warning :box ,c-warning)))

;;; --- Jinx
   `(jinx-accept                       ((,class :inherit font-lock-negation-char-face)))
   `(jinx-highlight                    ((,class :background ,yellow-5 :foreground ,bg-inactive)))
   `(jinx-misspelled                   ((,class :underline ,link)))

;;; --- Dictionary (better use sdcv)
   `(dictionary-button-face            ((,class :inherit bold)))
   `(dictionary-reference-face         ((,class :inherit link)))
   `(dictionary-word-definition-face   (( )))
   `(dictionary-word-entry-face        ((,class :inherit font-lock-comment-face)))

;;; --- Shr
   `(shr-abbreviation     ((,class :inherit (org-list-dt org-verbatim))))
   `(shr-code             ((,class :inherit (org-block fixed-pitch))))
   `(shr-h1               ((,class :inherit message-header-subject)))
   `(shr-h2               ((,class :inherit org-level-1)))
   `(shr-h3               ((,class :inherit org-level-3)))
   `(shr-h4               ((,class :inherit org-level-4)))
   `(shr-h5               ((,class :inherit org-level-5)))
   `(shr-h6               ((,class :inherit org-level-6)))
   `(shr-selected-link    ((,class :inherit link :box t)))
   `(shr-link             ((,class :inherit link :foreground ,link)))
   `(shr-text             ((,class )))

;;; --- eww
   `(eww-invalid-certificate      ((,class :foreground ,error)))
   `(eww-valid-certificate        ((,class :foreground ,done)))
   `(eww-form-checkbox            ((,class :inherit org-checkbox :height 0.9)))
   `(eww-form-file                ((,class :inherit eww-form-submit :foreground ,error)))
   `(eww-form-select              ((,class :inherit eww-form-submit :foreground ,bg-tag)))
   `(eww-form-submit              ((,class :background ,bg-inactive :box t :foreground ,blue-5)))
   `(eww-form-text                ((,class :background ,bg-inactive :foreground ,fg-main :extend nil)))
   `(eww-form-textarea            ((,class :inherit eww-form-text :foreground ,fg-dim)))

;;; --- Mingus
   `(mingus-mark-face             ((,class :inherit dired-mark)))
   `(mingus-artist-face           ((,class :foreground ,heading-1)))
   `(mingus-album-face            ((,class :underline t :foreground ,title)))
   `(mingus-playlist-face         ((,class :foreground ,c-operator)))
   `(mingus-directory-face        ((,class :inherit dired-directory)))
   `(mingus-playing-face          ((,class :foreground ,heading-5)))
   `(mingus-pausing-face          ((,class :foreground ,yellow-5)))
   `(mingus-stopped-face          ((,class :foreground ,error)))
   `(mingus-song-file-face        ((,class :foreground ,c-string)))
   `(mingus-album-stale-face      ((,class :foreground ,c-regexb)))

;;; --- WhichKey
   `(which-func                                 ((,class :inherit bold :foreground ,blue-5)))
   `(which-key-command-description-face         ((,class :foreground ,heading-2)))
   `(which-key-group-description-face           ((,class :foreground ,error)))
   `(which-key-highlighted-command-face         ((,class :foreground ,c-warning :underline t)))
   `(which-key-key-face                         ((,class :inherit marginalia-key)))
   `(which-key-local-map-description-face       ((,class :foreground ,heading-4)))
   `(which-key-note-face                        ((,class :foreground ,fg-comment)))
   `(which-key-docstring-face                   ((,class :inherit which-key-note-face)))
   `(which-key-separator-face                   ((,class :inherit shadow)))
   `(which-key-special-key-face                 ((,class :inherit error)))

;;; --- diff-mode
   `(diff-added                      ((,class :background ,bg-added :foreground ,fg-added)))
   `(diff-changed                    ((,class :background ,bg-inactive :foreground ,fg-changed :extend t)))
   `(diff-changed-unspecified        ((,class :inherit diff-changed)))
   `(diff-removed                    ((,class :background ,bg-removed :foreground ,fg-removed)))
   `(diff-refine-added               ((,class :background ,bg-added-refine :foreground ,fg-added)))
   `(diff-refine-changed             ((,class :background ,bg-changed-refine :foreground ,fg-changed)))
   `(diff-refine-removed             ((,class :background ,bg-removed-refine :foreground ,fg-removed)))
   `(diff-indicator-added            ((,class :inherit diff-added :foreground ,fg-added-5)))
   `(diff-indicator-changed          ((,class :inherit diff-changed :foreground ,fg-changed-5)))
   `(diff-indicator-removed          ((,class :inherit diff-removed :foreground ,fg-removed-5)))
   `(diff-context                    (( )))
   `(diff-error                      ((,class :inherit error)))
   `(diff-file-header                ((,class :inherit bold)))
   `(diff-function                   ((,class :background ,bg-inactive)))
   `(diff-header                     (( )))
   `(diff-hunk-header                ((,class :inherit bold :background ,bg-inactive)))
   `(diff-index                      ((,class :inherit italic)))
   `(diff-nonexistent                ((,class :inherit bold)))

   ;; --- ediff
   `(ediff-current-diff-A            ((,class :background ,bg-removed :foreground ,fg-removed)))
   `(ediff-current-diff-Ancestor     ((,class :background ,bg-inactive)))
   `(ediff-current-diff-B            ((,class :background ,bg-added :foreground ,fg-added)))
   `(ediff-current-diff-C            ((,class :background ,bg-changed :foreground ,fg-changed)))
   `(ediff-even-diff-A               ((,class :background ,bg-diff-context)))
   `(ediff-even-diff-Ancestor        ((,class :background ,bg-diff-context)))
   `(ediff-even-diff-B               ((,class :background ,bg-diff-context)))
   `(ediff-even-diff-C               ((,class :background ,bg-diff-context)))
   `(ediff-fine-diff-A               ((,class :background ,bg-removed-refine :foreground ,fg-removed)))
   `(ediff-fine-diff-Ancestor        ((,class :inherit dtest-themes-subtle-cyan)))
   `(ediff-fine-diff-B               ((,class :background ,bg-added-refine :foreground ,fg-added)))
   `(ediff-fine-diff-C               ((,class :background ,bg-changed-refine :foreground ,fg-changed)))
   `(ediff-odd-diff-A                ((,class :inherit ediff-even-diff-A)))
   `(ediff-odd-diff-Ancestor         ((,class :inherit ediff-even-diff-Ancestor)))
   `(ediff-odd-diff-B                ((,class :inherit ediff-even-diff-B)))
   `(ediff-odd-diff-C                ((,class :inherit ediff-even-diff-C)))

   ;; --- Eglot
   `(eglot-mode-line                         ((,class :inherit bold :foreground ,clock)))
   `(eglot-type-hint-face                    ((,class :inherit eglot-inlay-hint-face)))
   `(eglot-inlay-hint-face                   ((,class :height 0.8 :inherit shadow :foreground ,fg-dim)))
   `(eglot-parameter-hint-face               ((,class :inherit eglot-inlay-hint-face)))
   `(eglot-highlight-symbol-face             ((,class :inherit bold)))
   `(eglot-diagnostic-tag-deprecated-face    ((,class :strike-t<hrough t :inherit shadow)))
   `(eglot-diagnostic-tag-unnecessary-face   ((,class :inherit shadow)))

;;; --- Eldoc-box
   `(eldoc-box-body              ((,class :background ,bg-dim :foreground ,fg-main)))
   `(eldoc-box-border            ((,class :background ,cursor)))

;;; --- flycheck
   `(flycheck-error             ((,class :underline (:style wave :color ,error))))
   `(flycheck-info              ((,class :underline (:style wave :color ,fg-region))))
   `(flycheck-warning           ((,class :underline (:style wave :color ,yellow-5))))
   `(flycheck-fringe-error      ((,class :inherit bold :background ,error :foreground ,bg-dim)))
   `(flycheck-fringe-info       ((,class :inherit bold :background ,cursor :foreground ,bg-dim)))
   `(flycheck-fringe-warning    ((,class :inherit bold :background ,yellow-5 :foreground ,bg-main)))

;;; ---  flycheck-color-mode-line
   `(flycheck-color-mode-line-error-face         ((,class :inherit flycheck-fringe-error)))
   `(flycheck-color-mode-line-info-face          ((,class :inherit flycheck-fringe-info)))
   `(flycheck-color-mode-line-running-face       ((,class :inherit italic)))
   `(flycheck-color-mode-line-info-face          ((,class :inherit flycheck-fringe-warning)))

;;; --- flycheck-indicator
   `(flycheck-indicator-disabled         ((,class :inherit italic :foreground ,fg-dim)))
   `(flycheck-indicator-error            ((,class :inherit error)))
   `(flycheck-indicator-info             ((,class :inherit bold)))
   `(flycheck-indicator-running          ((,class :inherit italic)))
   `(flycheck-indicator-success          ((,class :inherit success)))
   `(flycheck-indicator-warning          ((,class :inherit warning)))

;;; --- flymake
   `(flymake-error                                ((,class :inherit flycheck-error)))
   `(flymake-error-echo                           ((,class :inherit error)))
   `(flymake-note                                 ((,class :inherit flycheck-info)))
   `(flymake-note-echo                            ((,class :inherit success)))
   `(flymake-warning                              ((,class :inherit flycheck-warning)))
   `(flymake-warning-echo                         ((,class :inherit warning)))
   `(flymake-note-echo-at-eol                     ((,class :foreground ,fg-region)))

;;; --- flyspell (better use jinx)
   `(flyspell-duplicate         ((,class :inherit jinx-highlight)))
   `(flyspell-incorrect         ((,class :inherit jinx-misspelled)))

;;; --- shell-script
   `(sh-heredoc           ((,class :inherit font-lock-string-face)))
   `(sh-quoted-exec       ((,class :inherit font-lock-builtin-face)))

;;; --- shortdoc
   `(shortdoc-heading     ((,class :inherit info-title-2)))
   `(shortdoc-section     ((,class :inherit info-menu-header)))

;;; --- doc-view
   ;; TODO: might be in emacs30
   ;; `(doc-view-svg-background    ((,class :color ,bg-main)))
   ;; `(doc-view-svg-foreground    ((,class :color ,fg-main)))

;;; --- show-paren
   `(show-paren-match               ((,class :inherit bold :foreground ,bg-inactive :background ,done)))
   `(show-paren-match-expression    ((,class :background ,bg-inactive)))
   `(show-paren-mismatch            ((,class :inherit error)))

;;; --- Message
   `(message-cited-text-1           ((,class :inherit org-level-1)))
   `(message-cited-text-2           ((,class :inherit org-level-2)))
   `(message-cited-text-3           ((,class :inherit org-level-3)))
   `(message-cited-text-4           ((,class :inherit org-level-4)))
   `(message-header-name            ((,class :height 0.9 :foreground ,heading-7)))
   `(message-header-newsgroups      ((,class :inherit message-header-other)))
   `(message-header-to              ((,class :font ,haki-sans-font :inherit bold :slant italic :height 1.5 :foreground ,heading-1)))
   `(message-header-cc              ((,class :foreground ,c-var)))
   `(message-header-subject         ((,class :font ,haki-title-font :inherit bold :height 1.8 :foreground ,title)))
   `(message-header-xheader         ((,class :inherit message-header-other)))
   `(message-header-other           ((,class :inherit bold :font ,haki-sans-font :height 1.0 :foreground ,c-warning)))
   `(message-mml                    ((,class :foreground ,c-property)))
   `(message-separator              ((,class :inherit separator-line)))
   `(header-line                    ((,class :height 0.9)))

;;; --- Info
   `(Info-quoted            ((,class :inherit org-verbatim)))
   `(info-header-node       ((,class :inherit (shadow bold) :foreground ,c-var)))
   `(info-xref-visited      ((,class :inherit link :foreground ,heading-6)))
   `(info-xref              ((,class :foreground ,clock)))
   `(info-header-xref       ((,class :foreground ,c-regexb)))
   `(info-index-match       ((,class :inherit highlight)))
   `(info-menu-star         ((,class :foreground ,error)))
   `(info-node              ((,class :inherit bold)))
   `(info-title-1           ((,class :inherit org-level-1)))
   `(info-title-2           ((,class :inherit org-level-2)))
   `(info-title-3           ((,class :inherit org-level-3)))
   `(info-title-4           ((,class :inherit org-level-4)))
   `(info-menu-header       ((,class :inherit org-level-5)))

;;; --- helpful
   `(helpful-heading          ((,class :inherit org-level-1)))
   `(help-for-help-header     ((,class :inherit helpful-heading)))
   `(help-key-binding         ((,class :inherit marginalia-key :background ,bg-dim)))

;;; --- customize
   `(custom-button       ((,class :inherit button)))

;;; --- popup-tip
   `(popup-face         ((,class :inherit corfu-default)))
   `(popup-tip-face     ((,class :inherit corfu-default :background ,bg-inactive)))

;;; --- Outline
   `(outline-1          ((,class :inherit org-level-1)))
   `(outline-2          ((,class :inherit org-level-2)))
   `(outline-3          ((,class :inherit org-level-3)))
   `(outline-4          ((,class :inherit org-level-4)))
   `(outline-5          ((,class :inherit org-level-5)))
   `(outline-6          ((,class :inherit org-level-6)))
   `(outline-7          ((,class :inherit org-level-6 :foreground ,heading-7)))
   `(outline-8          ((,class :inherit org-level-6 :foreground ,heading-8)))

;;; --- Vterm
   `(vterm-color-black            ((,class :background ,fg-comment :foreground ,fg-comment)))
   `(vterm-color-red              ((,class :background ,c-const :foreground ,c-const)))
   `(vterm-color-green            ((,class :background ,heading-4 :foreground ,heading-4)))
   `(vterm-color-yellow           ((,class :background ,heading-2 :foreground ,heading-2)))
   `(vterm-color-blue             ((,class :background ,heading-3 :foreground ,heading-3)))
   `(vterm-color-magenta          ((,class :background ,c-builtin :foreground ,c-builtin)))
   `(vterm-color-cyan             ((,class :background ,c-keyword :foreground ,c-keyword)))
   `(vterm-color-white            ((,class :background ,fg-dim :foreground ,fg-main)))
   `(vterm-color-default          ((,class :background ,bg-main :foreground ,fg-main)))
   `(vterm-color-inverse-video    ((,class :background ,bg-main :inverse-video t)))
   `(vterm-color-underline        ((,class :underline t)))

;;; --- eat
   `(eat-term-color-0             ((,class :background ,fg-comment :foreground ,fg-comment)))
   `(eat-term-color-1             ((,class :background ,c-const :foreground ,c-const)))
   `(eat-term-color-2             ((,class :background ,heading-4 :foreground ,heading-4)))
   `(eat-term-color-3             ((,class :background ,heading-2 :foreground ,heading-2)))
   `(eat-term-color-4             ((,class :background ,heading-3 :foreground ,heading-3)))
   `(eat-term-color-5             ((,class :background ,c-builtin :foreground ,c-builtin)))
   `(eat-term-color-6             ((,class :background ,c-keyword :foreground ,c-keyword)))
   `(eat-term-color-7             ((,class :background ,fg-dim :foreground ,fg-main)))
   `(eat-term-color-8             ((,class :background ,fg-comment :foreground ,fg-comment)))
   `(eat-term-color-9             ((,class :background ,c-const :foreground ,c-const)))
   `(eat-term-color-10            ((,class :background ,heading-4 :foreground ,heading-4)))
   `(eat-term-color-11            ((,class :background ,heading-2 :foreground ,heading-2)))
   `(eat-term-color-12            ((,class :background ,heading-3 :foreground ,heading-3)))
   `(eat-term-color-13            ((,class :background ,c-builtin :foreground ,c-builtin)))
   `(eat-term-color-14            ((,class :background ,c-keyword :foreground ,c-keyword)))
   `(eat-term-color-15            ((,class :background ,fg-dim :foreground ,fg-main)))

;;; --- eshell
   `(eshell-ls-archive            ((,class :foreground ,c-const)))
   `(eshell-ls-backup             ((,class :inherit shadow)))
   `(eshell-ls-clutter            ((,class :inherit shadow)))
   `(eshell-ls-directory          ((,class :foreground ,heading-3)))
   `(eshell-ls-executable         ((,class :foreground ,heading-4)))
   `(eshell-ls-missing            ((,class :inherit error)))
   `(eshell-ls-product            ((,class :inherit shadow)))
   `(eshell-ls-readonly           ((,class :foreground ,link)))
   `(eshell-ls-special            ((,class :foreground ,c-regexc)))
   `(eshell-ls-symlink            ((,class :inherit dired-symlink)))
   `(eshell-ls-unreadable         ((,class :inherit shadow)))
   `(eshell-prompt                ((,class :foreground ,heading-1)))

   ;; --- Ement
   `(ement-room-fully-read-marker       ((,class :inherit success)))
   `(ement-room-membership              ((,class :inherit shadow)))
   `(ement-room-mention                 ((,class :inherit highlight :foreground ,blue-5)))
   `(ement-room-name                    ((,class :inherit bold :foreground ,title)))
   `(ement-room-reactions               ((,class :inherit shadow)))
   `(ement-room-read-receipt-marker     ((,class :inherit match)))
   `(ement-room-self                    ((,class :inherit bold :foreground ,heading-1)))
   `(ement-room-self-message            ((,class :foreground ,fg-comment)))
   `(ement-room-timestamp               ((,class :inherit shadow)))
   `(ement-room-timestamp-header        ((,class :inherit bold :foreground ,clock)))
   `(ement-room-user                    ((,class :inherit bold :foreground ,heading-3)))

;;; --- nano modeline
   `(nano-modeline-active-status-RW     ((,class :inherit region :foreground ,fg-main)))
   `(nano-modeline-active-status-**     ((,class :inherit region :foreground ,fg-dim)))
   `(nano-modeline-active-status-RO     ((,class :inherit region :foreground ,c-string)))
   `(nano-modeline-active               ((,class :inherit mode-line)))
   `(nano-modeline-active-primary       ((,class :inherit mode-line :foreground ,title)))
   `(nano-modeline-active-name          ((,class :inherit mode-line :foreground ,heading-4)))

;;; --- magit
   `(magit-bisect-bad                ((,class :inherit error)))
   `(magit-bisect-good               ((,class :inherit success)))
   `(magit-bisect-skip               ((,class :inherit warning)))
   `(magit-blame-date                (( )))
   `(magit-blame-dimmed              ((,class :inherit shadow)))
   `(magit-blame-hash                (( )))
   `(magit-blame-highlight           ((,class :background ,bg-main :foreground ,fg-main)))
   `(magit-blame-name                (( )))
   `(magit-blame-summary             ((  )))
   `(magit-branch-local              ((,class :foreground ,c-property)))
   `(magit-branch-remote             ((,class :foreground ,c-warning)))
   `(magit-branch-upstream           ((,class :inherit italic)))
   `(magit-branch-warning            ((,class :inherit warning)))
   `(magit-cherry-equivalent         ((,class )))
   `(magit-cherry-unmatched          ((,class )))
   `(magit-diff-added                ((,class :background ,bg-added-4 :foreground ,fg-added)))
   `(magit-diff-added-highlight      ((,class :background ,bg-added :foreground ,fg-added)))
   `(magit-diff-base                 ((,class :background ,bg-changed-4 :foreground ,fg-changed)))
   `(magit-diff-base-highlight       ((,class :background ,bg-changed :foreground ,fg-changed)))
   `(magit-diff-context              ((,class :inherit shadow)))
   `(magit-diff-context-highlight          ((,class :background ,bg-diff-context)))
   `(magit-diff-file-heading               ((,class :inherit bold :foreground ,heading-2)))
   `(magit-diff-file-heading-highlight     ((,class :inherit magit-diff-file-heading :background ,bg-dim)))
   `(magit-diff-file-heading-selection     ((,class :inherit bold)))
   `(magit-diff-hunk-heading               ((,class :background ,bg-inactive)))
   `(magit-diff-hunk-heading-highlight     ((,class :inherit bold )))
   `(magit-diff-hunk-heading-selection     ((,class :inherit bold )))
   `(magit-diff-hunk-region                ((,class :inherit bold)))
   `(magit-diff-lines-boundary             ((,class :background ,fg-main)))
   `(magit-diff-lines-heading              ((,class :background ,fg-dim :foreground ,bg-main)))
   `(magit-diff-removed                    ((,class :background ,bg-removed-4 :foreground ,fg-removed)))
   `(magit-diff-removed-highlight          ((,class :background ,bg-removed :foreground ,fg-removed)))
   `(magit-diffstat-added                  ((,class :foreground ,fg-added-5)))
   `(magit-diffstat-removed                ((,class :foreground ,fg-removed-5)))
   `(magit-dimmed                          ((,class :inherit shadow)))
   `(magit-filename                        ((,class :foreground ,title)))
   `(magit-hash                            ((,class :foreground ,c-const)))
   `(magit-head                            ((,class :inherit magit-branch-local)))
   `(magit-header-line                     ((,class :inherit bold)))
   `(magit-header-line-key                 ((,class :inherit help-key-binding)))
   `(magit-header-line-log-select          ((,class :inherit bold)))
   `(magit-keyword                         ((,class :foreground ,c-keyword)))
   `(magit-keyword-squash                  ((,class :inherit bold :foreground ,c-warning)))
   `(magit-log-author                      ((,class :inherit message-header-name)))
   `(magit-log-date                        ((,class :foreground ,clock)))
   `(magit-log-graph                       ((,class :inherit shadow)))
   `(magit-mode-line-process               ((,class :inherit bold )))
   `(magit-mode-line-process-error         ((,class :inherit bold :foreground ,error)))
   `(magit-process-ng                      ((,class :inherit error)))
   `(magit-process-ok                      ((,class :inherit success)))
   `(magit-reflog-amend                    ((,class :inherit warning)))
   `(magit-reflog-checkout                 ((,class :inherit bold :foreground ,blue-5)))
   `(magit-reflog-cherry-pick              ((,class :inherit success)))
   `(magit-reflog-commit                   ((,class :inherit bold)))
   `(magit-reflog-merge                    ((,class :inherit success)))
   `(magit-reflog-other                    ((,class :inherit bold :foreground ,c-var)))
   `(magit-reflog-rebase                   ((,class :inherit bold :foreground ,yellow-5)))
   `(magit-reflog-remote                   ((,class :inherit (bold magit-branch-remote))))
   `(magit-reflog-reset                    ((,class :inherit error)))
   `(magit-refname                         ((,class :inherit shadow)))
   `(magit-refname-pullreq                 ((,class :inherit shadow)))
   `(magit-refname-stash                   ((,class :inherit shadow)))
   `(magit-refname-wip                     ((,class :inherit shadow)))
   `(magit-section                         ((,class :background ,bg-dim :foreground ,fg-main)))
   `(magit-section-heading                 ((,class :inherit bold)))
   `(magit-section-heading-selection       ((,class :inherit bold )))
   `(magit-section-highlight               ((,class :background ,bg-dim)))
   `(magit-sequence-done                   ((,class :inherit success)))
   `(magit-sequence-drop                   ((,class :inherit error)))
   `(magit-sequence-exec                   ((,class :inherit bold )))
   `(magit-sequence-head                   ((,class :inherit bold )))
   `(magit-sequence-onto                   ((,class :inherit (bold shadow))))
   `(magit-sequence-part                   ((,class :inherit warning)))
   `(magit-sequence-pick                   ((,class :inherit bold)))
   `(magit-sequence-stop                   ((,class :inherit error)))
   `(magit-signature-bad                   ((,class :inherit error)))
   `(magit-signature-error                 ((,class :inherit error)))
   `(magit-signature-expired               ((,class :inherit warning)))
   `(magit-signature-expired-key           ((,class :foreground ,c-warning)))
   `(magit-signature-good                  ((,class :inherit success)))
   `(magit-signature-revoked               ((,class :inherit bold :foreground ,c-warning)))
   `(magit-signature-untrusted             ((,class :inherit (bold shadow))))
   `(magit-tag                             ((,class )))

;;; --- Transient
   `(transient-key                         ((,class :inherit help-key-binding)))

;;; --- Markdown
   `(markdown-blockquote-face              ((,class :inherit org-verbatim :weight medium)))
   `(markdown-bold-face                    ((,class :inherit bold)))
   `(markdown-code-face                    ((,class :inherit org-code)))
   `(markdown-comment-face                 ((,class :inherit font-lock-comment-face)))
   `(markdown-footnote-marker-face         ((,class :inherit org-meta-line)))
   `(markdown-footnote-text-face           ((,class :inherit org-meta-line)))
   `(markdown-gfm-checkbox-face            ((,class :inherit org-checkbox)))
   `(markdown-header-delimiter-face        ((,class :foreground ,fg-dim)))
   `(markdown-header-face                  ((,class :inherit org-document-title)))
   `(markdown-header-face-1                ((,class :inherit org-level-1)))
   `(markdown-header-face-2                ((,class :inherit org-level-2)))
   `(markdown-header-face-3                ((,class :inherit org-level-3)))
   `(markdown-header-face-4                ((,class :inherit org-level-4)))
   `(markdown-header-face-5                ((,class :inherit org-level-5)))
   `(markdown-header-face-6                ((,class :inherit org-level-6)))
   `(markdown-header-rule-face             ((,class :foreground ,fg-dim)))
   `(markdown-highlight-face               ((,class :inherit highlight)))
   `(markdown-hr-face                      ((,class :foreground ,fg-inactive)))
   `(markdown-html-attr-name-face          ((,class :foreground ,fg-main)))
   `(markdown-html-attr-value-face         ((,class :foreground ,fg-main)))
   `(markdown-html-entity-face             ((,class :foreground ,fg-main)))
   `(markdown-html-tag-delimiter-face      ((,class :foreground ,fg-main)))
   `(markdown-html-tag-name-face           ((,class :foreground ,fg-main)))
   `(markdown-inline-code-face             ((,class :inherit org-code)))
   `(markdown-italic-face                  ((,class :inherit italic)))
   `(markdown-language-info-face           ((,class :foreground ,c-string)))
   `(markdown-language-keyword-face        ((,class :foreground ,cursor)))
   `(markdown-line-break-face              ((,class :foreground ,fg-inactive)))
   `(markdown-link-face                    ((,class :inherit org-link)))
   `(markdown-link-title-face              ((,class :inherit markdown-link-face :foreground ,c-warning)))
   `(markdown-list-face                    ((,class :inherit shadow)))
   `(markdown-markup-face                  ((,class :inherit org-meta-line)))
   `(markdown-math-face                    ((,class :foreground ,fg-main)))
   `(markdown-metadata-key-face            ((,class :foreground ,c-keyword)))
   `(markdown-metadata-value-face          ((,class :foreground ,c-keyword)))
   `(markdown-missing-link-face            ((,class :foreground ,fg-main)))
   `(markdown-plain-url-face               ((,class :inherit link)))
   `(markdown-pre-face                     ((,class )))
   `(markdown-reference-face               ((,class :foreground ,yellow-5)))
   `(markdown-strike-through-face          ((,class :strike-through t)))
   `(markdown-table-face                   ((,class :inherit org-table)))
   `(markdown-url-face                     ((,class :inherit org-link)))

;;; --- Meow (meow > evil)
   `(meow-beacon-indicator                   ((,class :height 1.0 :weight ultra-bold :foreground ,heading-5)))
   `(meow-beacon-cursor                      ((,class :background ,heading-5)))
   `(meow-insert-indicator                   ((,class :weight ultra-bold :foreground ,heading-4)))
   `(meow-insert-cursor                      ((,class :background ,heading-4)))
   `(meow-keypad-indicator                   ((,class :weight ultra-bold :foreground ,heading-3)))
   `(meow-keypad-cursor                      ((,class :background ,heading-3)))
   `(meow-motion-indicator                   ((,class :weight ultra-bold :foreground ,heading-2)))
   `(meow-motion-cursor                      ((,class :background ,heading-2)))
   `(meow-normal-indicator                   ((,class :weight ultra-bold :foreground ,title)))
   `(meow-normal-cursor                      ((,class :background ,title)))
   `(meow-search-indicator                   ((,class :foreground ,c-regexb)))
   `(meow-search-highlight                   ((,class :inherit jinx-highlight)))
   `(meow-position-highlight-number          ((,class :foreground ,cursor)))
   `(meow-position-highlight-number-1        ((,class :inherit meow-position-highlight-number)))
   `(meow-position-highlight-number-2        ((,class :inherit meow-position-highlight-number)))
   `(meow-cheatsheet-command                 ((,class :inherit (fixed-pitch bold) :height 0.8 :foreground ,c-warning)))
   `(meow-cheatsheet-highlight               ((,class :inherit (fixed-pitch) :height 0.7 :foreground ,c-var)))

;;; --- evil (just modeline)
   `(doom-modeline-evil-emacs-state      ((,class :inherit org-verbatim)))
   `(doom-modeline-evil-insert-state     ((,class :inherit meow-insert-indicator)))
   `(doom-modeline-evil-normal-state     ((,class :inherit meow-normal-indicator)))
   `(doom-modeline-evil-visual-state     ((,class :inherit meow-normal-indicator :foreground ,c-string)))
   `(doom-modeline-evil-operator-state   ((,class :inherit meow-beacon-indicator)))
   `(doom-modeline-evil-motion-state     ((,class :inherit meow-motion-indicator)))
   `(doom-modeline-evil-replace-state    ((,class :inherit meow-normal-indicator :foreground ,c-var)))

;;; --- Avy
   `(avy-background-face                 ((,class :background ,bg-main :foreground ,fg-comment :extend t)))
   `(avy-goto-char-timer-face            ((,class :inherit bold :background ,bg-inactive :foreground ,clock)))
   `(avy-lead-face                       ((,class :weight ultra-bold :foreground ,fg-main :background ,haki-region)))
   `(avy-lead-face-0                     ((,class :inherit avy-lead-face :background ,heading-8)))
   `(avy-lead-face-1                     ((,class :inherit avy-lead-face :background ,c-regexc)))
   `(avy-lead-face-2                     ((,class :inherit (avy-lead-face) :background ,c-builtin)))

;;; --- Man
   `(Man-overstrike                      ((,class :inherit bold :foreground ,heading-1)))
   `(Man-underline                       ((,class :foreground ,c-string :underline t)))
   `(Man-reverse                         ((,class :foreground ,c-warning)))
   `(woman-addition                      ((,class :foreground ,verbatim)))
   `(woman-bold                          ((,class :inherit bold :foreground ,heading-1)))
   `(woman-italic                        ((,class :inherit italic :foreground ,heading-3)))
   `(woman-unknown                       ((,class :foreground ,c-regexc)))

;;; --- tab-bar
   `(tab-bar                             ((,class :background ,bg-main)))
   `(tab-bar-tab-group-current           ((,class :box (:line-width (2 . -2) :color "gray50"))))
   `(tab-bar-tab-group-inactive          ((,class :box (:line-width (2 . -2) :color "gray50"))))
   `(tab-bar-tab                         ((,class :inherit region)))
   `(tab-bar-tab-inactive                ((,class :inherit mode-line-inactive)))
   `(tab-line                            ((,class :background ,bg-main)))

;;; --- centaur-tabs
   `(centaur-tabs-active-bar-face               ((,class :background ,bg-inactive)))
   `(centaur-tabs-close-mouse-face              ((,class :inherit bold :background ,error :box (:color ,cursor :line-width (1 . -3)))))
   `(centaur-tabs-close-selected                ((,class :inherit centaur-tabs-selected)))
   `(centaur-tabs-close-unselected              ((,class :inherit centaur-tabs-unselected)))
   `(centaur-tabs-modified-marker-selected      ((,class :inherit centaur-tabs-selected)))
   `(centaur-tabs-modified-marker-unselected    ((,class :inherit centaur-tabs-unselected)))
   `(centaur-tabs-default                       ((,class :background ,bg-main)))
   `(centaur-tabs-selected                      ((,class :inherit region)))
   `(centaur-tabs-selected-modified             ((,class :inherit (italic centaur-tabs-selected))))
   `(centaur-tabs-unselected                    ((,class :background ,bg-dim :foreground ,fg-comment)))
   `(centaur-tabs-unselected-modified           ((,class :inherit (italic centaur-tabs-unselected))))

;;; --- solaire-mode
   `(solaire-default-face                       ((,class :inherit default :background ,bg-dim)))
   `(solaire-hl-line-face                       ((,class :inherit hl-line :background ,bg-inactive :extend t)))
   `(solaire-org-hide-face                      ((,class :inherit org-hide :foreground ,bg-main)))
   `(solaire-fringe-face                        ((,class :inherit fringe :background ,bg-dim)))
   `(solaire-region-face                        ((,class :inherit region)))
   `(solaire-mode-line-face                     ((,class :foreground ,fg-dim)))
   `(solaire-header-line-face                   ((,class )))
   `(solaire-line-number-face                   ((,class :inherit line-number :background ,bg-dim :foreground ,fg-inactive)))

;;; --- vundo
   `(vundo-highlight                            ((,class :inherit (bold vundo-nodeatom) :foreground ,link)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'haki)
;;; haki-theme.el ends here
