;;; packages.el --- eos layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `eos-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `eos/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `eos/pre-init-PACKAGE' and/or
;;   `eos/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst eos-packages
  '((counsel-wordnut
     :requires ivy
     :location (recipe
                :fetcher github
                :repo "thanhvg/counsel-wordnut"))
    company
    fold-this
    (git-complete :location (recipe
                             :fetcher github
                             :repo "zk-phi/git-complete"))
    (google-suggest :location (recipe
                               :fetcher github
                               :repo "thanhvg/emacs-google-suggest"))
    (helm-wordnut
     :requires helm
     :location (recipe
                :fetcher github
                :repo "manuel-uberti/helm-wordnut"))
    howdoyou
    hnreader
    ;; (helm-wikipedia :location (recipe
    ;;                            :fetcher github
    ;;                            :repo "emacs-helm/helm-wikipedia"))

    virtual-comment
    ;; (virtual-comment :location (recipe
    ;;                             :fetcher github
    ;;                             :repo "thanhvg/emacs-virtual-comment"))

    ;; (reddigg :location (recipe
    ;;                             :fetcher github
    ;;                             :repo "thanhvg/emacs-reddigg"))
    reddigg)

  "The list of Lisp packages required by the eos layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun eos/init-counsel-wordnut ()
  (use-package counsel-wordnut
    :defer t
    :init
    (spacemacs/set-leader-keys "xww" #'counsel-wordnut)))

(defun eos/init-google-suggest ()
  (use-package google-suggest
    :defer t
    :init (progn
            (spacemacs/set-leader-keys
              "yG" #'google-eww
              "yg" #'google-google))))

(defun eos/init-howdoyou ()
  (use-package howdoyou
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "yY" #'howdoyou-query
        "yy" #'howdoyou-with-google-suggest
        "yn" #'howdoyou-next-link
        "yr" #'howdoyou-reload-link
        "y1" #'howdoyou-go-back-to-first-link
        "yp" #'howdoyou-previous-link))))

(defun eos/init-hnreader ()
  (use-package hnreader
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "yh" "hackernews")
      (spacemacs/set-leader-keys
        "yhn" #'hnreader-news
        "yhp" #'hnreader-past
        "yhm" #'hnreader-more
        "yhs" #'hnreader-show
        "yhc" #'hnreader-comment
        "yhi" #'hnreader-org-insert-hn-link
        "yhb" #'hnreader-back
        "yha" #'hnreader-ask))))

(defun eos/init-reddigg ()
  (use-package reddigg
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ya" "reddigg")
      (spacemacs/set-leader-keys
        "yaa" #'reddigg-view-main
        "yas" #'reddigg-view-sub
        "yac" #'reddigg-view-comments))))

(defun eos/init-virtual-comment ()
  (use-package virtual-comment
    :init
    (progn
      (add-hook 'find-file-hook 'virtual-comment-mode)
      (add-hook 'virtual-comment-show-mode 'outline-minor-mode)
      (spacemacs/declare-prefix "cv" "virtual-comments")
      (spacemacs/set-leader-keys
        "cvv" #'virtual-comment-make
        "cvd" #'virtual-comment-delete
        "cvs" #'virtual-comment-show
        "cvj" #'virtual-comment-next
        "cvn" #'virtual-comment-next
        "cvN" #'virtual-comment-previous
        "cvk" #'virtual-comment-previous
        "cvp" #'virtual-comment-paste
        "cve" #'virtual-comment-persist
        "cva" #'virtual-comment-add-location
        "cvo" #'virtual-comment-goto-location
        "cvy" #'virtual-comment-remember-current-location
        "cvr" #'virtual-comment-realign))
    :config
    ;; (evilified-state-evilify
    ;;   virtual-comment-show-mode virtual-comment-show-mode-map
    ;;   "q" 'quit-window)

    ;; (evilified-state-evilify-map virtual-comment-show-mode-map
    ;;   :mode virtual-comment-show-mode
    ;;   :bindings
    ;;   "q" 'quit-window)

    (evil-set-initial-state 'virtual-comment-show-mode-map 'normal)
    (evil-collection-define-key 'normal 'virtual-comment-show-mode-map
      (kbd "<backtab>") 'outline-show-all 
      (kbd "<tab>") 'outline-toggle-children
      "[[" 'outline-previous-visible-heading
      "]]" 'outline-next-visible-heading
      (kbd "C-k") 'outline-backward-same-level
      (kbd "C-j") 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gj" 'outline-forward-same-level
      "q" 'quit-window)

    ;; (advice-add 'virtual-comment-make
    ;;             :around #'spacemacs--advice-virtual-comment)
    ))

(defun eos/init-helm-wordnut ()
  (use-package helm-wordnut
    :defer t
    :init
    (spacemacs/set-leader-keys "xww" #'helm-wordnut)))

(defun eos/init-git-complete ()
  (use-package git-complete
    :bind ("C-c C-SPC" . git-complete)))

(defun eos/init-fold-this ()
  (use-package fold-this
    :defer t
    :init
    (spacemacs/set-leader-keys
      "zz" #'fold-this-with-indent
      "zh" #'fold-this-with-header)))

;; (defun eos/init-helm-wikipedia ()
;;   (use-package helm-wikipedia
;;     :defer t
;;     :init
;;     (spacemacs/set-leader-keys
;;       "yw" #'helm-wikipedia-suggest)))

(defun eos/post-init-company ()
  (spacemacs/set-leader-keys "t SPC" 'eos-toggle-company-default)
  (global-set-key (kbd "C-c SPC") 'eos-company-dabbrev))
;;; packages.el ends here
