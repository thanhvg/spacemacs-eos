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
  '(
    (ipa :location (recipe
                         :fetcher github
                         :repo "thanhvg/ipa.el"))
    howdoyou
    hnreader)
  
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

(defun eos/init-howdoyou ()
  (use-package howdoyou
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "yy" #'howdoyou-query
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
        "yhb" #'hnreader-back
        "yha" #'hnreader-ask))))

(defun eos/init-ipa ()
  ;; (require 'ipa)
  (use-package ipa
    ;; with defer anotations won't show after restart
    ;; :defer t 
    :init
    (progn
      (add-hook 'find-file-hook 'ipa-mode)
      (setq ipa-overlay-position "above")
      (spacemacs/declare-prefix "an" "annotate")
      (spacemacs/declare-prefix "anE" "ipa-edit-above")
      (spacemacs/declare-prefix "anM" "ipa-move-above-anotation")
      (spacemacs/set-leader-keys
        "ani" #'ipa-insert
        "ane" #'ipa-edit
        "anE" (lambda () (interactive) (ipa-edit 4))
        "ans" #'ipa-show
        "anm" #'ipa-move
        "anM" (lambda () (interactive) (ipa-move 4))
        "ann" #'ipa-next
        "anN" #'ipa-previous
        "anp" #'ipa-previous
        "anr" #'ipa-refresh
        "anj" #'ipa-jump))))
;;; packages.el ends here
