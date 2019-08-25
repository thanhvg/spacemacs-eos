;;; packages.el --- eos layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@x230>
;; URL: https://github.com/syl20bnr/spacemacs
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
    (howdoyou :location (recipe
                         :fetcher github
                         :repo "thanhvg/emacs-howdoyou"))
    (hnreader :location (recipe
                         :fetcher github
                         :repo "thanhvg/emacs-hnreader"))
    )
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
      (spacemacs/set-leader-keys "yy" #'howdoyou-query)
      (spacemacs/set-leader-keys "yn" #'howdoyou-next-link)
      (spacemacs/set-leader-keys "yr" #'howdoyou-reload-link)
      (spacemacs/set-leader-keys "y1" #'howdoyou-go-back-to-first-link)
      (spacemacs/set-leader-keys "yp" #'howdoyou-previous-link))))

(defun eos/init-hnreader ()
  (use-package hnreader
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "yh" "hackernews")
      (spacemacs/set-leader-keys "yhn" #'hnreader-news)
      (spacemacs/set-leader-keys "yhp" #'hnreader-past)
      (spacemacs/set-leader-keys "yhm" #'hnreader-more)
      (spacemacs/set-leader-keys "yhs" #'hnreader-show)
      (spacemacs/set-leader-keys "yhb" #'hnreader-back)
      (spacemacs/set-leader-keys "yha" #'hnreader-ask))))
;;; packages.el ends here
