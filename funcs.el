;;; funcs.el --- eos layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; fold-this functions

(defun fold-this-with-indent (beg end &optional header)
  "Refine beg to the fist non-white char and end to the last one.
So the indent is respected."
  (interactive "r")
  (fold-this (save-excursion
               (goto-char beg)
               (re-search-forward "[^ ]" end)
               (1- (point)))
             (save-excursion
               (goto-char end)
               (re-search-backward "[^ ]" beg)
               (point))
             header))

(defun fold-this-with-header (beg end)
  "Ask for header to put in the fold."
  (interactive "r")
  (fold-this-with-indent beg end (read-string "Header: ")))
