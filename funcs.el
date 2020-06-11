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

(defun howdoyou-with-google-suggest ()
  "`howdoyou-query' with `google-suggest'."
  (interactive)
  (howdoyou-query (google-suggest)))

(defun google-google ()
  "Build query to search with google suggest."
  (interactive)
  (let* ((query (google-suggest))
         (url (format eos-google-suggest-search-url
                      (url-hexify-string query))))
    (browse-url url)))

(defun google-eww ()
  "Build query to search with google suggest."
  (interactive)
  (let* ((query (google-suggest))
         (url (format eos-google-suggest-search-url
                      (url-hexify-string query))))
    (eww url)))


(defun thanh-company-dabbrev ()
  "Use company-dabbrev."
  (interactive)
  (let ((company-backends '(company-dabbrev)))
    (company-complete)))
