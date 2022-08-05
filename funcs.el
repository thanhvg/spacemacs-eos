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

;; (defun spacemacs--advice-virtual-comment (origfunc &rest args)
;;   "Enable evil for ORIGFUNC."
;;   (let ((evil-want-minibuffer t))
;;     (apply origfunc args)))

(defun eos-company-dabbrev ()
  "Use company-dabbrev."
  (interactive)
  (let ((company-backends '(company-dabbrev)))
    (company-complete)))

(defun eos-toggle-company-default ()
  (interactive)
  (unless company-mode
    (company-mode))
  (if eos-original-company-backends
      (progn
        (setq-local company-backends eos-original-company-backends)
        (setq eos-original-company-backends nil))
    (setq eos-original-company-backends company-backends)
    (setq-local company-backends spacemacs-default-company-backends)))

(defun spacemacs/elfeed-view-with-declutter ()
  "Open current feed with `declutter'."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Sent to declutter: %s" link)
      (declutter link))))

(defun spacemacs/pocket-view-with-declutter ()
  "Open current entry with `declutter'."
  (interactive)
  (when-let ((id (tabulated-list-get-id))
             (item (ht-get pocket-reader-items id))
             (url (pocket-reader--get-url item)))
    (declutter url)
    (message url)))

(defun spacemacs//get-url-under-point ()
  "Try to figure out is there any URL under point.
Returns nil if none. Code from `decluter'"
  (let ((url (get-text-property (point) 'shr-url)))
    (if url
        url
      (thing-at-point 'url))))

(defun spacemacs/view-url-at-point (&optional arg)
  "dispatch current url at point to appropriate handlers.
When ARG then use `eww'."
  (interactive "p")
  (when-let* ((url (spacemacs//get-url-under-point))
              (host (url-host (url-generic-parse-url url))))
    (message url)
    (cond
     ((string-match-p "news.ycombinator.com" host)
      (require 'hnreader)
      (hnreader-promise-comment url))
     ((string-match-p ".*stackoverflow.com" host)
      (require 'howdoyou)
      (howdoyou-read-so-link url)
      (pop-to-buffer (howdoyou--get-buffer)))
     ((string-match-p ".reddit.com" host)
      (require 'reddigg)
      (reddigg-view-comments url))
     ((eql arg 4) (eww url))
     (t
      (require 'declutter)
      (declutter-url url)))))

(defun spacemacs/declutter-current-eww ()
  (interactive)
  (declutter-url (plist-get eww-data :url)))
