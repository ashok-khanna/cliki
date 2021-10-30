;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2012 Vladimir Sedach <vsedach@common-lisp.net>
;;; Copyright 2012 Jianshi Huang <jianshi.huang@gmail.com>

;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public
;;; License as published by the Free Software Foundation, either
;;; version 3 of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Affero General Public License for more details.

;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package #:cliki2)
(in-readtable cliki2)

;;; topics and backlinks

(defun canonicalize (title)
  (string-downcase (cut-whitespace title)))

(defun collect-links (link-type content)
  (let (names)
    (ppcre:do-register-groups (name) (#?/${link-type}\(([^\)]*)\)/ content)
      (pushnew (canonicalize name) names :test #'string=))
    names))

(defun topics (content)
  (collect-links "\\*" content))

(defun page-links (content)
  (collect-links "\\_" content))

;;; full-text search

(defparameter *common-words*
  '("" "a" "also" "an" "and" "are" "as" "at" "be" "but" "by" "can" "for" "from"
    "has" "have" "here" "i" "if" "in" "is" "it" "not" "of" "on" "or" "s" "see"
    "so" "that" "the" "there" "this" "to" "us" "which" "with" "you"))

(defun words (content)
  (let (words)
    (dolist (word (ppcre:split "<.*?>|_|\\s|[^\\w]" (string-downcase content)))
      (unless (member word *common-words* :test #'string=)
        (pushnew (stem:stem word) words :test #'string=)))
    words))

(defun search-articles (phrase)
  (let ((words (words phrase)))
    (when words
      (sort (copy-list (reduce (lambda (&optional a b)
                                 (intersection a b :test #'string=))
                               (mapcar #'articles-by-search-word words)))
            #'< :key (lambda (title)
                       (let ((title (canonicalize title)))
                        (loop for word in words
                              for weight from 0 by 100
                              thereis (awhen (search word title)
                                        (+ (* it 100) weight (length title)))
                              finally (return most-positive-fixnum))))))))

(defun paginate-article-summaries (start articles &optional (next-page-uri "?"))
  (let ((page-size 10)
        (start (or (parse-integer (or start "0") :junk-allowed t) 0)))
    (flet ((page-uri (page# label)
             #H[<span><a href="${next-page-uri}&start=${(* page# page-size)}">${label}</a></span>]))

      #H[<ol start="${(1+ start)}">]
      (loop for i from start below (min (+ start page-size) (length articles))
            do (pprint-article-summary-li (elt articles i) "<br />"))
      #H[</ol>
      <div id="paginator">
      <span>Result page:</span>]
      (unless (= 0 start)
        (page-uri (ceiling (- start page-size) page-size) "&lt;"))
      (dotimes (page# (ceiling (length articles) page-size))
        (if (= start (* page# page-size))
            #H[<span>${(1+ page#)}</span>]
            (page-uri page# (1+ page#))))
      (unless (>= (+ start page-size) (length articles))
        (page-uri (ceiling (+ start page-size) page-size) "&gt;"))
      #H[</div>])))

(defpage /site/search "Search results" (query start)
  #H[<h1>Search results</h1>]
  (aif (search-articles query)
       (paginate-article-summaries start it #U?query={query})
       #H[No results found]))
