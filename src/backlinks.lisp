;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2012 Vladimir Sedach <vsedach@common-lisp.net>

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

(defun print-link-list (links printer)
  #H[<ul>]
  (dolist (link links)
    #H[<li>] (funcall printer link))
  #H[</ul>])

(defpage /site/backlinks () (article)
  (let* ((article-obj (find-article article :error t))
         (content     (cached-content article)))
    (setf *title*  #?'Link information for "${ (article-title article-obj) }"'
          *footer* (article-footer (latest-revision article-obj)))

    #H[<h1>Link information for ] (pprint-article-link article) #H[</h1>
    Topics:] (print-link-list (topics content) #'pprint-topic-link)
    #H[Links to other articles:]
    (print-link-list (page-links content) #'pprint-article-link)
    #H[Links from other articles:]
    (print-link-list (article-backlinks article) #'pprint-article-link)
    #H[Articles in '${ article }' topic:]
    (print-link-list (articles-by-topic article) #'pprint-article-link)))
