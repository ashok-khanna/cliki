;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2011, 2012 Vladimir Sedach <vsedach@common-lisp.net>

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

(defpage /site/tools "Tools" ()
  #H[<h2>Tools</h2>
  <ul>
  <li><a href="$(#/site/all-articles)">All articles</a></li>
  <li><a href="$(#/site/all-topics)">All topics</a></li>
  <li><a href="$(#/site/uncategorized)">Uncategorized articles</a></li>
  <li><a href="$(#/site/deleted-articles)">Deleted articles</a></li>
  <li><a href="$(#/site/blacklist)">Blacklist of users/IPs</a></li>
  </ul>])

(defpage /site/blacklist "Blacklist" ()
  #H[<h3>Banned accounts/IPs</h3>
  <ul>]
  (dolist (banned (get-blacklist))
    #H[<li>${ (account-link banned) }</li>])
  #H[</ul>])

(defpage /site/all-articles "All articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles (complement #'deleted?))))

(defpage /site/deleted-articles "Deleted articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles #'deleted?)))

(defpage /site/uncategorized "Uncategorized articles" (start)
  (paginate-article-summaries
   start
   (get-all-articles (lambda (article)
                       (not (or (deleted? article)
                                (topics (cached-content
                                         (article-title article)))))))))

(defpage /site/all-topics "All topic markers" ()
  #H[<ul>]
  (dolist (topic (sort (all-topics) #'string-lessp))
    #H[<li>] (pprint-topic-link topic) #H[</li>])
  #H[</ul>])
