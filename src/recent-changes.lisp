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

(defun find-previous-revision (revision)
  (cadr (member revision (revisions
                          (find-article (parent-title revision) :error t)))))

(defun %render-revision-summary (revision)
  (pprint-revision-link revision)
  #H[ <a class="internal" href="${ (article-link (parent-title revision)) }">${ (escape-for-html (parent-title revision)) }</a>
  - ${ (escape-for-html (summary revision)) } ${ (account-link (author-name revision)) } ]
  (awhen (find-previous-revision revision)
    (output-compare-link it revision "diff")))

(defun render-revision-summary (revision)
  #H[<li>] (%render-revision-summary revision) #H[</li>])

(defpage /site/recent-changes "Recent Changes" ()
  (setf *header* #?[<link rel="alternate" type="application/atom+xml" title="recent changes" href="$(#/site/feed/recent-changes.atom)">])
  #H[<h1>Recent Changes</h1>
  <a class="internal" href="$(#/site/feed/recent-changes.atom)">ATOM feed</a>
  <ul>] (map nil #'render-revision-summary (get-recent-changes)) #H[</ul>])

;;; feed

(defun iso8601-time (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun feed-doc (title link updated entries-body)
  (setf (content-type*) "application/atom+xml")
  (with-output-to-string (*html-stream*)
    #H[<?xml version="1.0" encoding="utf-8"?>
    <feed xmlns="http://www.w3.org/2005/Atom">
      <title>${title}</title>
      <link href="${ (escape-for-html link) }" />
      <updated>${ (iso8601-time updated) }</updated>]
      (funcall entries-body)
    #H[</feed>]))

(defun feed-format-content (revision)
  (escape-for-html
   (with-output-to-string (*html-stream*)
     (%render-revision-summary revision)
     (render-diff-table (find-previous-revision revision) revision nil))))

(defun feed-present-revision (revision)
  #H[<entry>
  <title>${ (parent-title revision) } - ${ (escape-for-html (summary revision)) } ${ (author-name revision) }</title>
  <link href="${ (escape-for-html (revision-link revision)) }" type="text/html" />
  <updated>${ (iso8601-time (revision-date revision)) }</updated>
  <content type="html">${ (feed-format-content revision) }</content>
</entry>])

(%defpage /site/feed/recent-changes.atom :get ()
  (feed-doc
   #?"${(wiki-name *wiki*)} Recent Changes" #/site/feed/recent-changes.atom
   (revision-date (car (get-recent-changes)))
   (lambda ()
     (map nil #'feed-present-revision (get-recent-changes)))))

(%defpage /site/feed/article.atom :get (title)
  (let ((article (find-article title :error t)))
    (feed-doc
     #?"${(wiki-name *wiki*)} Article ${ (escape-for-html title) } Edits"
     #/site/feed/article.atom?title={title}
     (revision-date (latest-revision article))
     (lambda ()
       (loop repeat 20 for revision in (revisions article)
             do (feed-present-revision revision))))))
