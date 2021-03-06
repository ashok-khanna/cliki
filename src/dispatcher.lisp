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

(defun guess-article-name ()
  (subseq (url-decode (script-name*)) 1))

(defun show-deleted-article-page (article)
  (setf (return-code*)
        404
        *footer*
        (with-output-to-string (*html-stream*)
          #H[<li><a href="$(#/site/history?article={ (article-title article) })">History</a></li>]))
  #H[Article was deleted])

(defun render-article (article
                       &optional (page-title (article-title article)))
  (let ((*header* #?[<link rel="alternate" type="application/atom+xml" title="ATOM feed of edits to current article"
                  href="$(#/site/feed/article.atom?title={ (article-title article) })">]))
    (render-page page-title
      (if (deleted? article)
          (show-deleted-article-page article)
          (progn
            #H[<div id="article-title">${ (article-title article) }</div>]
            (render-revision (latest-revision article)
                             (cached-content (article-title article))))))))

(defun article-dispatcher (request)
  (lambda ()
    (let ((article (find-article (guess-article-name))))
      (cond
        ((not article)
         (setf (return-code*) 404)
         (render-page "Article not found"
           #H[<h1>Article not found</h1>
           <a href="$(#/site/edit-article?title={(guess-article-name)})">Create</a>]))
        ((get-parameter "download" request)
         (redirect (elt
                    (nth-value
                     1
                     (ppcre:scan-to-strings
                      #?/_P\((.*?)\)/ (cached-content (article-title article))))
                    0)))
        (t (render-article article))))))

(defmethod acceptor-status-message :around ((acceptor cliki2-acceptor)
                                            status-code &key &allow-other-keys)
  (unless (and (equal status-code 404) (not (boundp '*wiki*)))
    (call-next-method)))

(define-easy-handler (root :uri "/") ()
  (render-article (find-article "index") (description *wiki*)))

(%defpage /robots.txt :both ()
  "User-agent: *
Disallow: /site/")

(defun wiki-static-dispatcher ()
  (create-prefix-dispatcher
   "/static/"
   (lambda ()
     (let ((request-path (request-pathname *request* "/static/")))
       (setf (header-out :cache-control) "max-age=31536000")
       (handle-static-file
        (merge-pathnames request-path (wiki-path "static/")))))))
