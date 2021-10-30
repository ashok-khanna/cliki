;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2011, 2012 Vladimir Sedach <vsedach@common-lisp.net>
;;; Copyright 2012 Goheeca

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

(sanitize:define-sanitize-mode +links-only+
    :elements ("a")
    :attributes (("a" . ("href" "class")))
    :protocols  (("a" . (("href" . (:ftp :http :https :mailto :relative))))))

(sanitize:define-sanitize-mode +cliki-tags+
    :elements ("a" "blockquote" "q" "dd" "dl" "dt" "h1" "h2" "h3" "h4" "h5"
               "h6" "hgroup" "pre" "code" "kbd" "samp" "cite" "var" "time"
               "figure" "figcaption" "img" "table" "caption" "tbody" "td"
               "tfoot" "th" "thead" "tr" "col" "colgroup" "ul" "ol" "li" "b"
               "em" "i" "small" "big" "tt" "strike" "strong" "dfn" "s" "sub"
               "sup" "u" "abbr" "ruby" "rp" "rt" "bdo" "mark" "br" "hr")
    :attributes ((:all         . ("dir" "lang" "title" "class"))
                 ("a"          . ("href"))
                 ("abbr"       . ("title"))
                 ("bdo"        . ("dir"))
                 ("blockquote" . ("cite"))
                 ("col"        . ("span" "width" "align" "valign"))
                 ("colgroup"   . ("span" "width" "align" "valign"))
                 ("img"        . ("align" "alt" "height" "src" "width"))
                 ("ol"         . ("start" "reversed" "type"))
                 ("ul"         . ("type"))
                 ("code"       . ("lang"))
                 ("q"          . ("cite"))
                 ("table"      . ("summary" "width"))
                 ("td"         . ("abbr" "axis" "colspan" "rowspan" "width"))
                 ("th"         . ("abbr" "axis" "colspan" "rowspan" "scope" "width")))

    :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
                ("blockquote"  . (("cite" . (:http :https :relative))))
                ("img"         . (("src"  . (:http :https :relative))))
                ("q"           . (("cite" . (:http :https :relative))))))

(defun generate-html-from-markup (markup)
  #H[<div id="article">]
  (let ((start 0)
        tag-start
        close-tag)
      (labels ((find-tag (tag start)
                 (search tag markup :start2 start :test #'string-equal))
               (find-next-tag (start)
                 (let ((next-pre  (find-tag "<pre>" start))
                       (next-code (find-tag "<code" start))
                       min)
                   (when next-pre
                     (setf close-tag "</pre>"
                           min       next-pre))
                   (when next-code
                     (unless (and next-pre (< next-pre next-code))
                       (setf close-tag "</code>"
                             min       next-code)))
                   min)))
        (loop while (setf tag-start (find-next-tag start))
              do (write-string (parse-markup-fragment markup start tag-start)
                               *html-stream*)
                 (setf start (+ (length close-tag)
                                (or (find-tag close-tag tag-start)
                                    (return))))
                 (write-string (funcall (if (equal close-tag "</pre>")
                                            #'escape-pre-block
                                            #'markup-code)
                                        markup tag-start start)
                               *html-stream*)))
      (write-string (parse-markup-fragment markup start (length markup))
                    *html-stream*))
  #H[</div>])

(defun parse-markup-fragment (markup start end)
  (ppcre:regex-replace-all
   "\\n\\n"
   (sanitize:clean
    (ppcre:regex-replace-all
     "< "
     (parse-cliki-markup
      (escape-parens-in-href-links markup start end))
     "&lt; ")
    +cliki-tags+)
   "<p>"))

(defun escape-pre-block (markup start end)
  (ppcre:regex-replace
     "<(?:PRE|pre)>((?:.|\\n)*?)</(?:PRE|pre)>" markup
     (lambda (match preformatted)
       (declare (ignore match))
       #?[<pre>${(escape-for-html preformatted)}</pre>])
     :simple-calls t :start start :end end))

(defun escape-parens-in-href-links (markup start end)
  (ppcre:regex-replace-all
    "(?:href|HREF)=\"[^\"]*\""
    markup
    (lambda (match)
      (with-output-to-string (s)
        (loop for c across match do
          (princ (case c
                   (#\( "%28")
                   (#\) "%29")
                   (t   c))
                 s))))
    :simple-calls t :start start :end end))

(defun parse-cliki-markup (markup)
  (ppcre:regex-replace-all
   "(_|\\*|/|_H|_P)\\(((?:\\\\\\)|[^\\)])*)\\)"
   markup
   (lambda (match prefix title)
     (declare (ignore match))
     (with-output-to-string (*html-stream*)
       (funcall (cdr
                 (assoc prefix
                        '(("_"  . pprint-article-underscore-link)
                          ("*"  . pprint-topic-link)
                          ("/"  . format-topic-list)
                          ("_H" . format-hyperspec-link)
                          ("_P" . format-package-link))
                        :test #'string=))
                (ppcre:regex-replace-all "\\\\\\)" title ")"))))
   :simple-calls t))

(defun pprint-article-underscore-link (title)
  (destructuring-bind (&optional (real-title "") link-title)
      (ppcre:split "\\|" title)
    (%print-article-link real-title "internal"
                         (or link-title real-title))))

(defun article-description (article-title)
  (let ((c (cached-content article-title)))
    (subseq c 0 (ppcre:scan "\\.(?:\\s|$)|\\n|$" c))))

(defun pprint-article-summary-li (article-title separator)
  #H[<li>] (pprint-article-link article-title) #H[ ${separator}
  ${(sanitize:clean
     (with-output-to-string (*html-stream*)
       (generate-html-from-markup (article-description article-title)))
     +links-only+)}
  </li>])

(defun format-topic-list (topic) ;; /(
  #H[<ul>] (dolist (article (articles-by-topic topic))
             (pprint-article-summary-li article "-"))
  #H[</ul>])

(defun format-hyperspec-link (symbol) ;; _H(
  #H[<a href="${(clhs-lookup:spec-lookup symbol)}" class="hyperspec">${symbol}</a>])

(defun format-package-link (link) ;; _P(
  #H[<a href="${link}">ASDF-install package (obsolete) ${link}</a>])

(let ((supported-langs (sort (mapcar (lambda (x)
                                       (symbol-name (car x)))
                                     colorize::*coloring-types*)
                             #'> :key #'length)))
  (defun markup-code (markup start end)
    (ppcre:regex-replace
     "<(?:CODE|code)(.*?)?>((?:.|\\n)*?)</(?:CODE|code)>" markup
     (lambda (match maybe-lang code)
       (declare (ignore match))
       (let ((lang (loop for lang in supported-langs
                         when (search lang maybe-lang :test #'char-equal)
                         return (find-symbol lang :keyword))))
         (if lang
             (let ((colorize::*css-background-class* "nonparen"))
	       #?[<div class="code">${(colorize::html-colorization lang code)}</div>])
             #?[<code>${(escape-for-html code)}</code>])))
     :simple-calls t :start start :end end)))
