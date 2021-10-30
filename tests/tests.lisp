;;; Copyright 2019 Vladimir Sedach <vsedach@common-lisp.net>

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


(in-package #:cliki2.tests)

(def-suite cliki2-tests)

(defvar *test-wiki*
  (cliki2::make-wiki
   "test" "" (ensure-directories-exist #P"/tmp/cliki2test/") ""))

(defun run-tests ()
  (let ((cliki2::*wiki* *test-wiki*))
    (run! 'cliki2-tests)))

(defmacro mtest (name expected markup)
  `(test ,name
       (is (string= ,expected
                    (cliki2::parse-cliki-markup ,markup)))))

(in-suite cliki2-tests)

(mtest parse-internal-link1
  "<a href=\"/Foo%20bar\" class=\"new\">Foo bar</a>"
  "_(Foo bar)")

(mtest parse-internal-link2
  "<a href=\"/Something%20else\" class=\"new\">Shortname</a>"
  "_(Something else|Shortname)")

(mtest parse-internal-link3
  "<a href=\"/Foo%20bar\" class=\"new\">Foo bar</a>)))"
  "_(Foo bar))))")

(mtest parse-empty-internal-link
  "<a href=\"/\" class=\"new\"></a>"
  "_()")

(mtest parse-empty-internal-link1
  "<a href=\"/\" class=\"new\"></a>"
  "_(|)")

(mtest parse-empty-internal-link2
  "<a href=\"/\" class=\"new\">x</a>"
  "_(|x)")

(mtest parse-hyperspec-link
  "<a href=\"NIL\" class=\"hyperspec\">setf</a>"
  "_H(setf)")

(mtest parse-topic-link
  "<a href=\"/fruit\" class=\"new\">fruit</a>"
  "*(fruit)")

(mtest parse-topic-list
  "<ul></ul>"
  "/(list of lists)")

(mtest parse-empty-topic-list
  "<ul></ul>"
  "/()")

(mtest bunch-of-links
  "<a href=\"/foo\" class=\"new\">foo</a> something <a href=\"/bar\" class=\"new\">bar</a> <a href='http://common-lisp.net'>somewhere</a>
 <ul></ul>"
  "_(foo) something *(bar) <a href='http://common-lisp.net'>somewhere</a>
 /(baz)")

(test escape-parens-in-href-links
  (is
   (string=
    "*(baz) <a href=\"foo %28bar%29\">foo (bar)</a> _(not bar)"
    (cliki2::escape-parens-in-href-links
     "*(baz) <a href=\"foo (bar)\">foo (bar)</a> _(not bar)" 0 51))))

(mtest parse-article-link-with-escaped-parentheses
  "<a href=\"/Foo%20%28bar%29\" class=\"new\">Foo (bar)</a>"
  "_(Foo (bar\\))")

(mtest parse-article-link-with-escaped-parentheses1
  "<a href=\"/Foo%20%28bar%29%20baz\" class=\"new\">Foo (bar) baz</a>"
  "_(Foo (bar\\) baz)")

(mtest parse-article-link-with-escaped-parentheses2
  "<a href=\"/Foo%20%28bar%29\" class=\"new\">Foo (bar)</a>)"
  "_(Foo (bar\\)))")

(mtest parse-empty-article
  ""
  "")

(mtest parse-short-article
  "a"
  "a")

(mtest parse-short-article1
  "ab"
  "ab")

(mtest parse-short-article2
  "abc"
  "abc")

(mtest parse-short-article3
  "("
  "(")

(mtest parse-short-article4
  "_("
  "_(")

(mtest parse-short-article5
  ")"
  ")")

(mtest parse-short-article6
  "()"
  "()")

(mtest parse-some-article
  "something <a href=\"/Foo%20bar\" class=\"new\">Foo bar</a> baz"
  "something _(Foo bar) baz")

(mtest parse-some-article1
  "(abc <a href=\"/Foo%20bar\" class=\"new\">Foo bar</a> xyz)"
  "(abc _(Foo bar) xyz)")

(mtest parse-some-article2
  "abc <a href=\"/Foo%20bar\" class=\"new\">Foo bar</a> (xyz"
  "abc _(Foo bar) (xyz")
