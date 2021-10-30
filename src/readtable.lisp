;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2011 Vladimir Sedach <vsedach@common-lisp.net>

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

(defparameter %defined-uris '(("/")))
(defparameter %referenced-uris ())

(defvar *html-stream* *standard-output*)

(defreadtable cliki2
  (:merge :standard uri-template:uri-template)
  (:case :invert)
  (:dispatch-macro-char
   #\# #\?
   #'cl-interpol::interpol-reader)
  (:dispatch-macro-char
   #\# #\H
   (lambda (&rest args)
     `(princ ,(apply #'cl-interpol::interpol-reader args) *html-stream*)))
  (:dispatch-macro-char
   #\# #\/
   (lambda (stream subchar arg)
     (declare (ignore subchar arg))
     (let ((uri-path
            (with-output-to-string (x)
              (princ #\/ x)
              (loop until (member (peek-char nil stream nil #\Space t)
                                  '(#\Space #\Newline #\Tab #\? #\) #\{))
                    do (princ (read-char stream) x)))))
       (pushnew (cons uri-path (or *compile-file-pathname* *load-pathname*))
                %referenced-uris :key #'car :test #'equal)
       `(concatenate 'string ,uri-path
                     ,@(uri-template:read-uri-template stream))))))
