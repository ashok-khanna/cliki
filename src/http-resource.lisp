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

(defmacro %defpage (page method parameters &body body)
  (let ((page-uri (symbol-name (if (listp page) (car page) page))))
    `(progn
       (pushnew (list ,page-uri) %defined-uris :test #'equal)
       (define-easy-handler (,(if (listp page) (cadr page) page) :uri ,(string-downcase page-uri) :default-request-type ,method) ,parameters
         ,@body))))

(defmacro defpage (page title parameters &body body)
  `(%defpage ,page :both ,parameters
     (render-page ,title ,@body)))

(defmacro defhandler (page parameters &body body)
  `(%defpage ,page :post ,(mapcar (lambda (p)
                                    (list p :request-type :both))
                                  parameters)
     (redirect (or (progn ,@body) "/") :code 303)))
