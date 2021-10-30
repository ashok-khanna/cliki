;;; Copyright 2011 Andrey Moskvitin <archimag@gmail.com>
;;; Copyright 2012, 2019 Vladimir Sedach <vsedach@common-lisp.net>

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

(defvar *wiki*)
(defvar *account* nil)

(defclass cliki2-acceptor (easy-acceptor)
  ((dispatch-table :reader dispatch-table :initarg :dispatch-table)
   (wikis          :reader wikis          :initarg :wikis)))

(defmethod acceptor-dispatch-request ((acceptor cliki2-acceptor) request)
  (let* ((host   (subseq (host) 0 (or (position #\: (host)) (length (host)))))
         (*wiki* (cadr (assoc host (wikis acceptor) :test #'string=))))
    (setf (header-out "Content-Security-Policy")
          "default-src 'none'; style-src 'self' 'unsafe-inline'; img-src *; form-action 'self';")
    (if *wiki*
        (let ((*account* (account-auth)))
          (loop for dispatcher in (dispatch-table acceptor)
                for action = (funcall dispatcher request)
                when action return (funcall action)
                finally (call-next-method)))
        (call-next-method))))
