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

(defstruct session
  account
  expires-at
  password-digest)

(defun logout ()
  (with-lock-held ((session-lock *wiki*))
    (remhash (cookie-in "cliki2auth") (sessions *wiki*)))
  (set-cookie "cliki2auth" :value "" :path "/")
  nil)

(defun expire-old-sessions (wiki)
  (with-lock-held ((session-lock wiki))
    (loop for x being the hash-key of (sessions wiki)
          using (hash-value session) do
          (when (< (session-expires-at session) (get-universal-time))
            (remhash x (sessions wiki))))))

(defun next-expiry-time ()
  (+ (get-universal-time) (* 60 60 24 180)))

(defun login (account)
  (let (secret)
    (with-lock-held ((session-lock *wiki*))
      (loop while (gethash (setf secret (make-random-string 60))
                           (sessions *wiki*)))
      (setf (gethash secret (sessions *wiki*))
            (make-session :account         account
                          :expires-at      (next-expiry-time)
                          :password-digest (account-password-digest account))))
    (set-cookie "cliki2auth" :value secret :path "/" :expires (next-expiry-time))))

(defun account-auth ()
  (let* ((secret (cookie-in "cliki2auth")))
    (awhen (with-lock-held ((session-lock *wiki*))
             (gethash secret (sessions *wiki*)))
      (if (and (< (get-universal-time) (session-expires-at it))
               (string= (account-password-digest (session-account it))
                        (session-password-digest it)))
          (progn (setf (session-expires-at it) (next-expiry-time))
                 (session-account it))
          (logout)))))

;;; captcha

(defvar captcha-ops '(floor ceiling truncate round))

(defun make-captcha ()
  (list (elt captcha-ops (random (length captcha-ops)))
        (- (random 40) 20)
        (1+ (random 10))))

(defun emit-captcha-inputs (captcha class size)
  #H[<input class="${class}" name="captcha-answer" size="${size}" />
     <input type="hidden" name="captcha-op" value="${(elt captcha 0)}" />
     <input type="hidden" name="captcha-x"  value="${(elt captcha 1)}" />
     <input type="hidden" name="captcha-y"  value="${(elt captcha 2)}" />])

(defun check-captcha ()
  (let ((x      (parse-integer (parameter "captcha-x")      :junk-allowed t))
        (y      (parse-integer (parameter "captcha-y")      :junk-allowed t))
        (answer (parse-integer (parameter "captcha-answer") :junk-allowed t))
        (op     (find (parameter "captcha-op") captcha-ops
                      :key #'symbol-name :test #'string-equal)))
    (when (and op x y answer)
      (= (funcall op x y) answer))))
