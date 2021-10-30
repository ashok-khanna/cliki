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

;;; unreferenced uri checking
(dolist (unreferenced-uri (set-difference %referenced-uris %defined-uris
                                          :key #'car :test #'string-equal))
  (warn "Reference warning: referencing unknown URI resource ~a in file ~a"
        (car unreferenced-uri) (cdr unreferenced-uri)))

(defvar *cliki-server* nil)

(defun start-cliki-server (port homedir wikis)
  (if *cliki-server*
      (progn (warn "CLiki server already running")
             *cliki-server*)
      (progn
       ;; SBCL, CCL and possibly others always start w/same pseudo-random seed
        (setf *random-state* (make-random-state t))

        ;; set up HyperSpec paths
        (setf clhs-lookup::*hyperspec-pathname*
              (merge-pathnames "HyperSpec/" homedir)
              clhs-lookup::*hyperspec-map-file*
              (merge-pathnames "HyperSpec/Data/Symbol-Table.text" homedir)
              clhs-lookup::*hyperspec-root* "/site/HyperSpec/")

        (bt:make-thread
         (lambda ()
           (loop (map nil (lambda (x)
                            (expire-old-sessions (cadr x)))
                      wikis)
                 (sleep (* 60 60 24)))))

        (let ((error-log  (merge-pathnames "error-log" homedir))
              (access-log (merge-pathnames "access-log" homedir)))
          (open error-log  :direction :probe :if-does-not-exist :create)
          (open access-log :direction :probe :if-does-not-exist :create)
          (let ((acceptor
                 (make-instance
                  'cliki2-acceptor
                  :port                     port
                  :access-log-destination   access-log
                  :message-log-destination  error-log
                  :wikis                    wikis
                  :dispatch-table
                  (list
                   (wiki-static-dispatcher)
                   (create-folder-dispatcher-and-handler
                    "/site/HyperSpec/" (merge-pathnames #p"HyperSpec/" homedir))
                   (create-static-file-dispatcher-and-handler
                    "/site/error-log" error-log "text/plain")
                   'dispatch-easy-handlers
                   'article-dispatcher))))
            (start acceptor)
            (setf *cliki-server* acceptor))))))
