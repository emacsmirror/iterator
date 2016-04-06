;;; iterator.el --- A library to create and use elisp iterators objects. -*- lexical-binding: t -*-

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>

;; Copyright (C) 2009 ~ 2014 Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 24.1+
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; X-URL: https://github.com/thierryvolpiatto/iterator

;; This file is not part of GNU Emacs. 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Code:

(require 'cl-lib)

(cl-defsubst iterator:position (item seq &key (test 'eq))
  "Get position of ITEM in SEQ.
A simple replacement of CL `position'."
  (cl-loop for i in seq for index from 0
           when (funcall test i item) return index))

(defun iterator:list (seq)
  "Return an iterator from SEQ."
  (let ((lis seq))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defun iterator:next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(cl-defun iterator:sub-next (seq elm &key (test 'eq))
  "Create iterator from position of ELM to end of SEQ."
  (let* ((pos      (iterator:position elm seq :test test))
         (sub      (nthcdr (1+ pos) seq))
         (iterator (iterator:list sub)))
    (lambda ()
      (iterator:next iterator))))

(cl-defun iterator:sub-prec (seq elm &key (test 'eq))
  "Create iterator from position of ELM to beginning of SEQ."
  (let* ((rev-seq  (reverse seq))
         (pos      (iterator:position elm rev-seq :test test))
         (sub      (nthcdr (1+ pos) rev-seq))
         (iterator (iterator:list sub)))
    (lambda ()
      (iterator:next iterator))))

(defun iterator:circular (seq)
  "Infinite iteration on SEQ."
  (let ((lis seq))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (pcase lis (`(,_ . ,ll) (or ll seq))))
         elm))))

(cl-defun iterator:sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (let* ((rev-seq  (reverse seq))
         (pos      (1+ (iterator:position elm rev-seq :test test)))
         (sub      (append (nthcdr pos rev-seq) (cl-subseq rev-seq 0 pos)))
         (iterator (iterator:circular sub)))
    (lambda ()
      (iterator:next iterator))))

(cl-defun iterator:sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (let* ((pos      (1+ (iterator:position elm seq :test test)))
         (sub      (append (nthcdr pos seq) (cl-subseq seq 0 pos)))
         (iterator (iterator:circular sub)))
    (lambda ()
      (iterator:next iterator))))

(defun iterator:apply-fun-on-list (fun seq)
  "Create an iterator that apply function FUN on each elm of SEQ."
  (let ((lis seq)
        (fn fun))
    (lambda ()
      (let ((elm (if (car lis)
                     (funcall fn (car lis)))))
        (setq lis (cdr lis))
        elm))))

(defun iterator:scroll-list (seq size)
  "Create an iterator of the cl-subseq of the cdr of SEQ ending to SIZE."
  (let* ((lis seq)
         (end size))
    (lambda ()
      (let ((sub (cl-subseq lis 0 end)))
        (setq lis (cdr lis))
        (if (< (length lis) end)
            (setq end (- end 1)))
        (remove nil sub)))))

(defun iterator:scroll-up (seq elm size)
  (let* ((pos (cl-position (car (last elm)) seq))
         (sub (reverse (cl-subseq seq 0 pos)))
         (iterator (iterator:scroll-list sub size)))
    (lambda ()
      (reverse (iterator:next iterator)))))

(defun iterator:scroll-down (seq elm size)
  (let* ((pos (cl-position (car (last elm)) seq))
         (sub (cl-subseq seq pos))
         (iterator (iterator:scroll-list sub size)))
    (lambda ()
      (iterator:next iterator))))

(defun iterator:fibo ()
  "Fibonacci generator."
  (let ((a 0)
        (b 1))
    (lambda ()
      (cl-psetq a b
                b (+ a b))
      a)))

;;; Provide
(provide 'iterator)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; iterator.el ends here
