;;; iterator.el --- A library to create and use elisp iterators objects. -*- lexical-binding: t -*-

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>

;; Copyright (C) 2009 ~ 2014 Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 24.1+
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; Version: 1.1
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

;;; Commentary:

;; Provide simple iterations functions.
;; Usage:
;; (setq foo (iterator:list '(a b c)))
;; (iterator:next foo) => a
;; (iterator:next foo) => b
;; (iterator:next foo) => c

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun iterator:list (seq &optional cycle)
  "Return an iterator from SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (if cycle
                      (or (cdr lis) seq)
                    (cdr lis)))
        elm))))

;;;###autoload
(defun iterator:circular (seq)
  "Infinite iteration on SEQ."
  (iterator:list seq 'cycle))

(defun iterator:next (iterator)
  "Return next elm of ITERATOR."
  (and iterator (funcall iterator)))

(cl-defun iterator:sub-next (seq elm &key (test 'eq))
  "Create iterator from position of ELM to end of SEQ."
  (let* ((pos      (cl-position elm seq :test test))
         (sub      (nthcdr (1+ pos) seq))
         (iterator (iterator:list sub)))
    (lambda ()
      (iterator:next iterator))))

(cl-defun iterator:sub-prec (seq elm &key (test 'eq))
  "Create iterator from position of ELM to beginning of SEQ."
  (let* ((rev-seq  (reverse seq))
         (pos      (cl-position elm rev-seq :test test))
         (sub      (nthcdr (1+ pos) rev-seq))
         (iterator (iterator:list sub)))
    (lambda ()
      (iterator:next iterator))))

(cl-defun iterator:sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (let* ((rev-seq  (reverse seq))
         (pos      (1+ (cl-position elm rev-seq :test test)))
         (sub      (append (nthcdr pos rev-seq) (cl-subseq rev-seq 0 pos)))
         (iterator (iterator:circular sub)))
    (lambda ()
      (iterator:next iterator))))

(cl-defun iterator:sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (let* ((pos      (1+ (cl-position elm seq :test test)))
         (sub      (append (nthcdr pos seq) (cl-subseq seq 0 pos)))
         (iterator (iterator:circular sub)))
    (lambda ()
      (iterator:next iterator))))

;;; Provide
(provide 'iterator)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; iterator.el ends here
