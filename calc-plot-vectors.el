;;; calc-plot-vectors.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  nilninull

;; Author: nilninull <nilninull@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package needs gnuplot and GNU calc.
;; calc is shipped in emacs package, it' no need to install.
;;
;; Sample key setting:
;; (define-key calc-mode-map "gm" 'calc-plot-vectors)
;;

;;; Code:

(require 'calc-graph)

(defvar calc-plot-vectors-count 0)

(defcustom calc-plot-vectors-initial-settings
  '(
    "set size ratio -1"
    "set view equal xy"
    "set view equal xyz"
    "set grid"
    )
  "Initialize commands for gnuplot used by `calc-plot-vectors'."
  :type '(repeat (string :tag "gnuplot command"))
  :group 'calc)

(defun calc-plot-vectors (count)
  "Plotting vectors or matrices with vectors style by gnuplot.

This command plot the vector/matrix of top of stack.

If COUNT is cons (it means with `C-u' universal argument),
add the vector/matrix previous plotting one.

If COUNT is a number (it means the key press as `C-N' or `M-N'),
top of N stack is used for plotting."
  (interactive "P")
  (cl-flet* ((make-file-name (n)
                             (expand-file-name (format "calc-plot-vectors-%d.dat" n)
                                               temporary-file-directory))
             (make-plot-spec (n)
                             (format "\"%s\" title \"No. %d\" with vectors"
                                     (make-file-name n)
                                     n))
             (make-dat (m n)
                       (with-temp-buffer
                         (cond ((math-matrixp m)
                                (let ((dim (calcFunc-vlen m)))
                                  (unless (and (<= 2 dim 3)
                                               (math-square-matrixp m))
                                    (error "Please run with square matrices of dimention 2 or 3"))
                                  (cl-loop for i from 1 to dim
                                           do (progn
                                                (cl-loop for j from 1 to dim
                                                         do (insert "0 "))
                                                (cl-loop for j from 1 to dim
                                                         do (insert (math-format-number (elt (elt m j) i))
                                                                    " "))
                                                (insert "\n")))))
                               ((math-vectorp m)
                                (let ((dim (calcFunc-vlen m)))
                                  (unless (<= 2 dim 3)
                                    (error "Please run with vectors of dimention 2 or 3"))
                                  (cl-loop for j from 1 to dim
                                           do (insert "0 "))
                                  (cl-loop for j from 1 to dim
                                           do (insert (math-format-number (elt m j))
                                                      " "))
                                  (insert "\n")))
                               (t
                                (error "Please use with vectors or matrices")))
                         (write-region nil nil (make-file-name n)))))
    (let ((ms (cond ((consp count)
                     (list (calc-top-list 1)
                           (list (cl-incf calc-plot-vectors-count))))
                    ((integerp count)
                     (list (nreverse (calc-top-list count))
                           (cl-loop for n from 1 to (setq calc-plot-vectors-count count)
                                    collect n)))
                    (t
                     (list (calc-top-list 1)
                           (list (setq calc-plot-vectors-count 1))))))
          (calc-graph-no-auto-view t))
      (when calc-plot-vectors-initial-settings
        (calc-graph-command (mapconcat 'identity
                                       calc-plot-vectors-initial-settings
                                       ";")))
      (apply #'cl-mapc #'make-dat ms)
      (calc-gnuplot-command (format "%s %s"
                                    (if (= (1- (length (caar ms))) 2)
                                        "plot"
                                      "splot")
                                    (mapconcat #'make-plot-spec
                                               (cl-loop for n from calc-plot-vectors-count downto 1
                                                        collect n)
                                               ", "))))))

(provide 'calc-plot-vectors)
;;; calc-plot-vectors.el ends here
