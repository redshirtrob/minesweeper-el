;;; minesweeper.el --- play minesweeper in Emacs

;; Copyright 2015 Robert Jones

;; Author: Robert Jones <robert.jones.sv@gmail.com>
;; Version: 2015.03.27
;; Package-Version: 20150327.01
;; URL:

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Beginner: 8x8 10 bombs
;; Intermediate: 16x16 40 bombs
;; Advanced: 31x16 99 bombs

;;; Code:

(define-derived-mode minesweeper-mode special-mode "minesweeper-mode"
  (define-key minesweeper-mode-map (kbd "k") 'minesweeper-up)
  (define-key minesweeper-mode-map (kbd "j") 'minesweeper-down)
  (define-key minesweeper-mode-map (kbd "h") 'minesweeper-left)
  (define-key minesweeper-mode-map (kbd "l") 'minesweeper-right)
  (define-key minesweeper-mode-map (kbd "SPC") 'minesweeper-toggle))

;;;##a#autoload
(defun minesweeper ()
  "Start playing minesweeper."
  (interactive)
  (switch-to-buffer "minesweeper")
  (minesweeper-mode)
  (minesweeper-init))

(require 'cl-lib)

(defvar *minesweeper-board* nil
  "The gameboard.")

(defvar *minesweeper-columns* 8
  "The width of the board.")

(defvar *minesweeper-rows* 8
  "The height of the board.")

(defvar *minesweeper-bombs* 10
  "The number of bombs on the board.")

;; Cell values
(defconst *minesweeper-default-symbol* 0)
(defconst *minesweeper-bomb-symbol* 9)

;; Cell visibily state
(defconst *minesweeper-cell-hidden-symbol* "H")
(defconst *minesweeper-cell-revealed-symbol* "R")
(defconst *minesweeper-cell-question-symbol* "?")
(defconst *minesweeper-cell-flagged-symbol* "F")

(defun minesweeper-init ()
  (message "minesweeper-init")
  (minesweeper-make-board)
  (minesweeper-print-board))

(defun minesweeper-board-size ()
    (* *minesweeper-columns* *minesweeper-rows*))

(defun minesweeper-make-board ()
    "Generate a random board"
  (setq *minesweeper-board* (make-vector (minesweeper-board-size) *minesweeper-default-symbol*))
  (setq *minesweeper-board-state* (make-vector (minesweeper-board-size) *minesweeper-default-symbol*))
  (setq bombs-placed 0)
  (while (< bombs-placed *minesweeper-bombs*)
    (let ((bomb-column (random *minesweeper-columns*))
          (bomb-row (random *minesweeper-rows*)))
      (when (/= *minesweeper-bomb-symbol* (minesweeper-get-symbol bomb-row bomb-column))
        (progn (minesweeper-set-symbol bomb-row bomb-column *minesweeper-bomb-symbol*)
               (setq bombs-placed (1+ bombs-placed))))))
  (dotimes (col *minesweeper-columns*)
    (dotimes (row *minesweeper-rows*)
      (let ((symbol (minesweeper-get-symbol row col)))
        (when (= symbol *minesweeper-default-symbol*)
          (minesweeper-set-symbol row col (minesweeper-count-adjacent-bombs row col)))))))

(defun minesweeper-neighbor-candidates (coord limit)
  "Return a list of valid coordinates for position COORD
and upper bound LIMIT"
  (-filter (lambda (x) (and (>= x 0) (< x limit)))
           (mapcar (lambda (x) (+ coord x)) '(-1 0 1))))

(defun minesweeper-get-neighbors (row col)
  "Return a list of neigbhor cells to cell (ROW, COL)"
  (let ((rows (minesweeper-neighbor-candidates row *minesweeper-rows*))
        (cols (minesweeper-neighbor-candidates col *minesweeper-columns*)))
    (-filter (lambda (x) (or (/= row (car x)) (/= col (cadr x))))
             (let (coords)
               (dolist (r rows)
                 (dolist (c cols)
                   (setq coords (cons (list r c) coords))))
               coords))))

(defun minesweeper-count-adjacent-bombs (row col)
  "Count the number of bombs adjacent to the cell at (ROW, COL)"
  (message "Neighbors: %s" (minesweeper-get-neighbors row col))
  (length (-filter
           (lambda (x)
             (message "Coords: (%s, %s)" (car x) (cadr x))
             (= *minesweeper-bomb-symbol* (minesweeper-get-symbol (car x) (cadr x))))
           (minesweeper-get-neighbors row col))))

(defun minesweeper-get-symbol (row col)
  "Get the symbol at (ROW, COL)"
  (elt *minesweeper-board*
       (+ (* row *minesweeper-columns*) col)))

(defun minesweeper-set-symbol (row col val)
  "Set the symbol at (ROW, COL) to VAL"
  (aset *minesweeper-board*
       (+ (* row *minesweeper-columns*) col)
       val))

(defun minesweeper-get-display-value (row col)
  (number-to-string (minesweeper-get-symbol row col)))

(defun minesweeper-insert-separator ()
  (dotimes (col *minesweeper-columns*)
    (insert "+---"))
  (insert "+\n"))

(defun minesweeper-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *minesweeper-rows*)
      (minesweeper-insert-separator)
      (dotimes (col *minesweeper-columns*) ;; values
        (insert "| ")
        (insert (minesweeper-get-display-value row col))
        (insert " "))
      (insert "|\n"))
    (minesweeper-insert-separator)))

(defun minesweeper-up ()
  (interactive)
  (message "minesweeper-up"))

(defun minesweeper-down ()
  (interactive)
  (message "minesweeper-down"))

(defun minesweeper-left ()
  (interactive)
  (message "minesweeper-left"))

(defun minesweeper-right ()
  (interactive)
  (message "minesweeper-right"))

(defun minesweeper-toggle ()
  (interactive)
  (message "minesweeper-toggle"))

(provide 'minesweeper)
;;; minesweeper-el ends here
