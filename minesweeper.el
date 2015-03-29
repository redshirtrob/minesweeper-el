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
  (define-key minesweeper-mode-map (kbd "n") 'minesweeper-new-game)
  (define-key minesweeper-mode-map (kbd "k") 'minesweeper-up)
  (define-key minesweeper-mode-map (kbd "j") 'minesweeper-down)
  (define-key minesweeper-mode-map (kbd "h") 'minesweeper-left)
  (define-key minesweeper-mode-map (kbd "l") 'minesweeper-right)
  (define-key minesweeper-mode-map (kbd "SPC") 'minesweeper-reveal))

;;;##a#autoload
(defun minesweeper ()
  "Start playing minesweeper."
  (interactive)
  (switch-to-buffer "minesweeper")
  (minesweeper-mode)
  (minesweeper-new-game))

(require 'cl-lib)

(defvar *minesweeper-board* nil
  "The gameboard.")

(defvar *minesweeper-columns* 8
  "The width of the board.")

(defvar *minesweeper-rows* 8
  "The height of the board.")

(defvar *minesweeper-bombs* 10
  "The number of bombs on the board.")

(defvar *minesweeper-current-row* 0
  "Current row position")

(defvar *minesweeper-current-column* 0
  "Current column position")

(defvar *minesweeper-game-over* t
  "Whether or not the game is over")

;; Cell values
(defconst *minesweeper-default-symbol* 0)
(defconst *minesweeper-bomb-symbol* 9)

;; Cell visibily state
(defconst *minesweeper-cell-hidden-symbol* "H")
(defconst *minesweeper-cell-revealed-symbol* "R")
(defconst *minesweeper-cell-question-symbol* "?")
(defconst *minesweeper-cell-flagged-symbol* "F")

(defun minesweeper-init ()
  (setq *minesweeper-game-over* nil)
  (setq *minesweeper-current-row* 0)
  (setq *minesweeper-current-column* 0)
  (minesweeper-make-board)
  (minesweeper-print-board))

(defun minesweeper-new-game ()
  (interactive)
  (minesweeper-init)
  (minesweeper-goto-start-position))

(defun minesweeper-board-size ()
    (* *minesweeper-columns* *minesweeper-rows*))

(defun minesweeper-make-board ()
    "Generate a random board"
  (setq *minesweeper-board* (make-vector (minesweeper-board-size) *minesweeper-default-symbol*))
  (setq *minesweeper-board-state* (make-vector (minesweeper-board-size) *minesweeper-cell-hidden-symbol*))
  (setq bombs-placed 0)
  (while (< bombs-placed *minesweeper-bombs*)
    (let ((bomb-column (random *minesweeper-columns*))
          (bomb-row (random *minesweeper-rows*)))
      (when (not (minesweeper-is-bomb bomb-row bomb-column))
        (progn (minesweeper-set-bomb bomb-row bomb-column)
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
  (length (-filter (lambda (x) (minesweeper-is-bomb (car x) (cadr x)))
                   (minesweeper-get-neighbors row col))))

(defun minesweeper-is-bomb (row col)
  (= *minesweeper-bomb-symbol* (minesweeper-get-symbol row col)))

(defun minesweeper-is-space (row col)
  (= *minesweeper-default-symbol* (minesweeper-get-symbol row col)))

(defun minesweeper-get-symbol (row col)
  "Get the symbol at (ROW, COL)"
  (elt *minesweeper-board*
       (+ (* row *minesweeper-columns*) col)))

(defun minesweeper-set-symbol (row col val)
  "Set the symbol at (ROW, COL) to VAL"
  (aset *minesweeper-board*
       (+ (* row *minesweeper-columns*) col)
       val))

(defun minesweeper-set-bomb (row col)
  (minesweeper-set-symbol row col *minesweeper-bomb-symbol*))

(defun minesweeper-get-cell-state (row col)
  (elt *minesweeper-board-state*
       (+ (* row *minesweeper-columns*) col)))

(defun minesweeper-set-cell-state (row col val)
  (aset *minesweeper-board-state*
        (+ (* row *minesweeper-columns*) col)
        val))

(defun minesweeper-reveal-current-cell ()
  (minesweeper-set-cell-state
   *minesweeper-current-row*
   *minesweeper-current-column*
   *minesweeper-cell-revealed-symbol*))

(defun minesweeper-reveal-connected-spaces ()
  "Reveal all cells adjacent to the current cell")

(defun minesweeper-get-display-value (row col)
  "Get the board display value for the cell at (ROW, COL)"
  (if *minesweeper-game-over*
      (if (minesweeper-is-bomb row col)
          "*"
        (number-to-string (minesweeper-get-symbol row col)))
    (let ((cell-state (minesweeper-get-cell-state row col)))
       (cond
        ((equal cell-state *minesweeper-cell-hidden-symbol*)
         " ")
        ((equal cell-state *minesweeper-cell-revealed-symbol*)
         (number-to-string (minesweeper-get-symbol row col)))
        ((equal cell-state *minesweeper-cell-question-symbol*)
         "?")
        ((equal cell-state *minesweeper-cell-flagged-symbol*)
         "F")))))

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

(defun minesweeper-goto-position ()
  "Move the cursor to the cell at (ROW, COL)"
  (interactive)
  (let* ((row-length (+ (* 4 *minesweeper-columns*) 2))
         (origin (+ row-length 3)))
    (goto-char (+ origin
                  (* *minesweeper-current-row* 2 row-length)
                  (* *minesweeper-current-column* 4)))))

(defun minesweeper-goto-start-position ()
  "Move the cursor to the first cell"
  (interactive)
  (setq *minesweeper-current-row* 0)
  (setq *minesweeper-current-column* 0)
  (minesweeper-goto-position))

(defun minesweeper-up ()
  "Move the cursor up one row."
  (interactive)
  (setq *minesweeper-current-row*
        (mod (1- *minesweeper-current-row*) *minesweeper-rows*))
  (minesweeper-goto-position))

(defun minesweeper-down ()
  "Move the cursor down one row."
  (interactive)
  (setq *minesweeper-current-row*
        (mod (1+ *minesweeper-current-row*) *minesweeper-rows*))
  (minesweeper-goto-position))

(defun minesweeper-left ()
  "Move the cursor left one column."
  (interactive)
  (setq *minesweeper-current-column*
        (mod (1- *minesweeper-current-column*) *minesweeper-columns*))
  (minesweeper-goto-position))

(defun minesweeper-right ()
  "Move the cursor right one column."
  (interactive)
  (setq *minesweeper-current-column*
        (mod (1+ *minesweeper-current-column*) *minesweeper-columns*))
  (minesweeper-goto-position))

(defun minesweeper-reveal ()
  (interactive)
  (message "minesweeper-reveal")
  (minesweeper-reveal-current-cell)
  (cond
   ((minesweeper-is-space *minesweeper-current-row* *minesweeper-current-column*)
    (minesweeper-reveal-connected-spaces))
   ((minesweeper-is-bomb *minesweeper-current-row* *minesweeper-current-column*)
    (setq *minesweeper-game-over* t)))
  (minesweeper-print-board)
  (minesweeper-goto-position))

(provide 'minesweeper)
;;; minesweeper-el ends here
