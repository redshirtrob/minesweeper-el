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
  (define-key minesweeper-mode-map (kbd "C-x C-n") 'minesweeper-new-game)
  (define-key minesweeper-mode-map (kbd "C-n") 'minesweeper-down)
  (define-key minesweeper-mode-map (kbd "C-p") 'minesweeper-up)
  (define-key minesweeper-mode-map (kbd "C-f") 'minesweeper-right)
  (define-key minesweeper-mode-map (kbd "C-b") 'minesweeper-left)
  (define-key minesweeper-mode-map (kbd "C-a") 'minesweeper-row-first)
  (define-key minesweeper-mode-map (kbd "C-e") 'minesweeper-row-last)
  (define-key minesweeper-mode-map (kbd "SPC") 'minesweeper-toggle-mark)
  (define-key minesweeper-mode-map (kbd "RET") 'minesweeper-reveal))

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

;; Cell values
(defconst *minesweeper-default-symbol* 0)
(defconst *minesweeper-bomb-symbol* 9)

;; Cell visibily state
(defconst *minesweeper-cell-hidden-symbol* "H")
(defconst *minesweeper-cell-revealed-symbol* "R")
(defconst *minesweeper-cell-question-symbol* "?")
(defconst *minesweeper-cell-flagged-symbol* "F")

;; Game State
(defconst *minesweeper-game-active* 1)
(defconst *minesweeper-game-over-win* 2)
(defconst *minesweeper-game-over-lose* 3)

(defvar *minesweeper-game-state* *minesweeper-game-active*
  "Current game state.")

(defun minesweeper-init ()
  (setq *minesweeper-game-state* *minesweeper-game-active*)
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

(defun minesweeper-bomb-cells ()
  "List of cells with bombs."
  (cl-remove-if-not
              (lambda (x) (equal x *minesweeper-bomb-symbol*))
              *minesweeper-board*))

(defun minesweeper-is-bomb (row col)
  (= *minesweeper-bomb-symbol* (minesweeper-get-symbol row col)))

(defun minesweeper-is-space (row col)
  (= *minesweeper-default-symbol* (minesweeper-get-symbol row col)))

(defun minesweeper-is-number (row col)
  (and (not (minesweeper-is-bomb row col))
       (not (minesweeper-is-space row col))))

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

(defun minesweeper-set-cell-state-flagged (row col)
  (minesweeper-set-cell-state row col *minesweeper-cell-flagged-symbol*))

(defun minesweeper-set-cell-state-question (row col)
  (minesweeper-set-cell-state row col *minesweeper-cell-question-symbol*))

(defun minesweeper-set-cell-state-hidden (row col)
  (minesweeper-set-cell-state row col *minesweeper-cell-hidden-symbol*))

(defun minesweeper-set-cell-state-revealed (row col)
  (minesweeper-set-cell-state row col *minesweeper-cell-revealed-symbol*))

(defun minesweeper-is-cell-flagged (row col)
  (let ((cell-state (minesweeper-get-cell-state row col)))
    (equal *minesweeper-cell-flagged-symbol* cell-state)))

(defun minesweeper-revealed-bomb-cell-count ()
  "The number of bomb cells revealed.  Should only ever be 1."
  (setq index 0)
  (setq count 0)
  (while (< index (minesweeper-board-size))
    (if (and (= (elt *minesweeper-board* index) *minesweeper-bomb-symbol*)
             (equal (elt *minesweeper-board-state* index) *minesweeper-cell-revealed-symbol*))
        (setq count (1+ count)))
    (setq index (1+ index)))
  count)

(defun minesweeper-update-game-state ()
  "Update the game state for win/loss conditions."
  (cond ((and (equal (minesweeper-flagged-cells) (minesweeper-bomb-cells))
              (= 0 (minesweeper-number-of-hidden-cells)))
         (setq *minesweeper-game-state* *minesweeper-game-over-win*))
        ((/= 0 (minesweeper-revealed-bomb-cell-count))
         (setq *minesweeper-game-state* *minesweeper-game-over-lose*))))

(defun minesweeper-flagged-cells ()
  "List of flagged cells."
  (cl-remove-if-not
   (lambda (x) (equal x *minesweeper-cell-flagged-symbol*))
   *minesweeper-board-state*))

(defun minesweeper-flags-remaining ()
  "Number of flags remaining to be placed."
  (- *minesweeper-bombs* (length (minesweeper-flagged-cells))))

(defun minesweeper-number-of-hidden-cells ()
  "Number of cells still hidden or questioned."
  (cl-remove-if-not
   (lambda (x)
     (or (equal x *minesweeper-cell-hidden-symbol*)
         (equal x *minesweeper-cell-question-symbol*)))))

(defun minesweeper-reveal-current-cell ()
  (minesweeper-set-cell-state-revealed
   *minesweeper-current-row*
   *minesweeper-current-column*))

(defun minesweeper-reveal-connected-region ()
  "Reveal all cells connected to the current cell.
A cell is connected to another cell if there is a path
of spaces from one cell to another."
  (message "minesweeper-reveal-connected-region")
  (let* ((start-cell (list *minesweeper-current-row* *minesweeper-current-column*))
        (cells-to-examine (cons start-cell nil))
        (cells-already-examined))
    (while cells-to-examine
      (let* ((current-cell (car cells-to-examine))
             (neighbors (minesweeper-get-neighbors (car current-cell) (cadr current-cell))))
        ;; Examine each cell in the neighbors list:
        ;;
        ;; If cell is a space, push it onto the list of cells
        ;; to examine (unless it's already on the list or has
        ;; already been examined).
        ;;
        ;; If cell is a number, mark it as revealed and
        ;; put it on the list of cells aready examined
        (dolist (neighbor neighbors)
          (when (not (member neighbor cells-already-examined))
            (cond
             ((minesweeper-is-space (car neighbor) (cadr neighbor))
              (setq cells-to-examine (cons neighbor cells-to-examine)))
             ((minesweeper-is-number (car neighbor) (cadr neighbor))
              (minesweeper-set-cell-state-revealed (car neighbor) (cadr neighbor))))))
        (minesweeper-set-cell-state-revealed (car current-cell) (cadr current-cell))
        (setq cells-already-examined (cons current-cell cells-already-examined))
        (setq cells-to-examine (remove current-cell cells-to-examine))))))

(defun minesweeper-get-display-value (row col)
  "Get the board display value for the cell at (ROW, COL)"
  (if (= *minesweeper-game-state* *minesweeper-game-over-lose*)
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
    (minesweeper-insert-separator)

    ;; Game Status
    (cond
     ((= *minesweeper-game-state* *minesweeper-game-over-win*)
      (insert "\n\tYou Win!\n"))
     ((= *minesweeper-game-state* *minesweeper-game-over-lose*)
      (insert "\n\tYou Lose...\n"))
     (t
      (insert "\n\n")))

    (insert "\tFlags\n")
    (insert (format "\t%3s" (minesweeper-flags-remaining)))
    (insert "\n")))

(defun minesweeper-set-cursor-position (row col)
  "Move the cursor to the cell at (ROW, COL)"
  (interactive)
  (let* ((row-length (+ (* 4 *minesweeper-columns*) 2))
         (origin (+ row-length 3)))
    (setq *minesweeper-current-row* row)
    (setq *minesweeper-current-column* col)
    (goto-char (+ origin
                  (* row 2 row-length)
                  (* col 4)))))

(defun minesweeper-goto-start-position ()
  "Move the cursor to the first cell"
  (interactive)
  (minesweeper-set-cursor-position 0 0))

(defun minesweeper-up ()
  "Move the cursor up one row."
  (interactive)
  (minesweeper-set-cursor-position
   (mod (1- *minesweeper-current-row*) *minesweeper-rows*)
   *minesweeper-current-column*))

(defun minesweeper-down ()
  "Move the cursor down one row."
  (interactive)
  (minesweeper-set-cursor-position
   (mod (1+ *minesweeper-current-row*) *minesweeper-rows*)
   *minesweeper-current-column*))

(defun minesweeper-left ()
  "Move the cursor left one column."
  (interactive)
  (minesweeper-set-cursor-position
   *minesweeper-current-row*
   (mod (1- *minesweeper-current-column*) *minesweeper-columns*)))

(defun minesweeper-right ()
  "Move the cursor right one column."
  (interactive)
  (minesweeper-set-cursor-position
   *minesweeper-current-row*
   (mod (1+ *minesweeper-current-column*) *minesweeper-columns*)))

(defun minesweeper-row-first ()
  "Move the cursor to the first column in the current row"
  (interactive)
  (minesweeper-set-cursor-position *minesweeper-current-row* 0))

(defun minesweeper-row-last ()
  "Move the cursor to the last column in the current row"
  (interactive)
  (minesweeper-set-cursor-position *minesweeper-current-row* (1- *minesweeper-columns*)))

(defun minesweeper-toggle-mark ()
  "Toggle the visual mark the current cell.
Do nothing if the current cell is revealed.
Clear -> Flagged -> Questioned -> Clear."
  (interactive)
  (message "minesweeper-toggle-mark")
  (let* ((row *minesweeper-current-row*)
         (col *minesweeper-current-column*)
         (cell-state (minesweeper-get-cell-state row col)))
    (cond
     ((equal cell-state *minesweeper-cell-hidden-symbol*)
      (minesweeper-set-cell-state-flagged row col))
     ((equal cell-state *minesweeper-cell-flagged-symbol*)
      (minesweeper-set-cell-state-question row col))
     ((equal cell-state *minesweeper-cell-question-symbol*)
      (minesweeper-set-cell-state-hidden row col))))
  (minesweeper-update-game-state)
  (minesweeper-print-board)
  (minesweeper-set-cursor-position *minesweeper-current-row* *minesweeper-current-column*))

(defun minesweeper-reveal ()
  "Reveal the board value at the current cell.
Does nothing when the cell is flagged."
  (interactive)
  (message "minesweeper-reveal")
  (when (not (minesweeper-is-cell-flagged *minesweeper-current-row* *minesweeper-current-column*))
    (minesweeper-reveal-current-cell)
    (when (minesweeper-is-space *minesweeper-current-row* *minesweeper-current-column*)
      (minesweeper-reveal-connected-region))
    (minesweeper-update-game-state)
    (minesweeper-print-board)
    (minesweeper-set-cursor-position *minesweeper-current-row* *minesweeper-current-column*)))

(provide 'minesweeper)
;;; minesweeper-el ends here
