(in-package :gridgreed)

(defclass grid ()
  ((rows :initarg :rows :initform (error "Grid needs argument: rows") :reader rows)
   (cols :initarg :cols :initform (error "Grid needs argument: cols") :reader cols)
   (cells :initform (make-array (list 40 40) :initial-element (make-instance 'cell)) :accessor cells)))

(defmethod initialize-instance :after ((this grid) &key)
  (dotimes (row (rows this))
    (dotimes (col (cols this))
      ;(setf (get-cell this row col) (make-instance 'cell))
      (setf (aref (cells this) row col) (make-instance 'cell :row row :col col)))))

(defclass cell ()
  ((row :initarg :row :accessor row)
   (col :initarg :col :accessor col)
   (touched :initform T :accessor touched)
   (owner :initarg :owner :initform nil :accessor owner)
   (strength :initarg :strength :initform 0 :accessor strength)))

(defun cell-is-occupied (cell)
  (not (cell-is-neutral cell)))

(defmethod print-object ((obj cell) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "~s ~s" (row obj) (col obj))))

(defun cell-is-neutral (cell)
  (if (owner cell)
      nil
      T))

(defun set-cell-owner (grid i j owner)
  (setf (owner (get-cell grid i j)) owner))

(defmethod get-cell ((grid grid) i j)
  (if (valid-cell-p grid i j)
      (aref (cells grid) i j)
      nil))

(defmethod (setf get-cell) (value (grid grid) i j)
  (when (valid-cell-p grid i j)
    (setf (aref (cells grid) i j) value)))

(defmethod render ((this grid))
  (with-pushed-canvas ()
    (dotimes (row (rows this))
      (dotimes (col (cols this))

        (let ((x (+ 8 (* col *cell-size*)))
              (y (+ 10 (* row *cell-size*)))
              (cell (get-cell this row col)))

          ;(draw-text (write-to-string (round (strength cell))) (vec2 (+ x (/ *cell-size* 2)) (+ y (/ *cell-size* 2))) :fill-color *black*)

          (when (not (cell-is-neutral cell))
            (let* ((color (color (owner cell)))
                  (strength (strength cell))
                  (alpha (/ (* 50 (log (+ 1 strength) 10)) 100)))
;                   (alpha (+ 0.4 (* 4 (/ (strength cell) 800)))))
              (setf (w color) alpha)
              (draw-rect (vec2 x y) *cell-size* *cell-size* :stroke-paint color :fill-paint color)))

          (draw-rect (vec2 x y) *cell-size* *cell-size* :stroke-paint *black*))))))

(defmethod valid-cell-p ((grid grid) i j)
  (if (or
       (< i 0)
       (< j 0)
       (> i (1- (rows grid)))
       (> j (1- (cols grid))))
        nil
      T))

(defun get-neighbor (grid row col direction)
  (let ((target-row row)
        (target-col col))
    (cond ((and (eql direction :north))
           (setf target-row (1+ row)))
          ((and (eql direction :south))
           (setf target-row (1- row)))
          ((and (eql direction :west))
           (setf target-col (1- col)))
          ((and (eql direction :east))
           (setf target-col (1+ col)))
          ((and (eql direction :north-east))
           (setf target-row (1+ row))
           (setf target-col (1- col)))
          ((and (eql direction :north-west))
           (setf target-row (1+ row))
           (setf target-col (1+ col)))
          ((and (eql direction :south-east))
           (setf target-row (1- row))
           (setf target-col (1+ col)))
          ((and (eql direction :south-west))
           (setf target-row (1- row))
           (setf target-col (1- col))))
    (if (valid-cell-p grid target-row target-col)
        (get-cell grid target-row target-col)
        nil)))

(defun get-neighbors-of-list (grid cells &optional debugging)
  (let ((result '()))
    (dolist (cell cells)

      (when cell

        (when debugging
          (format t "RESULT = ~a~%" result)
          (format t "~a~%~%~%" (get-neighbors grid (row cell) (col cell))))

        (setf result (append result (get-neighbors grid (row cell) (col cell))))))
    result))

(defmacro push-not-nil (obj place)
  `(when ,obj
    (push ,obj ,place)))

(defun get-neighbors (grid row col &key (owner nil) (distance 1))
  (let ((result '())
        (d distance)
        (d-min-1 (1- distance)))
    ; corners
    (push-not-nil (get-cell grid (- row d) (- col d)) result)
    (push-not-nil (get-cell grid (- row d) (+ col d)) result)
    (push-not-nil (get-cell grid (+ row d) (- col d)) result)
    (push-not-nil (get-cell grid (+ row d) (+ col d)) result)

    ; top line
    (dolist (c-diff (range (- d-min-1) d-min-1))
      (push-not-nil (get-cell grid (+ row d) (- col c-diff)) result))

    ; bottom line
    (dolist (c-diff (range (- d-min-1) d-min-1))
      (push-not-nil (get-cell grid (- row d) (- col c-diff)) result))

    ; left line
    (dolist (r-diff (range (- d-min-1) d-min-1))
      (push-not-nil (get-cell grid (- row r-diff) (- col d)) result))

    ; right line
    (dolist (r-diff (range (- d-min-1) d-min-1))
      (push-not-nil (get-cell grid (+ row r-diff) (+ col d)) result))

    (if owner
        (setf result (remove-if-not (lambda (cell) (eql (owner cell) owner)) result))
        result)))

(defmethod move-distance ((c1 cell) (c2 cell))
  ;; manhattan distance
  (let ((r1 (row c1))
        (r2 (row c2))
        (c1 (col c1))
        (c2 (col c2)))
    (+ (abs (- r1 r2)) (abs (- c1 c2)))))

(defmethod distance ((c1 cell) (c2 cell))
  (let ((r1 (row c1))
        (r2 (row c2))
        (c1 (col c1))
        (c2 (col c2)))
    (max (abs (- r1 r2)) (abs (- c1 c2)))))

(defmethod decay-untouched-cells ((this grid) player amount)
  (dotimes (row (rows this))
    (dotimes (col (cols this))
      (let* ((cell (get-cell this row col))
             (current-strength (strength cell))
             (new-strength (- current-strength amount)))
        (when (and (eq (owner cell) player) (not (touched cell)))
          (setf (strength cell) (max 0 new-strength)))))))

(defmethod reset-cells-touched ((this grid))
  (dotimes (row (rows this))
    (dotimes (col (cols this))
      (let ((cell (get-cell this row col)))
        (setf (touched cell) nil)))))
