(in-package :gridgreed)

(defclass player ()
  ((x :initform 0 :accessor xpos)
   (y :initform 0 :accessor ypos)
   (name :initarg :name :initform "Player" :accessor name)
   (owned-count :initform 1 :accessor owned-count)
   (cell-time :initform 0 :accessor cell-time)
   (cell-ticks :initform 0 :accessor cell-ticks)
   (color :initarg :color :initform (error "Supply color for player.") :reader color)))

(defmethod render ((this player))
  (with-pushed-canvas ()
    (let ((x (+ 8 (* *cell-size* (xpos this))))
          (y (+ 10 (* *cell-size* (ypos this)))))
      ;(draw-circle (vec2 x y) 16 :fill-paint *blue*))
      (draw-rect (vec2 x y) *cell-size* *cell-size* :thickness 2.0 :stroke-paint *black*))))

(defmethod print-object ((obj player) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (with-slots (x y name owned-count color) obj
       (format stream "~a at ~a x ~a" name x y))))

(defun reset-cell-time (player)
  (setf (cell-ticks player) -1))

(defun set-cell-time-now (player)
  (setf (cell-time player) (real-time-seconds)))
