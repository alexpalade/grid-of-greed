(cl:in-package :gridgreed)

(defclass score-bar ()
  ((width :initarg :width :initform (error "Please provide with for score bar") :reader width)
   (height :initarg :height :initform (error "Please provide height for score bar") :reader height)
   (origin :initarg :origin :initform (vec2 0 0) :reader origin)
   (p1-bar-width :initform 0 :accessor p1-bar-width)
   (p2-bar-width :initform 0 :accessor p2-bar-width)))

(defmethod render-with-scores ((this score-bar) score-p1 score-p2)

  (let* ((p1-percentage (float (/ score-p1 (+ score-p1 score-p2)))))
    (setf (p1-bar-width this)
          (* (width this) p1-percentage))
    (setf (p2-bar-width this) (- (width this) (p1-bar-width this))))
  (render this))

(defmethod render ((this score-bar))
  (with-pushed-canvas ()
    (draw-rect (origin this) (width this) (height this) :stroke-paint *grid-color*)
    (draw-rect (origin this) (p1-bar-width this) (height this) :fill-paint *p1-color* :stroke-paint *grid-color*)
    (let* ((p2-bar-x (+ (x (origin this)) (p1-bar-width this)))
          (origin-p2-bar (vec2 p2-bar-x (y (origin this)))))
      (draw-rect origin-p2-bar (p2-bar-width this) (height this) :fill-paint *p2-color*))))
