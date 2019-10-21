(cl:in-package :gridgreed)

(defgame gridgreed ()
  ((rows :initform 13 :accessor rows)
   (cols :initform 19 :accessor cols)
   (grid :accessor grid)
   (grid-next :accessor grid-next)
   (p1 :initform (make-instance 'player :name "Player 1" :color (create-vec4-copy *p1-color*)) :accessor p1)
   (p2 :initform (make-instance 'player :name "Player 2" :color (create-vec4-copy *p2-color*)) :accessor p2)
   (last-tick :initform 0 :accessor last-tick)
   (tick-duration :initform 0.2 :reader tick-duration)
   (last-fast-tick :initform 0 :accessor fast-last-tick)
   (fast-tick-duration :initform 0.5 :reader fast-tick-duration)
   (state :initform :playing :accessor state)
   (countdown-start-time :accessor countdown-start-time)
   (score-bar
    :initform (make-instance 'score-bar :height 10 :width 771 :origin (vec2 8 560))
    :accessor score-bar))

  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Grid of Greed"))

(defun print-debug (app)
  ;(format t "VALUE WE GOT = ~A~%" (get-neighbors-of-list (grid app) (get-neighbors (grid app) 7 9) T))
  ;(setf (owner (get-cell (grid app) 7 9)) (p1 app))
  ;(dolist (n (get-neighbors-of-list (grid app) (get-neighbors (grid app) 7 9)))
  ;  (setf (strength n) 4.2)))
  (format t "~a~%" (valid-cell-p (grid app) 9 7))
  (format t "BUT... ~a~%" (strength (aref (cells (grid app)) 7 9))))

(defmethod post-initialize ((this gridgreed))

  (setf (grid this) (make-instance 'grid :rows (rows this) :cols (cols this)))
  (setf (grid-next this) (make-instance 'grid :rows (rows this) :cols (cols this)))

  (restart-level this)

  (setf (state this) :countdown)
  (setf (countdown-start-time this) (real-time-seconds))

  (bind-button :r :pressed (lambda () (restart-level this)))
  (bind-button :p :pressed (lambda () (print-debug this)))

  (bind-button :escape :pressed #'gamekit:stop)

  (bind-button :w :pressed (lambda () (move-player this (p1 this) :up)))
  (bind-button :w :repeating (lambda () (move-player this (p1 this) :up)))

  (bind-button :s :pressed (lambda () (move-player this (p1 this) :down)))
  (bind-button :s :repeating (lambda () (move-player this (p1 this) :down)))

  (bind-button :a :pressed (lambda () (move-player this (p1 this) :left)))
  (bind-button :a :repeating (lambda () (move-player this (p1 this) :left)))

  (bind-button :d :pressed (lambda () (move-player this (p1 this) :right)))
  (bind-button :d :repeating (lambda () (move-player this (p1 this) :right))))

  (bind-button :up :pressed (lambda () (move-player this (p2 this) :up)))
  (bind-button :up :repeating (lambda () (move-player this (p2 this) :up)))

  (bind-button :down :pressed (lambda () (move-player this (p2 this) :down)))
  (bind-button :down :repeating (lambda () (move-player this (p2 this) :down)))

  (bind-button :left :pressed (lambda () (move-player this (p2 this) :left)))
  (bind-button :left :repeating (lambda () (move-player this (p2 this) :left)))

  (bind-button :right :pressed (lambda () (move-player this (p2 this) :right)))
  (bind-button :right :repeating (lambda () (move-player this (p2 this) :right)))

(defmethod draw ((app gridgreed))
  (with-pushed-canvas ()
    (translate-canvas 10 10)
    (with-slots (grid p1 p2 score-bar state) app
      (render grid)
      (render p1)
      (render p2)
      (render-with-scores score-bar (owned-count p1) (owned-count p2))

      (when (eql state :countdown)
        (let* ((text-pos (vec2 310 310))
               (rect-width 120)
               (rect-height 45)
               (rect-origin (vec2 300 292))
               (seconds-passed (round (- (real-time-seconds) (countdown-start-time app))))
               (seconds-left (- 2 seconds-passed))
               (message (format nil "Get ready: ~A" seconds-left)))
            (draw-rect rect-origin rect-width rect-height :fill-paint *white* :stroke-paint *black*)
            (draw-text message text-pos)))

      (when (eql state :game-over)
        (let* ((winner (check-winning-condition app))
               (winner-name (name winner))
               (message (format nil "~A is the winner!" winner-name)))
        (multiple-value-bind (_ text-width) (calc-text-bounds message)
          (declare (ignore _))
          (let ((text-pos-1 (vec2 300 310))
                (text-pos-2 (vec2 300 290))
                (rect-width (+ text-width 60))
                (rect-height 80)
                (rect-origin (vec2 280 265))
                (color (if (eql winner p1) *p1-color* *p2-color*)))
            (draw-rect rect-origin rect-width rect-height :fill-paint *white* :stroke-paint *black*)
            (draw-text message text-pos-1 :fill-color color)
            (draw-text "Press R to restart" text-pos-2))))))))

(defun player-influence-tick (app player)
  (let* ((strength-0 (* 30 (/ (owned-count player) 90)))
         (strength-1 (round (/ strength-0 4)))
         (strength-2 (round (/ strength-0 5)))
         (strength-3 (round (/ strength-0 6)))
         (strength-4 (/ strength-0 15))
         (strength-5 (/ strength-0 20))
         (cell-ticks (cell-ticks player))
         (cell-ticks-modulo (mod cell-ticks 11))
         (grid (grid app)))

    (when (eq cell-ticks-modulo 0)
      (reset-cells-touched (grid app))
      (influence-cell app (get-cell (grid app) (ypos player) (xpos player)) player strength-0))
    (when (eq cell-ticks-modulo 1)
      (influence-neighbors app (get-cell grid (ypos player) (xpos player)) player strength-1 1))
    (when (eq cell-ticks-modulo 3)
      (influence-neighbors app (get-cell grid (ypos player) (xpos player)) player strength-2 2))
    (when (eq cell-ticks-modulo 5)
      (influence-neighbors app (get-cell grid (ypos player) (xpos player)) player strength-3 3))
    (when (eq cell-ticks-modulo 8)
      (influence-neighbors app (get-cell grid (ypos player) (xpos player)) player strength-4 4))
    (when (eq cell-ticks-modulo 10)
      (influence-neighbors app (get-cell grid (ypos player) (xpos player)) player strength-5 5)
      (decay-untouched-cells (grid app) player 1))))

(defmethod act-player ((app gridgreed) (player player))
  (let* ((now (real-time-seconds))
         (cell-time (- now (cell-time player))))
    (when (> cell-time 0.01)
      (set-cell-time-now player)
      (incf (cell-ticks player))
      (player-influence-tick app player))))

(defmethod act ((app gridgreed))
  (when (and (check-winning-condition app)
             (eql (state app) :playing))
    (setf (state app) :game-over))

  (when (eql (state app) :countdown)
    (let* ((time-passed (- (real-time-seconds) (countdown-start-time app)))
           (time-left (- 2 time-passed)))
      (when (<= time-left 0)
        (setf (state app) :playing))))

  (when (eql (state app) :playing)
    (with-slots (p1 p2) app
      (act-player app p1)
      (act-player app p2)))

  (with-slots (last-tick tick-duration grid) app
    (when (> (real-time-seconds) (+ last-tick tick-duration))
      (tick app)
      (setf last-tick (real-time-seconds)))))

(defun influence-cell (app cell player amount)
  (if (eq (owner cell) player)
      (strengthen-cell cell amount)
      (progn
        (weaken-cell cell amount)
        (when (zerop (strength cell))
          (setf (strength cell) 0)
          (setf (touched cell) T)
          (reset-cell-time player)
          (player-capture-cell app player cell)))))

(defun influence-neighbors (app cell player amount distance)
  (dolist (n (get-neighbors (grid app) (row cell) (col cell)
                            :distance distance))
    (if (eq (owner n) player)
        (strengthen-cell n amount)
        (progn
          (when (owner n)
            (weaken-cell n amount))))))

(defun strengthen-cell (cell amount)
  (let ((cell-strength (strength cell)))
    (when (< cell-strength 100)
      (setf (touched cell) T)
      (setf (strength cell)
            (min (+ cell-strength amount)
                 100)))))

(defun weaken-cell (cell amount)
  (let ((cell-strength (strength cell)))
    (when (> cell-strength 0)
      (setf (touched cell) T)
      (setf (strength cell)
            (max (- cell-strength amount)
                 0)))))

(defmethod tick ((app gridgreed))
  (expand-territories app))

(defmethod get-strongest-player ((app gridgreed) cells)
  (let ((p1 (p1 app))
        (p2 (p2 app))
        (p1-strength 0)
        (p2-strength 0))

    (dolist (c cells)
      (when (eql (owner c) p1)
        (incf p1-strength (strength c)))
      (when (eql (owner c) p2)
        (incf p2-strength (strength c))))

    (if (and (zerop p1-strength) (zerop p2-strength))
        nil
        (if (> p1-strength p2-strength)
            p1
            p2))))

(defmethod expand-territories ((app gridgreed))
  (let ((changes '())
        (grid (grid app)))

    (dotimes (row (rows app))
      (dotimes (col (cols app))

        (let* ((cell (get-cell grid row col))
               (neighbors (get-neighbors grid row col))
               (strongest-player (get-strongest-player app neighbors)))
         
          (when (and strongest-player
                     (not (eql strongest-player (owner cell))))
            (setf changes (push (list cell strongest-player) changes))))))

    (dolist (change changes)
      (let ((cell (first change))
            (player (second change)))
        (when (<= (distance cell (get-cell grid (ypos player) (xpos player))) 5)
          (player-capture-cell app player cell))))))

(defmethod player-capture-cell ((app gridgreed) (player player) (cell cell))
      (when (eql (owner cell) (get-opponent-of app player))
        (decf (owned-count (get-opponent-of app player))))
      (when (not (eql (owner cell) player))
        (incf (owned-count player))
        (setf (owner cell) player)))

(defmethod get-opponent-of ((app gridgreed) player)
  (if (eql (p1 app) player)
      (p2 app)
      (p1 app)))

(defun play-game ()
  (start 'gridgreed))

(defmethod move-player ((app gridgreed) (player player) direction)
  (when (not (eql (state app) :playing))
    (return-from move-player nil))

  (when (and (eq direction :up) (< (ypos player) (1- (rows (grid app)))))
    (set-cell-time-now player)
    (reset-cell-time player)
    (setf (ypos player) (1+ (ypos player))))

  (when (and (eq direction :down) (> (ypos player) 0))
    (set-cell-time-now player)
    (reset-cell-time player)
    (setf (ypos player) (1- (ypos player))))

  (when (and (eq direction :left) (> (xpos player) 0))
    (set-cell-time-now player)
    (reset-cell-time player)
    (setf (xpos player) (1- (xpos player))))

  (when (and (eq direction :right) (< (xpos player) (1- (cols (grid app)))))
    (set-cell-time-now player)
    (reset-cell-time player)
    (setf (xpos player) (1+ (xpos player)))))

(defmethod check-winning-condition ((app gridgreed))
  (cond
    ((eq (owned-count (p1 app)) 0) (p2 app))
    ((eq (owned-count (p2 app)) 0) (p1 app))
    (t nil)))

(defmethod ai-move ((app gridgreed))
  (with-slots (p1 p2 grid) app
    (move-player app (p2 app) (nth (random 4) '(:up :down :left :right)))))

(defmethod restart-level ((app gridgreed))

  (dotimes (i (rows app))
    (dotimes (j (cols app))
      (setf (owner (get-cell (grid app) i j)) nil)
      (setf (strength (get-cell (grid app) i j)) 0)))

  (with-slots (p1 p2 grid) app

    (setf (owned-count p1) 1)
    (setf (owned-count p2) 1)

    (set-cell-owner grid 0 0 (p1 app))
    (setf (strength (get-cell grid 0 0)) 10)
    (setf (xpos p1) 0)
    (setf (ypos p1) 0)

    (reset-cell-time p1)
    (reset-cell-time p2)

    (let ((player2-y (- (rows app) 1))
          (player2-x (- (cols app) 1)))

      (set-cell-owner grid player2-y player2-x p2)
      (setf (strength (get-cell grid player2-y player2-x)) 10)

      (setf (xpos p2) player2-x)
      (setf (ypos p2) player2-y))

    (setf (state app) :countdown)
    (setf (countdown-start-time app) (real-time-seconds))))
