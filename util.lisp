(cl:in-package :gridgreed)

(defvar *black* (vec4 0 0 0 1))
(defvar *red* (vec4 1 0 0 1))
(defvar *green* (vec4 0 1 0 1))
(defvar *blue* (vec4 0 0 1 1))
(defvar *white* (vec4 1 1 1 1))

(defparameter *grid-color* (vec4 0.6 0.6 0.6 0))

;(defparameter *p1-color* (vec4 .5 .3 .9 1))
;(defparameter *p2-color* (vec4 .9 .7 .2 1))

(defparameter *p1-color* (vec4 .3 .7 .99 1))
(defparameter *p2-color* (vec4 .92 .13 .33 1))

(defvar *directions* '(:north :south :west :east :north-east :north-west :south-east :south-west))

(defparameter *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *origin* (vec2 0 0))
(defvar *center* (vec2 (/ *canvas-width* 2) (/ *canvas-height* 2)))

(defparameter *cell-size* 40.5)

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun range (min max)
   (loop :for n :from min :to max
      collect n))

(defun create-vec4-copy (v)
  (vec4 (x v) (y v) (z v) (w v)))
