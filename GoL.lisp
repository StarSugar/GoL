(ql:quickload "sdl2")

(defpackage "GOL"
  (:use "COMMON-LISP"))

(in-package "GOL")

(defparameter *screen-width* 800)
(defparameter *screen-height* 800)

(defparameter *scalar* 4)

(defparameter *max-x* (/ *screen-width* *scalar*))
(defparameter *max-y* (/ *screen-height* *scalar*))

(declaim (type (array bit) *map* *map2*))
(defparameter *map* (make-array (list *max-x* *max-y*) :element-type 'bit
                                :initial-element 0))
(defparameter *map2* (make-array (list *max-x* *max-y*) :element-type 'bit
                                 :initial-element 0))

(defparameter *delay* 1/20)

(declaim (inline -1+))
(defun -1+ (x)
  (- x 1))

(defmacro fx (x)
  `(the fixnum ,x))

;; middle of map
(declaim (inline neighbors-1))
(defun neighbors-1 (x y)
  (declare (type fixnum x y))
  (list (list (fx (-1+ x)) (fx (-1+ y)))
        (list (fx (-1+ x)) (fx y))
        (list (fx (-1+ x)) (fx (1+ y)))
        (list (fx x)       (fx (-1+ y)))
        (list (fx x)       (fx (1+ y)))
        (list (fx (1+ x))  (fx (-1+ y)))
        (list (fx (1+ x))  (fx y))
        (list (fx (1+ x))  (fx (1+ y)))))

;; edges of map
(declaim (inline neighbors-2))
(defun neighbors-2 (y)
  (declare (type fixnum y))
  (list (list 0 (fx (- y 1)))
        (list 1 (fx (- y 1)))
        (list 1 (fx y))
        (list 1 (fx (+ y 1)))
        (list 0 (fx (+ y 1)))))

(declaim (inline neighbors-3))
(defun neighbors-3 (max-x y)
  (declare (type fixnum max-x y))
  (list (list (fx (- max-x 1)) (fx (- y 1)))
        (list (fx (- max-x 2)) (fx (- y 1)))
        (list (fx (- max-x 2)) (fx y))
        (list (fx (- max-x 2)) (fx (+ y 1)))
        (list (fx (- max-x 1)) (fx (+ y 1)))))

(declaim (inline neighbors-4))
(defun neighbors-4 (x)
  (declare (type fixnum x))
  (list (list (fx (- x 1))  0)
        (list (fx (- x 1))  1)
        (list (fx x)        1)
        (list (fx (+ x 1))  1)
        (list (fx (+ x 1))  0)))

(declaim (inline neighbors-5))
(defun neighbors-5 (x max-y)
  (declare (type fixnum x max-y))
  (list (list (fx (- x 1))  (fx (- max-y 1)))
        (list (fx (- x 1))  (fx (- max-y 2)))
        (list (fx x)        (fx (- max-y 2)))
        (list (fx (+ x 1))  (fx (- max-y 2)))
        (list (fx (+ x 1))  (fx (- max-y 1)))))

;; corners of map
(declaim (inline neighbors-6))
(defun neighbors-6 ()
  (list (list 0 1)
        (list 1 0)
        (list 1 1)))

(declaim (inline neighbors-7))
(defun neighbors-7 (max-y)
  (declare (type fixnum max-y))
  (list (list 0 (- max-y 2))
        (list 1 (- max-y 1))
        (list 1 (- max-y 2))))

(declaim (inline neighbors-8))
(defun neighbors-8 (max-x)
  (declare (type fixnum max-x))
  (list (list (- max-x 2) 0)
        (list (- max-x 1) 1)
        (list (- max-x 2) 1)))

(declaim (inline neighbors-9))
(defun neighbors-9 (max-x max-y)
  (declare (type fixnum max-x max-y))
  (list (list (- max-x 2) (- max-y 1))
        (list (- max-x 1) (- max-y 2))
        (list (- max-x 2) (- max-y 2))))

(defun update ()
  "update the map one step"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((gmap *map*) (gmap2 *map2*) (max-x *max-x*) (max-y *max-y*))
    (declare (type (array bit) gmap gmap2)
             (type fixnum max-x max-y))
    (labels ((sum-of-points (points map)
               (loop for (x y) fixnum in points sum (bit map x y) fixnum))
             (update-at (x y points)
               (setf (bit gmap2 x y)
                     (let ((self (bit gmap x y)))
                       (case (+ self (sum-of-points points gmap))
                         (3 1)
                         (4 self)
                         (otherwise 0))))))
      (loop for x fixnum from 1 to (- max-x 2) do
            (loop for y from 1 to (- max-y 2) do
                  (update-at x y (neighbors-1 x y))))
      (loop for y fixnum from 1 to (- max-y 2) do
            (update-at 0 y (neighbors-2 y))
            (update-at (- max-x 1) y (neighbors-3 max-x y)))
      (loop for x fixnum from 1 to (- max-x 2) do
            (update-at x 0 (neighbors-4 x))
            (update-at x (- max-y 1) (neighbors-5 x max-y)))
      (update-at 0 0 (neighbors-6))
      (update-at 0 (- max-y 1) (neighbors-7 max-x))
      (update-at (- max-x 1) 0 (neighbors-8 max-y))
      (update-at (- max-x 1) (- max-y 1) (neighbors-9 max-x max-y))))
  (psetf *map* *map2*
         *map2* *map*))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Game of Life"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun renderer-map (renderer)
  ;; make it white
  (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
  (sdl2:render-clear renderer)
  ;; draw cells
  (loop for x from 0 below *max-x* do
        (loop for y from 0 below *max-y* do
              (when (= 1 (aref *map* x y))
                (sdl2:with-rects ((fill-rect (* x *scalar*) (* y *scalar*)
                                             *scalar* *scalar*))
                  ;; use black
                  (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #xFF)
                  (sdl2:render-fill-rect renderer fill-rect)))))
  (sdl2:render-present renderer))

(defun edit-mode (renderer)
  (psetf *map* *map2*
         *map2* *map*)
  (labels ((update ()
             (multiple-value-bind (x y)
               (sdl2:mouse-state)
               (setf x (truncate x *scalar*)
                     y (truncate y *scalar*))
               (setf (bit *map* x y) (- 1 (bit *map* x y)))
               (renderer-map renderer))))
    (update)
    (pause-loop renderer
      (block exit
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:idle () t)
          (:mousebuttondown ()
            (update))
          (:keydown (:keysym keysym)
            (return-from exit (sdl2:scancode keysym))))))))

(defun run-step (renderer)
  (renderer-map renderer)
  (update))

(defun pause-loop (renderer &optional initial-keycode &aux switchp)
  ;; there can be only sdl2:with-event-loop active, so use switchp to
  ;; avoid stack increasement
  (block exit
    (labels ((handle-keydown (keycode)
               (case keycode
                 (:scancode-space
                   (setf switchp :run)
                   (return-from exit))
                 ((:scancode-right :scancode-return)
                  (run-step renderer)))))
      (handle-keydown initial-keycode)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle () )
        (:keydown (:keysym keysym)
          (handle-keydown (sdl2:scancode keysym)))
        (:mousebuttondown ()
          (setf switchp :edit)
          (return-from exit)))))
  (case switchp
    (:run (run-loop renderer))
    (:edit (edit-mode renderer))))

(defun run-loop (renderer &aux switchp)
  (block exit
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle () (run-step renderer) (sleep *delay*))
      (:keydown (:keysym keysym)
        (case (sdl2:scancode keysym)
          (:scancode-space
            (setf switchp t)
            (return-from exit))))))
  (when switchp
    (pause-loop renderer)))

(defun main-loop ()
  (with-window-renderer (window renderer)
    (run-step renderer) ; show initial map
    (pause-loop renderer)))

(setf (aref *map* 20 20) 1)
(setf (aref *map* 20 21) 1)
(setf (aref *map* 19 21) 1)
(setf (aref *map* 20 22) 1)
(setf (aref *map* 21 22) 1)

(format t "space: pause/continue~%right/return: step when pause~%mouse click to edit when pause~%")

#+sbcl (sdl2:make-this-thread-main (lambda () (main-loop)))
#-sbcl (main-loop)

#+sbcl (sb-ext:exit)
#+ccl (quit)
