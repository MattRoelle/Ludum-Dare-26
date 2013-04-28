(require 'lispbuilder-sdl)
(require 'cl-opengl)

(setq grid (make-hash-table))

(defun init-grid ()
  (loop
    for x from 0 to 20
    do (loop
         for y from 0 to 15
         do
           (setf (gethash (+ (* x 15) y) grid) (list
                                                 'neutral
                                                 0
                                                 (cons x y))))))

(defun get-grid-key (x y)
  (+ (* x 15) y))

(defmacro restartable (&body body)
    `(restart-case
      (progn ,@body)
      (continue () :report "Continue")))

(defun initgl ()
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:viewport 0 0 640 480)
    (gl:ortho 0 640 480 0 0 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity))

(defun drawq (x y w h)
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:with-primitive :quads
      (gl:vertex 0 0)
      (gl:vertex w 0)
      (gl:vertex w h)
      (gl:vertex 0 h))))

(defun draw ()
    (gl:clear :color-buffer-bit)
    
    (gl:with-pushed-matrix
      (maphash #'(lambda (h k)
                   (cond
                     ((equal (first k) 'neutral)
                      (gl:color 1 1 1)
                      (drawq (* 32 (car (nth 2 k))) (* 32 (cdr (nth 2 k))) 32 32))
                     (t
                      (gl:color 0 0 1)
                      (drawq (* 32 (car (nth 2 k))) (* 32 (cdr (nth 2 k))) 32 32))))
               grid))
    
    (gl:flush)
    (sdl:update-display))

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    (initgl)
    (init-grid)
    (setf (gethash (get-grid-key 2 3) grid) (list
                                              'player
                                              0
                                              (cons 2 3)))
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       #+(and sbcl (not sb-thread)) (restartable
                                      (sb-sys:serve-all-events 0))
       (restartable (draw))))))

(main)
