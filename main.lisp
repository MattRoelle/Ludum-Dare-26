(require 'lispbuilder-sdl)
(require 'cl-opengl)

(setq player '(0 448 32 32))
(setq lives 3)
(setq bullets (list))
(setq enemies (list))

(defmacro restartable (&body body)
    `(restart-case
      (progn ,@body)
      (continue () :report "Continue")))

;expected rect definition (x y width height)
(defun rect-collide? (r1 r2)
  (cond
    ((< (+ (second r1) (fourth r1)) (second r2)) nil)
    ((> (second r1) (+ (second r2) (fourth r2))) nil)
    ((< (+ (first r1) (third r1)) (first r2)) nil)
    ((> (first r1) (+ (first r2) (third r2))) nil)
    (t t)))

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

(defun logic ()
  ;logic
  
  (when (sdl:get-key-state :sdl-key-right)
     (setf (first player) (+ (first player) 6)))
  (when (sdl:get-key-state :sdl-key-left)
     (setf (first player) (- (first player) 6)))
  (when (sdl:get-key-state :sdl-key-space)
     (setq bullets (append 
                     bullets
                     (list (list (+ 12 (first player)) (second player) 8 16)))))
    
  (map 'list #'(lambda (i)
           (if (or (<= (second i) (- 16)) (>= (second i) 640))
             (setq bullets (remove i bullets))
             (setf (second i) (- (second i) 8))))
       bullets)
  
  ;rendering
  (gl:clear :color-buffer-bit)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (apply 'drawq player)
    (gl:color 0 1 0)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          bullets)
    (gl:color 1 0 0)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          enemies))
  (gl:flush)
  (sdl:update-display)
  (sleep (/ 60 10000)))

(setq enemies (append enemies (list '(100 100 32 32))))

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    (initgl)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       #+(and sbcl (not sb-thread)) (restartable
                                      (sb-sys:serve-all-events 0))
       (restartable (logic))))))

(main)
