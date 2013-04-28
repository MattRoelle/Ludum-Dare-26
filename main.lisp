(require 'lispbuilder-sdl)
(require 'cl-opengl)

(setq player '(0 448 32 32))
(setq lives 3)
(setq bullets (list))
(setq enemies (list))
(setq ebullets (list))

(setq fire-time 0)


;enemy movement definitions
(setq wave #'(lambda (e)
               (setf (first e) (+ (first e) (* 7 (cos (/ (get-internal-real-time) 500)))))
               (setf (second e) (+ (second e) 1))))

(setq sprint #'(lambda (e)
                 (setf (second e) (+ (second e) 5))))

(setq right-strafe #'(lambda (e)
                       (cond
                         ((< (second e) 200)
                          (setf (second e) (+ (second e) 4))
                          (setf (first e) (+ (first e) .5)))
                         (t
                          (setf (first e) (+ (first e) 5))
                          (setf (second e) (+ (second e) .5))))))

(setq left-strafe #'(lambda (e)
                       (cond
                         ((< (second e) 200)
                          (setf (second e) (+ (second e) 4))
                          (setf (first e) (+ (first e) .5)))
                         (t
                          (setf (first e) (- (first e) 5))
                          (setf (second e) (+ (second e) .5))))))
;enemy shooting definitions
(setq semi-auto
      #'(lambda (e) 
          (when (>= (- (get-internal-real-time) (seventh e)) 1000)
                      (setq ebullets (append
                                      ebullets
                                      (list (list (+ 12 (first e)) (+ (second e) (fourth e)) 8 16))))
                      (setf (seventh e) (get-internal-real-time)))))

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
  
  (let ((keystate (sdl:get-keys-state)))  
    (cond
      
      ((and (find :sdl-key-right keystate) (find :sdl-key-up keystate))
       (setf (first player) (+ (first player) 5.5))
       (setf (second player) (- (second player) 5.5)))
      ((and (find :sdl-key-left keystate) (find :sdl-key-up keystate))
       (setf (first player) (- (first player) 5.5))
       (setf (second player) (- (second player) 5.5)))
      ((and (find :sdl-key-right keystate) (find :sdl-key-down keystate))
       (setf (first player) (+ (first player) 5.5))
       (setf (second player) (+ (second player) 5.5)))
      ((and (find :sdl-key-left keystate) (find :sdl-key-down keystate))
       (setf (first player) (- (first player) 5.5))
       (setf (second player) (+ (second player) 5.5)))     
      
      ((find :sdl-key-right keystate)
       (setf (first player) (+ (first player) 8)))
      ((find :sdl-key-left keystate)
       (setf (first player) (- (first player) 8))) 
      ((find :sdl-key-up keystate)
       (setf (second player) (- (second player) 8)))
      ((find :sdl-key-down keystate)
       (setf (second player) (+ (second player) 8))))
    
    (when (sdl:get-key-state :sdl-key-space)
         (when (>= (- (get-internal-real-time) fire-time) 200)
         (setq bullets (append
                         bullets
                         (list (list (+ 12 (first player)) (- (second player) 16) 8 16))))
        (setq fire-time (get-internal-real-time)))))
    
   (mapc #'(lambda (i)
           (if (or (<= (second i) (- 16)) (>= (second i) 640))
             (setq bullets (remove i bullets))
             (setf (second i) (- (second i) 8))))
       bullets) 
  
  (mapc #'(lambda (i)
           (if (or (<= (second i) (- 16)) (>= (second i) 640))
             (setq ebullets (remove i ebullets))
             (setf (second i) (+ (second i) 8))))
       ebullets) 
   
  (mapc #'(lambda (e)
            (funcall (fifth e) e)
            (funcall (sixth e) e)
            (mapc #'(lambda (b)
                      (cond
                        ((rect-collide? (subseq e 0 4) (subseq b 0 4))
                         (setq enemies (remove e enemies))
                         (setq bullets (remove b bullets)))))
                  bullets))
        enemies)
  
  ;rendering
  (gl:clear :color-buffer-bit)
  (gl:with-pushed-matrix
    (gl:color 0 0 0)
    (apply 'drawq player)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          bullets)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          ebullets)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          enemies))
  (gl:flush)
  (sdl:update-display)
  (sleep (/ 60 10000)))

(setq enemies (append enemies (list (list 300 100 32 32 right-strafe semi-auto 0))))

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    (initgl)
    (gl:clear-color 1 1 1 1)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       #+(and sbcl (not sb-thread)) (restartable
                                      (sb-sys:serve-all-events 0))
       (restartable (logic))))))

(main)
