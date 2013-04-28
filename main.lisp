(require 'lispbuilder-sdl)
(require 'cl-opengl)

(defvar *player* '(0 448 16 16))
(defvar *lives* 3)
(defvar *bullets* (list))
(defvar *enemies* (list))
(defvar *ebullets* (list))

(defvar *fire-time* 0)

(setq *enemies* ())
(setq *ebullets* ())

;enemy movement definitions
(defun wave (e)
  (setf (first e) (+ (first e) (* 7 (cos (/ (get-internal-real-time) 500)))))
  (setf (second e) (+ (second e) 1)))

(defun sprint (e)
  (setf (second e) (+ (second e) 5)))

(defun right-strafe (e)
  (cond
    ((< (second e) 200)
     (setf (second e) (+ (second e) 4))
     (setf (first e) (+ (first e) .5)))
    (t
     (setf (first e) (+ (first e) 5))
     (setf (second e) (+ (second e) .5)))))

(defun left-strafe (e)
  (cond
    ((< (second e) 200)
     (setf (second e) (+ (second e) 4))
     (setf (first e) (+ (first e) .5)))
    (t
     (setf (first e) (- (first e) 5))
     (setf (second e) (+ (second e) .5)))))
;enemy shooting definitions
(defun semi-auto (e) 
  (when (>= (- (get-internal-real-time) (seventh e)) 1000)
    (setq *ebullets* (append
                     *ebullets*
                     (list (list (+ 8 (first e)) (+ (second e) (fourth e)) 4 8 regular 0))))
    (setf (seventh e) (get-internal-real-time))))

(defun branch (e) 
  (when (>= (- (get-internal-real-time) (seventh e)) 1000)
    (mapc #'(lambda (angle)
              (setq *ebullets* (append
                               *ebullets*
                               (list (list (+ 8 (first e)) (+ (second e) (fourth e)) 4 8
                                           angled
                                           angle))))) 
          '(.87 1.17 1.57 1.97 2.27))
    (setf (seventh e) (get-internal-real-time))))

(defun small-branch (e) 
  (when (>= (- (get-internal-real-time) (seventh e)) 1000)
    (mapc #'(lambda (angle)
              (setq *ebullets* (append
                               *ebullets*
                               (list (list (+ 8 (first e)) (+ (second e) (fourth e)) 4 8
                                           angled
                                           angle))))) 
          '(1.17 1.57 1.97))
    (setf (seventh e) (get-internal-real-time))))

;bullet movement definitions
(defun regular (b)
  (setf (second b) (+ (second b) 7)))

(defun angle (b)
  (let* ((angle (sixth b))
         (dx (* 7 (cos angle)))
         (dy (* 7 (sin angle))))
    (setf (first b) (+ (first b) dx))
    (setf (second b) (+ (second b) dy))))

;enemy wave generation
(defun sprint-3 ()
 (setq *enemies* (append *enemies* (list (list (- 288 64) -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list 288 -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list (+ 288 64) -32 32 32 'sprint 'semi-auto 0)))))

(defun sprint-5 ()
 (setq *enemies* (append *enemies* (list (list (- 288 128) -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list (- 288 64) -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list 288 -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list (+ 288 64) -32 32 32 'sprint 'semi-auto 0)))) 
 (setq *enemies* (append *enemies* (list (list (+ 288 128) -32 32 32 'sprint 'semi-auto 0)))))

(defun branch-2-small ()
 (setq *enemies* (append *enemies* (list (list 32 -32 32 32 'right-strafe 'small-branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -32 32 32 'left-strafe 'small-branch 0)))))

(defun branch-2 ()
 (setq *enemies* (append *enemies* (list (list 32 -32 32 32 'right-strafe 'branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -32 32 32 'left-strafe 'branch 0)))))

(defun branch-4-small ()
 (setq *enemies* (append *enemies* (list (list 32 -128 32 32 'right-strafe 'small-branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 32 -32 32 32 'right-strafe 'small-branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -128 32 32 'left-strafe 'small-branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -32 32 32 'left-strafe 'small-branch 0)))))

(defun branch-4 ()
 (setq *enemies* (append *enemies* (list (list 32 -128 32 32 'right-strafe 'branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 32 -32 32 32 'right-strafe 'branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -128 32 32 'left-strafe 'branch 0)))) 
 (setq *enemies* (append *enemies* (list (list 448 -32 32 32 'left-strafe 'branch 0)))))


(branch-4)

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
       (setf (first *player*) (+ (first *player*) 5.5))
       (setf (second *player*) (- (second *player*) 5.5)))
      ((and (find :sdl-key-left keystate) (find :sdl-key-up keystate))
       (setf (first *player*) (- (first *player*) 5.5))
       (setf (second *player*) (- (second *player*) 5.5)))
      ((and (find :sdl-key-right keystate) (find :sdl-key-down keystate))
       (setf (first *player*) (+ (first *player*) 5.5))
       (setf (second *player*) (+ (second *player*) 5.5)))
      ((and (find :sdl-key-left keystate) (find :sdl-key-down keystate))
       (setf (first *player*) (- (first *player*) 5.5))
       (setf (second *player*) (+ (second *player*) 5.5)))     
      
      ((find :sdl-key-right keystate)
       (setf (first *player*) (+ (first *player*) 8)))
      ((find :sdl-key-left keystate)
       (setf (first *player*) (- (first *player*) 8))) 
      ((find :sdl-key-up keystate)
       (setf (second *player*) (- (second *player*) 8)))
      ((find :sdl-key-down keystate)
       (setf (second *player*) (+ (second *player*) 8))))
    
    (when (sdl:get-key-state :sdl-key-space)
         (when (>= (- (get-internal-real-time) *fire-time*) 200)
         (setq *bullets* (append
                         *bullets*
                         (list (list (+ 4 (first *player*)) (- (second *player*) 16) 8 16))))
        (setq *fire-time* (get-internal-real-time)))))
    
   (mapc #'(lambda (i)
           (if (or (<= (second i) (- 16)) (>= (second i) 640))
             (setq *bullets* (remove i *bullets*))
             (setf (second i) (- (second i) 8))))
       *bullets*) 
  
  (mapc #'(lambda (i)
           (if (or (<= (second i) (- 16)) (>= (second i) 640))
             (setq *ebullets* (remove i *ebullets*))
             (funcall (fifth i) i)))
       *ebullets*) 
   
  (mapc #'(lambda (e)
            (funcall (fifth e) e)
            (funcall (sixth e) e)
            (mapc #'(lambda (b)
                      (cond
                        ((rect-collide? (subseq e 0 4) (subseq b 0 4))
                         (setq *enemies* (remove e *enemies*))
                         (setq *bullets* (remove b *bullets*)))))
                  *bullets*))
        *enemies*)
  
  ;rendering
  (gl:clear :color-buffer-bit)
  (gl:with-pushed-matrix
    (gl:color 0 0 0)
    (apply 'drawq *player*)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          *bullets*)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          *ebullets*)
    (mapc #'(lambda (i)
              (apply 'drawq (subseq i 0 4)))
          *enemies*))
  (gl:flush)
  (sdl:update-display)
  (sleep (/ 60 10000)))

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
