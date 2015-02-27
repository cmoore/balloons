(in-package #:bubbles)

(defvar *listener* nil)
(defun start-server ()
  (unless *listener*
    (setf *listener* (make-instance 'hunchentoot:easy-acceptor
                                    :document-root (cl-ivy:resource-path ".")
                                    :port 8080))
    (hunchentoot:start *listener*)))
(defun stop-server ()
  (when *listener*
    (hunchentoot:stop *listener*)))
(defun reset-dispatch-table ()
  (setf *dispatch-table* (list #'dispatch-easy-handlers)))
(define-easy-handler (serve-js :uri "/js") ()
  (setf (content-type*) "application/json")
  (balloons))
(defun write-javascript ()
  (with-open-file (outfile (format nil "~abubbles.js" (cl-ivy:resource-path "."))
                           :direction :output
                           :if-exists :supersede)
    (write-string (bubbles) outfile)))

(defmacro+ps new-sprite (texture)
  `(new (chain -p-i-x-i (-sprite ,texture))))

(defmacro+ps set-texture (object texture)
  `(chain ,object (set-texture ,texture)))

(defmacro+ps make-balloon ()
  (with-ps-gensyms (ball)
    `(let ((,ball (new-sprite balloon-texture)))
       (chain ,ball scale (set normal-scale))
       (setf (@ ,ball anchor x) 0.5)
       (setf (@ ,ball to-reset) 0)
       (setf (@ ,ball anchor y) 0.5)
       (setf (@ ,ball interactive) t)
       (drop-balloon ,ball)
       
       (setf (chain ,ball click) (lambda (data)
                                   (chain this (set-texture balloon-clicked-texture))))
       (chain stage (add-child ,ball))
       (chain all-balloons (push ,ball)))))

(defun balloons ()
  (ps
    (defvar all-balloons (new (-array)))

    (defvar normal-scale 0.5)
    (defvar large-scale 1)
    
    (defvar window-width (- (@  window inner-width) 15))
    (defvar window-height (- (@ window inner-height) 18))

    (defvar stage (new (chain -p-i-x-i (-stage 0xffffff t))))
    (setf (@ stage interactive) true)
    
    (defvar renderer (chain -p-i-x-i (auto-detect-renderer window-width window-height)))
    (chain document body (append-child (chain renderer view)))
    
    (request-anim-frame animate)
    
    (defvar background (chain -p-i-x-i -sprite (from-image "img/background.jpg")))
    (chain stage (add-child background))
    
    (defvar balloon-texture (chain -p-i-x-i -texture (from-image "img/balloon_red.png")))
    (defvar balloon-clicked-texture (chain -p-i-x-i -texture (from-image "img/balloon_red_clicked.png")))
    
    

    (defun random-range (min max)
      (+ min (chain -math (floor (* (chain -math (random)) (+ 1 (- max min)))))))

    (defun drop-balloon (the-balloon)
      (setf (@ the-balloon position x) (random-range 1 window-width))
      (setf (@ the-balloon position y) (random-range window-height 5000)))

    (defun are-colliding (first second)
      (chain first (get-local-bounds) (contains second.x second.y)))
    (defun is-collision (first second)
      (not (or (> second.x (+ first.x first.width))
               (< (+ second.x second.width) first.x)
               (> second.y (+ first.y first.height))
               (< (+ second.y second.height) first.y))))
    
    (defun animate ()
      (chain _ (map all-balloons (lambda (first-balloon)
                                   (chain _ (map all-balloons (lambda (second-balloon)
                                                                (if (are-colliding first-balloon second-balloon)
                                                                    (set-texture first-balloon balloon-clicked-texture)
                                                                    (set-texture first-balloon balloon-texture)))))
                                   
                                   (when (> -100 (@ first-balloon position y))
                                     (drop-balloon first-balloon))
                                   
                                   (setf (@ first-balloon position y) (- (@ first-balloon position y) .5))
                                   
                                   (when (> -10 (@ first-balloon position x))
                                     (drop-balloon first-balloon)))))
      (chain renderer (render stage))
      (request-anim-frame animate))
    
    (dotimes (x 100)
      (make-balloon))))
