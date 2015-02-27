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
  (bubbles))
(defun write-javascript ()
  (with-open-file (outfile (format nil "~abubbles.js" (cl-ivy:resource-path "."))
                           :direction :output
                           :if-exists :supersede)
    (write-string (bubbles) outfile)))

(defmacro+ps new-sprite (texture)
  `(new (chain -p-i-x-i (-sprite ,texture))))
(defun balloons ()
  (ps
    
    (defvar all-balloons (new (-array)))

    (defvar normal-scale 0.5)
    (defvar large-scale 1)
    (defvar window-width (- (@  window inner-width) 50))
    (defvar window-height (- (@ window inner-height) 50))

    (defvar stage (new (chain -p-i-x-i (-stage 0xffffff t))))
    (setf (@ stage interactive) true)
    (defvar renderer (chain -p-i-x-i (auto-detect-renderer window-width window-height)))
    (chain document body (append-child (chain renderer view)))
    (request-anim-frame animate)
    (defvar background (chain -p-i-x-i -sprite (from-image "img/background.jpg")))
    (chain stage (add-child background))
    (defvar balloon (chain -p-i-x-i -texture (from-image "img/balloon_red.png")))
    (defvar clickified (chain -p-i-x-i -texture (from-image "img/balloon_red_clicked.png")))
    (defvar bbclicked (new (chain -p-i-x-i (-sprite clickified))))


    (defun animate ()
      (chain renderer (render stage)))

    (defun random-range (min max)
      (+ min (chain -math (floor (* (chain -math (random)) (+ 1 (- max min)))))))

    (defun drop-balloon (the-balloon)
      (setf (@ the-balloon position x) (random-range 1 window-width))
      (setf (@ the-balloon position y) (random-range 600 window-height)))
    
    (defun make-bubble ()
      (let ((the-balloon (new-sprite balloon)))
        (chain the-balloon scale (set normal-scale))
        (setf (@ the-balloon anchor x) 0.5)
        (setf (@ the-balloon anchor y) 0.5)
        (setf (@ the-balloon interactive) t)
        (drop-balloon the-balloon)
        
        (interactive_setup the-bubble)
        (chain stage (add-child the-bubble))
        (chain balloons (push the-bubble))))

    (defun is-collision (first second)
      (not (or (> second.x (+ first.x first.width))
               (< (+ second.x second.width) first.x)
               (> second.y (+ first.y first.height))
               (< (+ second.y second.height) first.y))))
    
    (defun animate ()
      (chain _ (map all-balloons (lambda (bubble)
                                   (progn
                                     (chain _ (map all-balloons (lambda (this-balloon)
                                                                  (if (is-collision bubble this-balloon)
                                                                      (chain bubble (set-texture clickified))
                                                                      (chain bubble (set-texture balloon))))))
                                     (if (> -100 bubble.position.y)
                                         (setf bubble.position.y (random-range 600 5000))
                                         (setf bubble.position.y (- bubble.position.y .5)))
                                     (when (= (@ bubble blowy) "left")
                                       (setf bubble.position.x (- bubble.position.x .2)))
                                     (when (= (@ bubble blowy) "right")
                                       (setf bubble.position.x (- bubble.position.x -0.2)))
                                     (when (> -10 bubble.position.x)
                                       (drop-balloon bubble))))))
      (chain renderer (render stage))
      (request-anim-frame animate))
    
    (defun interactive_setup (thing)
      (setf (chain thing click) (lambda (data)
                                  (setf (chain this is_clicked) 1)
                                  (chain this (set-texture clickified)))))
    (dotimes (x 100)
      (make-bubble))))
