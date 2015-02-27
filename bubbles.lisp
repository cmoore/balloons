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

(define-easy-handler (serve-ps-js :uri "/psjs") ()
  (setf (content-type*) "text/javascript")
  (ps* *ps-lisp-library*))

(define-easy-handler (serve-js :uri "/js") ()
  (setf (content-type*) "text/javascript")
  (balloons))

(defun write-javascript ()
  (with-open-file (outfile (format nil "~abubbles.js" (cl-ivy:resource-path "."))
                           :direction :output
                           :if-exists :supersede)
    (write-string (bubbles) outfile)))



(defmacro+ps add-balloon ()
  (with-ps-gensyms (ball)
    `(progn
       (defvar ,ball (chain game add (sprite 0 0 "balloon")))
       
       (chain game physics (enable ,ball (chain -phaser -physics -a-r-c-a-d-e)))
       (setf (@ ,ball check-world-bounds) t)
       (chain ,ball body bounce (set 1))

       (setf (chain ,ball input-enabled) t)
       (chain ,ball events on-input-down (add click-handler ,ball))
       
       (setf (chain ,ball body velocity y) (random-range -10 -20))
      
       (chain ,ball scale (set 0.5))
       (chain all-balloons (push ,ball))
       (drop-balloon ,ball))))

(defun balloons ()
  (ps
    (defvar score 0)
    (defvar score-text)
    
    (defvar window-width (- (@  window inner-width) 15))
    (defvar window-height (- (@ window inner-height) 18))

    (defun random-range (min max)
      (+ min (chain -math (floor (* (chain -math (random)) (+ 1 (- max min)))))))

    (defun drop-balloon (the-balloon)
      (setf (@ the-balloon position x) (random-range 1 window-width))
      (setf (@ the-balloon position y) (random-range window-height 2000)))
    
    (defvar all-balloons (new (-array)))

    (defun click-handler (obj)
      (setf score (+ score 1))
      (update-score)
      (drop-balloon obj))
    
    (defun handle-collision (first second)
      (setf (chain first body velocity x) 5)
      (setf (chain second body velocity x) -5))

    (defun update-score ()
      (chain score-text (set-text score)))
    
    (defvar game (new (chain -phaser (-game window-width window-height
                                            (@ -phaser -c-a-n-v-a-s) "balloons" (create preload preload
                                                                                        create create
                                                                                        update update)))))

    (defun update ()
      
      (chain _ (map all-balloons (lambda (balloon)
                                   (chain _ (map all-balloons
                                                 (lambda (second-balloon)
                                                   (chain game physics arcade (collide balloon second-balloon handle-collision nil this)))))
                                   (when (> -100 (@ balloon position y))
                                     (drop-balloon balloon))
                                   (when (> -10 (@ balloon position x))
                                     (drop-balloon balloon)))))
      (chain game input on-down (add-once update-score this)))
    
    (defun preload ()
      (chain game load (image "balloon" "img/balloon_red.png"))
      (chain game load (image "background" "img/background.jpg")))

    
    (defun create ()
      (setf (chain game stage disable-visibility-change) t)
      (defvar background (chain game add (sprite 0 0 "background")))
      (setf score-text (chain game add (text 50
                                             25 "0" (create font "40px Arial"
                                                            fill "#ff0044"
                                                            stroke "#fff"
                                                            stroke-thickness 2
                                                            align "center"))))
      (chain score-text anchor (set-to 0.5 0.5))
      
      (dotimes (i 100)
        (add-balloon)))))
