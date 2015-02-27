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

(defmacro+ps -> (&rest body)
  `(chain ,@body))

(defmacro+ps _ (function &rest body)
  `(-> _ (,function ,@body)))

(defmacro+ps add-entity (texture)
  (with-ps-gensyms (ball)
    `(progn
       (defvar ,ball (chain game add (sprite 0 0 ,texture)))
       
       (chain game physics (enable ,ball (chain -phaser -physics -a-r-c-a-d-e)))
       (setf (@ ,ball check-world-bounds) t)
       (chain ,ball body bounce (set 1))

       (setf (chain ,ball input-enabled) t)
       (chain ,ball events on-input-down (add click-handler ,ball))
       
       (setf (chain ,ball body velocity y) (random-range -20 -50))

       (setf (-> ,ball itexture) ,texture)
       
       (chain ,ball scale (set 0.5))
       (chain all-balloons (push ,ball))
       (drop-balloon ,ball))))

(defun balloons ()
  (ps
    (defvar score 0)
    (defvar score-text)
    (defvar fx)

    
    (defvar window-width (- (@  window inner-width) 15))
    (defvar window-height (- (@ window inner-height) 18))

    (defun random-range (min max)
      (+ min (-> -math (floor (* (-> -math (random)) (+ 1 (- max min)))))))

    (defun drop-balloon (the-balloon)
      (setf (@ the-balloon position x) (random-range 1 window-width))
      (setf (@ the-balloon position y) (random-range window-height 2000)))
    
    (defvar all-balloons (new (-array)))

    (defun click-handler (obj)
      (setf score (+ score 1))
      (update-score)
      (drop-balloon obj)

      (when (= (-> obj itexture) "roo")
        (-> boingfx (play "boing")))
      (when (= (-> obj itexture) "daisy")
        (-> boingfx (play "boing")))
      (when (= (-> obj itexture) "balloon")
        (-> popfx (play "pop"))))
    
    (defun handle-collision (first second)
      (setf (-> first body velocity x) 10)
      (setf (-> second body velocity x) -10))

    (defun update-score ()
      (-> score-text (set-text score)))

    (defun alter-speed (amount)
      (_ map all-balloons (lambda (balloon)
                            (setf (-> balloon body velocity y) amount))))
    
    (defvar game (new (-> -phaser (-game window-width window-height
                                         (@ -phaser -c-a-n-v-a-s) "balloons" (create preload preload
                                                                                     create create
                                                                                     update update)))))

    (defun update ()
      (_ map all-balloons (lambda (balloon)
                            (_ map all-balloons (lambda (second-balloon)
                                                  (-> game physics arcade (collide balloon second-balloon handle-collision nil this))))
                            (when (> -100 (@ balloon position y))
                              (drop-balloon balloon))
                            (when (> -50 (@ balloon position x))
                              (drop-balloon balloon))))
      (-> game input on-down (add-once update-score this)))
    
    (defun preload ()
      (-> game load (image "balloon" "img/balloon_red.png"))
      (-> game load (image "background" "img/background.jpg"))
      (-> game load (image "daisy" "img/daisy.png"))
      (-> game load (image "roo" "img/roo.png"))
      
      (-> game load (audio "popfx" "snd/pop.ogg"))
      (-> game load (audio "boingfx" "snd/boing.ogg"))
      (-> game load (audio "sheepfx" "snd/sheep.ogg")))
        
    (defun create ()
      (setf (-> game stage disable-visibility-change) t)
      
      (defvar background (-> game add (sprite 0 0 "background")))
      (-> background scale (set 1.5))
      
      (setf score-text (-> game add (text 50 25 "0" (create font "40px Arial"
                                                            fill "#ff0044"
                                                            stroke "#fff"
                                                            stroke-thickness 2
                                                            align "center"))))
      (-> score-text anchor (set-to 0.5 0.5))

      (setf popfx (-> game add (audio "popfx")))
      (setf sheepfx (-> game add (audio "sheepfx")))
      (setf boingfx (-> game add (audio "boingfx")))
      
      (-> popfx (add-marker "pop" 0 100))
      (-> sheepfx (add-marker "sheep" 0 100))
      (-> boingfx (add-marker "boing" 0 100))
      
      (setf (-> popfx allow-multiple) t)
      (setf (-> sheepfx allow-multiple) nil)
      (setf (-> boingfx allow-multiple) nil)
      
      (dotimes (i 50)
        (add-entity "balloon"))
      (dotimes (i 25)
        (add-entity "daisy"))
      (dotimes (i 25)
        (add-entity "roo")))))
