(in-package #:balloons)

(defvar *listener* nil)


(defun reset-dispatch-table ()
  (setf *dispatch-table* (list #'dispatch-easy-handlers)))

(defun start-server ()
  (unless *listener*
    (reset-dispatch-table)
    (setf *listener* (make-instance 'hunchentoot:easy-acceptor
                                    :document-root (cl-ivy:resource-path ".")
                                    :port 8080))
    (hunchentoot:start *listener*)))

(defun stop-server ()
  (when *listener*
    (hunchentoot:stop *listener*)))


(defun write-javascript ()
  (with-open-file (outfile (format nil "~aballoons.js" (cl-ivy:resource-path "."))
                           :direction :output
                           :if-exists :supersede)
    (write-string (balloons) outfile)))



(defmacro+ps -> (&rest body)
  `(chain ,@body))

(defmacro+ps _ (function &rest body)
  `(-> _ (,function ,@body)))

(defmacro+ps add-entity ()
  (with-ps-gensyms (ball)
    `(progn
       (defvar ,ball (chain game add (sprite 0 0 "balloon")))

       (chain game physics (enable ,ball (chain -phaser -physics -a-r-c-a-d-e)))
       (setf (@ ,ball check-world-bounds) t)
       (chain ,ball body bounce (set 1))

       (setf (chain ,ball input-enabled) t)
       (chain ,ball events on-input-down (add click-handler ,ball))
       
       (setf (chain ,ball body velocity y) (random-range -20 -50))

       (chain all-balloons (push ,ball))
       (drop-balloon ,ball))))

(defmacro with-page (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (htm ,@body)))

(define-easy-handler (balloons-handler :uri "/balloons") ()
  (with-page
      (:html :lang "en"
        (:head
          (:script :src "js/phaser.js")
          (:script :src "js/underscore-min.js"))
        (:body :style "background-color:#eee;"
          (:script :src "/balloons.js")))))

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
      (setf (@ the-balloon position y) (random-range window-height 2000))
      (-> the-balloon scale (set 0.5)))
    
    (defvar all-balloons (new (-array)))

    (defun click-handler (obj)
      (setf score (+ score 1))
      (update-score)
      (drop-balloon obj)
      (-> popfx (play "pop")))
    
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

                            (when (-> balloon to_reset)
                              (if (< 5 (-> balloon to_reset))
                                  (drop-balloon balloon)
                                  (setf (-> balloon to_reset) (+ (-> balloon to_reset) 1))))
                            
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
      (-> game load (audio "popfx" "snd/pop.ogg")))
        
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
      (-> popfx (add-marker "pop" 0 100))
           
      (setf (-> popfx allow-multiple) t)
      
      (dotimes (i 99)
        (add-entity)))))

(define-easy-handler (serve-js :uri "/balloons.js") ()
  (setf (content-type*) "text/javascript")
  (balloons))
