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

(defun bubbles ()
  (ps
    (defvar balloons (new (-array)))
    
    (defun animate ()
      (chain renderer (render stage)))
    (defvar w 640)
    (defvar h 480)

    (defvar stage (new (chain -p-i-x-i (-stage 0xffffff t))))
    (setf (@ stage interactive) true)
    (defvar renderer (chain -p-i-x-i (auto-detect-renderer w h)))
    (chain document body (append-child (chain renderer view)))
    (request-anim-frame animate)
    
    (defvar background (chain -p-i-x-i -sprite (from-image "img/background.jpg")))
    (chain stage (add-child background))
    (defvar balloon (chain -p-i-x-i -texture (from-image "img/balloon_red.png")))
    (defvar clickified (chain -p-i-x-i -texture (from-image "img/balloon_red_clicked.png")))
    (defvar bbclicked (new (chain -p-i-x-i (-sprite clickified))))

    (defvar bb1 (new (chain -p-i-x-i (-sprite balloon))))
    (defvar bb2 (new (chain -p-i-x-i (-sprite balloon))))
    (defvar bb3 (new (chain -p-i-x-i (-sprite balloon))))
    
    (defun animate ()
      (when (and bb1.is_swapped
                 bb2.is_swapped
                 bb3.is_swapped)
        ; To the moon, Alice.
        (setf bb1.position.y (- bb1.position.y 1))
        (setf bb2.position.y (- bb2.position.y .7))
        (setf bb3.position.y (- bb3.position.y 1.1)))
      
      (chain renderer (render stage))
      (request-anim-frame animate))
    
    (defun interactive_setup (thing)
                                        ;(chain thing (set-interactive true))
      (setf (chain thing mouseover) (lambda (data)
                                      (chain console (log "mouseover"))
                                      (unless (chain this is_swapped)
                                        (chain this scale (set 1.1 1.1)))))
      (setf (chain thing mouseout) (lambda (data)
                                     (when (not (chain this is_swapped))
                                       (chain this scale (set 1 1)))))
      
      (setf (chain thing click) (lambda (data)
                                  (if (chain this is_swapped)
                                      (progn
                                        (setf (chain this is_swapped) nil)
                                        (chain this scale (set 1 1))
                                        (chain this (set-texture balloon)))
                                      (progn
                                        (setf (chain this is_swapped) 1)
                                        (chain this scale (set 1.4 1.4))
                                        (chain this (set-texture clickified)))))))
    
    (setf bb1.anchor.x 0.5)
    (setf bb1.anchor.y 0.5)
    (setf bb1.position.x 115)
    (setf bb1.position.y 190)
    (setf bb1.interactive t)
    (chain stage (add-child bb1))
    (interactive_setup bb1)

    (setf bb2.anchor.x 0.5)
    (setf bb2.anchor.y 0.5)
    (setf bb2.position.x 280)
    (setf bb2.position.y 190)
    (setf bb2.interactive t)
    (chain stage (add-child bb2))       
    (interactive_setup bb2)    

    (setf bb3.anchor.x 0.5)        
    (setf bb3.anchor.y 0.5)        
    (setf bb3.position.x 450)      
    (setf bb3.position.y 190)      
    (setf bb3.interactive t)
    (chain stage (add-child bb3))       
    (interactive_setup bb3)))


(defun old-bubbles ()
  (with-open-file (outfile (format nil "~abubbles.js" (cl-ivy:resource-path "."))
                           :direction :output
                           :if-exists :supersede)
    (write-string 
     (ps
       (progn
         (defvar *circles* (new (-array)))
         (defvar *stage* nil)
         (defvar bubbles
           (create make-rrandom (lambda (min max)
                                  
                                  (chain -math (floor (* (chain -math (random)) (+ min (+ 1 (- max min)))))))
                   make-random (lambda (x y)
                                 (+ 1 (chain -math (floor (* y (chain -math (random)))))))
                   make-bubble (lambda ()
                                 (setf circle (new (chain createjs (-shape))))
                                 (chain circle graphics (begin-fill "DeepSkyBlue")
                                        (draw-circle 0 0 50))
                                 (setf (@ circle x) (chain bubbles (make-rrandom 700 800)))
                                 (setf (@ circle y) 800)
                                 (chain stage (add-child circle))
                                 (chain stage (update))
                                 (chain *circles* (push circle)))
                   init (lambda ()
                          (setf stage (new (chain createjs  (-stage "canvas"))))
                          (dotimes (x 10)
                            (chain bubbles (make-bubble)))
                          )))))
     outfile))
  )
