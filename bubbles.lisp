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
    (defvar game (new (chain -phaser (-game 800 600 (@ -phaser -c-a-n-v-a-s) "balloons" (create preload preload
                                                                                                create create)))))

    (defun preload ()
      (chain game load (image "balloon" "img/balloon_red.png")))

    
    (defun create ()
    
      (defvar balloon (chain game add (sprite 0 0 "balloon")))
      (chain game physics (enable balloon (chain -phaser -physics -a-r-c-a-d-e)))
      (setf (chain balloon body velocity y) 20))))
