;;;; bubbles.asd

(asdf:defsystem #:bubbles
  :description "Describe bubbles here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:parenscript
               #:hunchentoot
               #:cl-ivy)
  :components ((:file "package")
               (:file "bubbles")))

