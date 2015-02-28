;;;; bubbles.asd

(asdf:defsystem #:balloons
  :description "Describe balloons here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:parenscript
               #:hunchentoot
               #:cl-who
               #:cl-ivy)
  :components ((:file "package")
               (:file "balloons")))

