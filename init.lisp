
(ql:quickload 'bubbles)
(ql:quickload 'swank)
(swank:create-server :port 4005 :dont-close t)
(in-package :bubbles)
(start-server)
