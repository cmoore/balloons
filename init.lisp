
(ql:quickload 'balloons)
(ql:quickload 'swank)
(swank:create-server :port 4005 :dont-close t)
(in-package :balloons)
(start-server)
