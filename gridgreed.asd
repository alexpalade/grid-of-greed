(asdf:defsystem :gridgreed
  :description "Autumn 2019 Lisp Game Jam entry"
  :license "GPLv3"
  :version "0.9.0"
  :author "Alexandru Palade <alexpalade@gmail.com>"
  :depends-on (trivial-gamekit)
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "player")
               (:file "grid")
               (:file "score-bar")
               (:file "main")))
