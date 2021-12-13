;;;; aoc2021.asd

(asdf:defsystem #:aoc2021
  :description "Describe aoc2021 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (cl-ppcre)
  :components ((:file "package")
               (:file "aoc2021")
               (:file "day1")
               (:file "day2")
               ))
