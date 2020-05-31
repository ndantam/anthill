(asdf:defsystem anthill
  :description "Parallel stuff"
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "kernel" :depends-on ("package"))))
