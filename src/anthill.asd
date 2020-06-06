(asdf:defsystem anthill
  :description "Parallel stuff"
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "kernel" :depends-on ("package"))
               (:file "cognate" :depends-on ("kernel"))
               (:file "cognate/pmap" :depends-on ("cognate"))))
