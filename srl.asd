(asdf:defsystem "srl"
  :description "Symbolic Reinforcement Learning"
  :version "0.1"
  :author "Bryce MacInnis"
  :depends-on ("py4cl")
  :components ((:file "package")
               (:file "main"))
