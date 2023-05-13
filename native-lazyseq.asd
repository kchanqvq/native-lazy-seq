(defsystem native-lazyseq
  :description ""
  :author "Qiantan Hong <qthong@stanford.edu>"
  :license "GPLv3.0+"
  :depends-on (:trivial-package-local-nicknames
               :trivial-extensible-sequences
               :iterate :serapeum)
  :components ((:file "native-lazyseq")))
