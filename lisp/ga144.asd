(defpackage :ga144-system
  (:use :cl :asdf))
(in-package :ga144-system)


(defsystem ga144
  :description "GreenArrays GA144 tools"
  :version "0.0.1"
  :author "TQ Hirsch <thequux@thequux.com>"
  :license "MIT"
  :depends-on (:cl-parsec :rutils)
  :components ((:file "packages")
	       (:file "opcodes" :depends-on ("packages"))
	       (:file "simulator" :depends-on ("packages" "opcodes"))))

