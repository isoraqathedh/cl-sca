(defpackage :info.isoraqathedh.lang-make-and-shake.asdf
  (:use :cl :asdf))
(in-package :info.isoraqathedh.lang-make-and-shake.asdf)

(defsystem lang-make-and-shake
  :name "Language Make and Shake"
  :serial t
  :components ((:module "IPA-tools"
                :components ((:file "IPA-tools"))))
  :depends-on (:alexandria :split-sequence))
