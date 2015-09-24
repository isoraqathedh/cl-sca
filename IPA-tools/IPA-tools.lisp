(defpackage :info.isoraqathedh.IPA-tools
  (:use :cl :split-sequence :alexandria)
  (:nicknames :IPA-tools)
  (:export :get-phoneme))

(in-package :IPA-tools)

;;; Phoneme class and associated properties.

(defclass phoneme ()
  (properties))

(defgeneric phoneme-properties (phoneme)
  (:documentation "Retrieves *all* properties in a phoneme.")
  (:method ((phoneme phoneme))
    (hash-table-plist (slot-value phoneme 'properties))))

(defgeneric get-phoneme-property (property phoneme)
  (:documentation "Gets the property of a phoneme.")
  (:method (property (phoneme phoneme))
    (gethash property (slot-value phoneme 'properties))))

(defgeneric (setf get-phoneme-property) (value property phoneme)
  (:method (value property (phoneme phoneme))
    (setf (gethash property (slot-value phoneme 'properties)) value)))

(defmethod initialize-instance :after
    ((instance phoneme) &key character properties-plist &allow-other-keys)
  ;; The properties of the phoneme are defined as a plist
  (setf (slot-value instance 'properties)
        (plist-hash-table 
         (append (list :character character) properties-plist) :test 'equal)))

(defmethod print-object ((object phoneme) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "/~a/" (get-phoneme-property :character object))))

;;; Reading input data
#| Data about phonemes with IPA characters attached to them
   are extraordinarily hard to find.
   As such, yet another thing has to be read in manually.
   The data file here is adapted from Microsoft's website
   on the phonemmes.
   Unfortunately, the data has been at least somewhat corrupted,
   So whole columns are omitted, most notably the ASCIIipa alternatives. |#

(defparameter *expansion-data*
  (with-open-file (stream
                   (asdf:system-relative-pathname
                    'lang-make-and-shake "IPA-tools/expand" :type "lisp"))
    (let ((*read-eval* nil))
      (read stream)))
  "Data used in expansion for reading the data file.")

(defparameter *input-file*
  (asdf:system-relative-pathname 'lang-make-and-shake "IPA-tools/phonemes" :type "txt")
  "The list of files that together define the IPA's primary letters.")

(defun find-in-expansion-data (thing property-type)
  "Search utility function that finds the details in expansion data."
  (cdr (find thing (cdr (assoc property-type *expansion-data*))
             :key #'car :test #'string=)))

(defun parse-IPA-description (description-string)
  "Parses an IPA description provided by the data."
  (destructuring-bind (char key-properties &optional additional-properties)
      (split-sequence-if (lambda (a) (find a "	{} ")) description-string
                         :remove-empty-subseqs t)
    (make-instance
     'phoneme
     :character (character char)
     :properties-plist
     (append
      (loop for i in (split-sequence #\, key-properties
                                     :remove-empty-subseqs t)
            append (find-in-expansion-data i :key-properties))
      (loop for i in (split-sequence #\, additional-properties
                                     :remove-empty-subseqs t)
            append (find-in-expansion-data i :additional-properties))))))

(defparameter *IPA-phoneme-list*
  (with-open-file (data-file *input-file* :external-format :utf-8)
    (loop for i = (read-line data-file nil nil)
          while i
          collect (parse-IPA-description i)))
  "A list of characters defined by the IPA, expressed as phoneme objects.")

;;; External package access
#| After loading the data, 
   we now allow other packages to use the data freely. |#
