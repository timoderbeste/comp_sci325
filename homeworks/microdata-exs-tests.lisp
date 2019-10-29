(defpackage microdata-tests
  (:use :common-lisp :lisp-unit :microdata)
  )

(in-package :microdata-tests)

;;; Some simple tests for microdata reader exercises
;;;  http://www.cs.northwestern.edu/academics/courses/325/exercises/semweb-exs.php

;;; Update history:
;;;
;;; 12-13-2018 added test to HYPHENATE [CKR]
;;; 09-02-2016 fixed MD-EQUAL to delay retrieving ORG.SCHEMA package [CKR]
;;; 12-01-2015 fixed ASSERT-EQUALITY calls [CKR]
;;; 11-16-2012 fixed MD-EQUAL to use STRING= [CKR]
;;; 11-28-2011 add 3 word examples to CAMELIZE and HYPHENATE [CKR]

;;; CAMELIZE

(define-test camelize
  (assert-equal "job" (camelize "job"))
  (assert-equal "Job" (camelize "job" t))
  (assert-equal "jobPosting" (camelize "job-posting"))
  (assert-equal "BookFormatType" (camelize "book-format-type" t))
  (assert-equal "JobPosting" (camelize "job-posting" t))
)

;;; HYPHENATE

(define-test hyphenate
  (assert-equal "JOB" (hyphenate "job"))
  (assert-equal "JOB" (hyphenate "Job"))
  (assert-equal "JOB" (hyphenate "Job" :upper))
  (assert-equal "job" (hyphenate "Job" :lower))
  (assert-equal "JOB-POSTING" (hyphenate "jobPosting"))
  (assert-equal "JOB-POSTING" (hyphenate "JobPosting"))
  (assert-equal "BOOK-FORMAT-TYPE" (hyphenate "BookFormatType"))
  (assert-equal "URL" (hyphenate "URL"))
  (assert-equal "GET-ID" (hyphenate "getID"))
  (assert-equal "AN-A" (hyphenate "anA"))
  )

;;; READ-MICRODATA

;;; This is particularly tricky to test because we need to
;;; make sure symbols are being added to the org.schema
;;; package without incidentally adding those symbols
;;; in the test code, or assuming org.schema
;;; exists before reading any input.

;;; So we define MD-EQUAL that compares two lists, comparing
;;; symbols by name only, then checking the second
;;; list to verify that any non-NIL non-keyword
;;; symbols are external symbols in org.schema

(defun md-equal (x y)
  (let ((package (find-package :org.schema)))
    (labels ((compare (x y)
               (cond ((consp x)
                      (and (consp y) (every #'compare x y)))
                     ((or (null x) (not (symbolp x)) (keywordp x))
                      (equal x y))
                     ((symbolp y) 
                      (and (string= (symbol-name x) (symbol-name y))
                           (external-symbol-p y package)))
                     (t nil))))
      (compare x y))))

(defun external-symbol-p (sym pkg)
  (or (and (eql (symbol-package sym) pkg)
           (eql (nth-value 1 (find-symbol (symbol-name sym) pkg))
                :external))
      (fail "~S not external symbol in ~A" 
            sym (package-name pkg))))

  
(define-test read-microdata
  (let ((*md-pkg* (find-package "org.schema")))
    (assert-equality md-equal '() (read-microdata  "<div>something</div>"))
    (assert-equality md-equal '((thing))
                     (read-microdata "<div itemscope>something</div>"))
    (assert-equality md-equal '((job-posting))
      (read-microdata 
      "<div itemscope itemtype=\"http://schema.org/JobPosting\">something</div>"))

    ;; https://schema.org/docs/gs.html#microdata_itemprop
    (assert-equality md-equal '((movie (:name "Avatar") (:director "James Cameron") (:genre "Science fiction") (:trailer "Trailer")))
      (read-microdata
      "<div itemscope itemtype ='http://schema.org/Movie'>
  <h1 itemprop='name'>Avatar</h1>
  <span>Director: <span itemprop='director'>James Cameron</span> (born August 16, 1954)</span>
  <span itemprop='genre'>Science fiction</span>
  <a href='../movies/avatar-theatrical-trailer.html' itemprop='trailer'>Trailer</a>
</div>"))

    ;; https://schema.org/docs/gs.html#microdata_embedded
    (assert-equality md-equal '((movie (:name "Avatar")
                                       (:director (person (:name "James Cameron") (:birth-date "August 16, 1954")))
                                       (:genre "Science fiction") 
                                       (:trailer "Trailer")))
      (read-microdata
      "<div itemscope itemtype ='http://schema.org/Movie'>
  <h1 itemprop='name'>Avatar</h1>
  <div itemprop='director' itemscope itemtype='http://schema.org/Person'>
  Director: <span itemprop='name'>James Cameron</span> (born <span itemprop='birthDate'>August 16, 1954</span>)
  </div>
  <span itemprop='genre'>Science fiction</span>
  <a href='../movies/avatar-theatrical-trailer.html' itemprop='trailer'>Trailer</a>
</div>"))
    ))