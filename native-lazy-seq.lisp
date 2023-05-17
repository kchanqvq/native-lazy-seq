(uiop:define-package :native-lazy-seq
    (:use #:cl)
  (:import-from #:iter #:iter #:while #:sum)
  (:export #:lazy-seq #:generator #:with-iterators))

(in-package :native-lazy-seq)
(serapeum:eval-always
  (trivial-package-local-nicknames:add-package-local-nickname
   "PORTSEQ" "ORG.SHIRAKUMO.TRIVIAL-EXTENSIBLE-SEQUENCES"))

(defclass lazy-seq (portseq:sequence standard-object)
  ((contents :accessor contents :initform (serapeum:vect))
   (generator :accessor generator :initarg :generator))
  (:documentation "Lazy sequence class.
Accept a single initialization argument :GENERATOR, which must be a
function.

The function specifies how to lazily generate elements in the
sequence.  When it is called with no argument, it must return two
values ELEMENT, PRESENT-P.  If there are element to be generated,
ELEMENT should be the next ELEMENT and PRESENT-P should be
non-nil. Otherwise, PRESENT-P should be nil."))

(defmacro lazy-seq (() &body body)
  "Construct a lazy sequence.

BODY is evaluated each time we want to generate a new element in the
lazy sequence.  It should result in two values ELEMENT, PRESENT-P.  If
there are element to be generated, ELEMENT should be the next ELEMENT
and PRESENT-P should be non-nil. Otherwise, PRESENT-P should be nil."
  `(make-instance 'lazy-seq :generator (lambda () ,@body)))

(defun generate-element (s)
  "Try to generate one more element in lazy sequence S.
Return true if such a element is successfully generated."
  (multiple-value-bind (element present-p) (funcall (generator s))
    (when element
      (vector-push-extend element (contents s)))
    present-p))

(defmethod portseq:elt ((s lazy-seq) i)
  (iter
    (while (<= (length (contents s)) i))
    (unless (generate-element s)
      (error "Index ~A too large for ~S." i s)))
  (elt (contents s) i))

(defmethod portseq:length ((s lazy-seq))
  (iter
    (while (generate-element s)))
  (length (contents s)))

(defmethod portseq:make-simple-sequence-iterator
    ((s lazy-seq) &key from-end (start 0) end)
  ;; TODO: support the full protocol
  ;; 1. fall back to eager eval for from-end
  ;; 2. implement limit
  (assert (not from-end))
  (assert (not end))
  (values start end nil))

(defmethod portseq:iterator-endp ((s lazy-seq) i limit from-end)
  (and (>= i (length (contents s)))
       (not (generate-element s))))

(defun call-with-iterators (seq thunk)
  (portseq:with-sequence-iterator
      (iterator limit from-end step endp element)
      (seq)
    (funcall thunk
             (lambda ()
               (funcall element seq iterator))
             (lambda ()
               (setq iterator (funcall step seq iterator from-end))
               nil)
             (lambda ()
               (funcall endp seq iterator limit from-end)))))

(defmacro with-iterators ((element next endp) seq &body body)
  `(call-with-iterators
    ,seq
    (lambda (,element ,next ,endp)
      (declare (type function ,element ,next ,endp))
      ,@body)))

(defmethod portseq:make-sequence-like
    ((s lazy-seq) length &key initial-element initial-contents)
  (if initial-contents
      (progn
        (assert (null initial-element))
        (= length (length initial-contents))
        (with-iterators (element next endp) initial-contents
          (lazy-seq ()
            (if (funcall endp)
                (values nil nil)
                (values (prog1 (funcall element) (funcall next)) t)))))
      (let ((i 0))
        (lazy-seq ()
          (incf i)
          (when (<= i length)
            (values initial-element t))))))

(defmethod portseq:subseq ((s lazy-seq) start &optional end)
  (let ((i start))
    (lazy-seq ()
      (when (if end (< i end)
                (not (portseq:iterator-endp s i nil nil)))
        (let ((element (elt s i)))
          (incf i)
          (values element t))))))

(defmethod portseq:remove-if
    (predicate (s lazy-seq) &key from-end (start 0) end count (key #'identity))
  ;; :from-end Not implemented.
  ;; TODO: fall back to eager eval to implement this.
  (assert (not (and from-end count)))
  (let ((i start)
        (c 0))
    (lazy-seq ()
      (loop
        (if (if end (< i end)
                (not (portseq:iterator-endp s i nil nil)))
            (let ((element (elt s i)))
              (incf i)
              (if (and (not (and count (>= c count)))
                       (funcall predicate (funcall key element)))
                  (incf c)
                  (return (values element t))))
            (return))))))

(defmethod portseq:remove-if-not
    (predicate (s lazy-seq) &key from-end (start 0) end count (key #'identity))
  (portseq:remove-if (lambda (e) (not (funcall predicate e))) s
                     :from-end from-end :start start :end end :count count :key key))
