(in-package #:aoc2021)

(defpackage #:aoc2021/day15
  (:use #:cl #:aoc2021))

(in-package #:aoc2021/day15)

(defparameter *filename-15-0* (merge-pathnames "input-15-0.txt" (asdf:system-source-directory :aoc2021)))
(defparameter *filename-15-1* (merge-pathnames "input-15-1.txt" (asdf:system-source-directory :aoc2021)))

(defun read-input (filename)
  (with-open-file (f filename)
    (let ((data 
           (loop for line = (read-line f nil)
                 while line
                 collect (map 'list 'digit-char-p line))))
      (make-array (list (length data) (length (first data)))
                  :element-type 'integer :initial-contents data))))

(defun print-data (data &optional (stream *standard-output*))
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (terpri stream)
    (loop for y below ny
          do (progn
               (loop for x below nx
                     do (write-char (code-char (+ (aref data y x) (char-code #\0))) stream))
               (terpri stream)))))


#||
(print-data (read-input *filename-15-0*))
||#

(defun dijkstra (data)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1))
        (distances (make-array (array-dimensions data) :element-type 'integer :initial-element most-positive-fixnum))
        (previous (make-array (array-dimensions data) :initial-element nil))
        (remaining (make-instance 'cl-heap:priority-queue)))
    (setf (aref data 0 0) 0)
    #+nil
    (dotimes (y ny)
      (dotimes (x nx)
        (cl-heap:enqueue remaining (cons y x) (aref data y x))))
    (cl-heap:enqueue remaining (cons 0 0) 0)
    (setf (aref distances 0 0) 0)
    
    (flet ((neighbours (y x)
             (let ((res nil))
               (when (plusp y)
                 (push (cons (1- y) x) res))
               (when (< y (1- nx))
                 (push (cons (1+ y) x) res))
               (when (plusp x)
                 (push (cons y (1- x)) res))
               (when (< x (1- nx))
                 (push (cons y (1+ x)) res))
               res)))
      (loop while (plusp (cl-heap:queue-size remaining))
            for (y . x) = (cl-heap:dequeue remaining)
            until (and (= y (1- ny)) (= x (1- nx)))
            for distance = (aref distances y x)
            for neighbours = (neighbours y x)
            do (loop for (yy . xx) in neighbours
                     for new-distance = (+ distance (aref data yy xx))
                     when (< new-distance (aref distances yy xx))
                     do (progn
                          (setf (aref distances yy xx) new-distance
                                (aref previous yy xx) (cons y x))
                          (cl-heap:enqueue remaining (cons yy xx) new-distance)))
            finally (return (aref distances y x))))))

(defun day-15-1 (filename)
  (let ((data (read-input filename)))
    (dijkstra data)))

#||
(assert (= (day-15-1 *filename-15-0*) 40))
(day-15-1 *filename-15-1*)
||#

(defun tile-map (data)
  (let ((ny (array-dimension data 0))
        (nx (array-dimension data 1)))
    (let ((res (make-array (list (* ny 5)
                                 (* ny 5))
                           :element-type (array-element-type data))))
      (flet ((paste-array (y x)
               (let ((start-y (* y ny))
                     (start-x (* x nx))
                     (inc (mod (+ y x) 9)))
                 (loop for y below ny
                       do (loop for x below nx
                                for new-val = (+ (aref data y x) inc)
                                do (setf (aref res (+ start-y y) (+ start-x x))
                                         (if (> new-val 9)
                                           (- new-val 9)
                                           new-val)))))))
        (loop for y below 5
              do (loop for x below 5
                       do (paste-array y x))))
      res)))

(defun day-15-2 (filename)
  (let ((data (tile-map (read-input filename))))
    (dijkstra data)))


       

#||
(print-data (tile-map (read-input *filename-15-0*)))

(assert (= (day-15-2 *filename-15-0*) 315))
(time (day-15-2 *filename-15-1*))
||#