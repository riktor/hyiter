(import (hycl.core (*)))
(require
  (hycl.core (*))
  (hy.contrib.loop (loop)))


(defun matchp (clause ptn)
  (if (not (= (length clause) (length ptn)))
      False
      (let ((flag True))
        (for (tp (zip clause ptn))
          (when (and (not (= (get tp 1) '_)) (not (= (get tp 0) (get tp 1)))) 
            (setf flag False)
            (break)))
        flag)))

(defun flatten-destruc (ls)
  (let ((acc ()))
    (for (el ls)
      (if (consp (car el))
          (.extend acc (flatten-destruc el))
          (.append acc el)))
    acc))

(defun parse-clause (clause parsed ret-sym)
  (cond/cl
    ;; initially clause
    ((= (car clause) 'initially)
      (setf (get parsed :initially) (cdr clause)))

    ;; finally clause
    ((= (car clause) 'finally)
      (setf (get parsed :finally) (cdr clause)))
    
    ;; with clause
    ((matchp clause '(with _ = _))
      (if (not (consp (get clause 1)))
          (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
          (setf (get parsed :with)
                (append (nreverse (flatten-destruc (destruc (get clause 1) (get clause 3) 0))) 
                        (get parsed :with)))))
    
    ;; for = clause
    ((matchp clause '(for _ = _))
      (if (not (consp (get clause 1)))
          (progn
            (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
            (push `(~(get clause 1) ~(get clause 3)) (get parsed :for)))
          (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) (get clause 3) 0)))))
            (setf (get parsed :with)
                  (append destr (get parsed :with)))
            (setf (get parsed :for)
                  (append destr (get parsed :for))))))
    
    ((matchp clause '(for _ = _ then _))
      (let ((__cur__  (gensym)))
        (if (not (consp (get clause 1)))
            (progn
              (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
              (push `(~(get clause 1) ~(get clause 5)) (get parsed :for)))
            (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) __cur__ 0)))))
              (push `(~__cur__ (first ~(get clause 3)) ) (get parsed :with))
              (setf (get parsed :with)
                    (append destr (get parsed :with)))
              (push `(~__cur__ (first ~(get clause 5)) ) (get parsed :with))
              (setf (get parsed :for)
                    (append destr (get parsed :for)))))))

    ;; for in clause
    ((matchp clause '(for _ in _))
      (let ((__it__  (gensym "it"))
             (__cur__ (gensym "cur")))
        (push `(~__it__ ((get __builtins__ 'iter) ~(get clause 3))) (get parsed :with))
        (push `(~__cur__ (next ~__it__) ) (get parsed :with))
        (if (not (consp (get clause 1)))
            (progn                 
              (push `(~(get clause 1) ~__cur__) (get parsed :with))
              (push `(~__cur__ (next ~__it__)) (get parsed :for))
              (push `(~(get clause 1) ~__cur__) (get parsed :for)))
            (let ((destr (nreverse (flatten-destruc (destruc (get clause 1) __cur__ 0)))))
              (setf (get parsed :with)
                    (append destr (get parsed :with)))
              (push `(~__cur__ (next ~__it__)) (get parsed :for))
              (setf (get parsed :for)
                    (append destr (get parsed :for)))))))

    ;; for from clause
    ((matchp clause '(for _ from _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for)))
    ((matchp clause '(for _ from _ below _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for))
      (push `(not (< ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ upto _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) 1)) (get parsed :for))
      (push `(not (<= ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ below _ by _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) ~(get clause 7))) (get parsed :for))
      (push `(not (< ~(get clause 1) ~(get clause 5))) (get parsed :break)))
    ((matchp clause '(for _ from _ upto _ by _))
      (push `(~(get clause 1) ~(get clause 3)) (get parsed :with))
      (push `(~(get clause 1) (+ ~(get clause 1) ~(get clause 7))) (get parsed :for))
      (push `(not (<= ~(get clause 1) ~(get clause 5))) (get parsed :break)))

    ((matchp clause '(repeat _))
      (let ((__counter__ (gensym "counter")))
        (push `(~__counter__ 0) (get parsed :with))
        (push `(~__counter__ (+ ~__counter__ 1)) (get parsed :for))
        (push `(not (< ~__counter__ ~(get clause 1))) (get parsed :break))))

    ((matchp clause '(while _))
      (push `(not ~(get clause 1)) (get parsed :break)))

    ((matchp clause '(until _))
      (push (get clause 1) (get parsed :break)))
    
    (True
      (push clause (get parsed :body)))))


(defun parse-clauses (clauses ret-sym)
  (let ((parsed-clauses {:with nil
                         :initially nil
                         :for nil
                         :body nil
                         :finally nil
                         :break nil
                         }))
    (for (el clauses)
      (parse-clause el parsed-clauses ret-sym))
    (nreverse (get parsed-clauses :for))
    (nreverse (get parsed-clauses :with))
    (nreverse (get parsed-clauses :break))
    (nreverse (get parsed-clauses :body))
    parsed-clauses))

(defun replace-collect (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter))
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'collect))
                    (setf (get ls i) `(.append ~(cond/cl
                                                  ((matchp el '(collect _)) ret-sym)
                                                  ((matchp el '(collect _ into _))
                                                    (.extend acc [(get el 3)])
                                                    (get el 3)))
                                               ~(get el 1))
                          (get flag 0) True)
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-append (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'append))
                    (setf (get ls i) `(.extend ~(cond/cl
                                                  ((matchp el '(append _)) ret-sym)
                                                  ((matchp el '(append _ into _))
                                                    (.extend acc [(get el 3)])
                                                    (get el 3)))
                                               ~(get el 1))
                          (get flag 0) True)
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-maximize (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'maximize))
                    (let ((sym (cond/cl
                                 ((matchp el '(maximize _)) ret-sym)
                                 ((matchp el '(maximize _ into _))
                                   (.extend acc [(get el 3)])
                                   (get el 3)))))
                      (setf (get flag 0) True)
                      (setf (get ls i) `(setv ~sym (max ~sym ~(get el 1)))))
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-minimize (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'minimize))
                    (let ((sym (cond/cl
                                 ((matchp el '(minimize _)) ret-sym)
                                 ((matchp el '(minimize _ into _))
                                   (.extend acc [(get el 3)])
                                   (get el 3)))))
                      (setf (get flag 0) True)
                      (setf (get ls i) `(setv ~sym (min ~sym ~(get el 1)))))
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-sum (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'sum))
                    (let ((sym (cond/cl
                                 ((matchp el '(sum _)) ret-sym)
                                 ((matchp el '(sum _ into _))
                                   (.extend acc [(get el 3)])
                                   (get el 3)))))
                      (setf (get flag 0) True)
                      (setf (get ls i) `(setv ~sym (+ ~sym ~(get el 1)))))
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-count (flag ret-sym body)
  (let ((acc ()))
    (loop ((ls body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression) (= (car el) 'count))
                    (let ((sym (cond/cl
                                 ((matchp el '(count _)) ret-sym)
                                 ((matchp el '(count _ into _))
                                   (.extend acc [(get el 3)])
                                   (get el 3)))))
                      (setf (get flag 0) True)
                      (setf (get ls i) `(when ~(get el 1)
                                          (setv ~sym (+ ~sym 1)))))
                    (if (consp el)
                        (recur el acc))))))
    (cons body acc)))

(defun replace-return (body)
  (let ((acc ()))
    (loop ((ls  body)
            (acc acc))
          (if (typep ls HyCons)
              nil
              (for (i (range (length ls)))
                (setf el (get ls i))
                (when (and (consp el) (= (car el) 'iter)) 
                  (continue))
                (if (and (typep el HyExpression)
                         (or (= (car el) 'return) (= (car el) 'return-from)))
                    (cond/cl
                      ((matchp el '(return _))
                        (setf (get ls i) `(raise (hyiter.core.Return ~(get el 1)))))
                      ((matchp el '(return-from _ _))
                        (setf (get ls i) `(raise (hyiter.core.TaggedReturn ~(get el 1) ~(get el 2))))))                                        
                    (if (consp el)
                        (recur el acc)))))
          (cons body acc))
    body))


(defclass Return (Exception)
  (defn __init__ (self val)
    (setf (. self val) val)))

(defclass TaggedReturn (Exception)
  (defn __init__ (self  tag val)
    (setf (. self tag) tag)
    (setf (. self val) val)))

(defmacro return-iter (val)
  `(raise (Return ~val)))

(defmacro return-from (tag val)
  `(raise (TaggedReturn ~tag ~val)))

(defmacro/g! iter (&rest clauses)  
  (let ((g!parsed (parse-clauses clauses g!ret))
         (body-and-accs nil)
         (res nil)
         (flag [False])
         (init-var nil))
    (for (el [replace-collect replace-append
              replace-minimize replace-sum replace-maximize 
              replace-count])
      (setf res (el flag g!ret (get g!parsed :body)))
      (when (car flag)
        (setf body-and-accs res)           
        (cond/cl
          ((in el [replace-collect replace-append]) (setf init-var []))             
          ((= el replace-minimize) (setf init-var 10000000000000))
          ((= el replace-maximize) (setf init-var -10000000000000))
          ((in el [replace-sum replace-count]) (setf init-var 0)))
        (break)))
    `(try
       (do
         (setv ~g!tag ~(if (keyword? (car clauses))
                           (car clauses)
                           None))
         (setv ~g!ret ~init-var)
         ~@(replace-return (get g!parsed :initially)) 
         (try
           (do
             (setv ~@(flatten-1 (get g!parsed :with)))
             (setv ~@(flatten-1 (mapcar (lambda (x) `(~x [])) (cdr body-and-accs))))
             (while True
               ~@(replace-return (get g!parsed :body)) 
               (setv ~@(flatten-1 (get g!parsed :for)))
               (when (or ~@(get g!parsed :break))
                 (break))))
           (except (e StopIteration)
             None))
         ~@(replace-return (get g!parsed :finally)) 
         ~g!ret)
       (except (r hyiter.core.Return)
         (. r val))
       (except (tr hyiter.core.TaggedReturn)
         (if (= (. tr tag) ~g!tag)
             (. tr val)
             (raise tr))))))

