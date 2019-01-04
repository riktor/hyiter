(import
  [hycl.core [*]])

(require
  [hycl.core [*]]
  [hy.contrib.loop [loop]])


(defun matchp (clause ptn)
  (if (not (= (length clause) (length ptn)))
      False
      (let ((flag True))
        (for [tp (zip clause ptn)]
          (when (and (not (= (get tp 1) '_)) (not (= (get tp 0) (get tp 1)))) 
            (setf flag False)
            (break)))
        flag)))

(defun flatten-destruc (ls)
  (let ((acc ()))
    (for [el ls]
      (if (consp (car el))
          (.extend acc (flatten-destruc el))
          (.append acc el)))
    acc))

(defun parse-clause (clause parsed ret-sym)   
  (cond/cl
    ((keyword? clause)
     (setf (get parsed :loop-tag) clause))
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
        (push `(~__it__ (iter ~(get clause 3))) (get parsed :with))
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
    
    ((matchp clause '(drop _))
      (let ((__counter__ (gensym "counter")))
        (push `(~__counter__ 0) (get parsed :with))
        (push `(~__counter__ (+ ~__counter__ 1)) (get parsed :for))
        (push `(< ~__counter__ ~(get clause 1)) (get parsed :drop))))
    ((matchp clause '(dropwhile _))
      (push (get clause 1) (get parsed :drop)))

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
                         :drop nil
                         :loop-tag nil}))
    (for [el clauses]
      (parse-clause el parsed-clauses ret-sym))
    (nreverse (get parsed-clauses :for))
    (nreverse (get parsed-clauses :with))
    (nreverse (get parsed-clauses :break))
    (nreverse (get parsed-clauses :body))
    parsed-clauses))

(defmacro! nreplace-clauses (tree condition replace)
  `(let ((!acc! ())
          (~g!orig-tree ~tree)
          (~g!replaced [False]))
     (loop     
       ((~g!tree ~g!orig-tree))       
       (progn
         (for [~g!ind (range (len ~g!tree))]         
           (setf !el! (get ~g!tree ~g!ind))         
           (when ~condition           
             (setf (get ~g!tree ~g!ind) ~replace
                   (get ~g!replaced 0) True))
           (when (and (consp !el!) (not (= (get !el! 0) 'itr)))
             (recur !el!)))))
     [~g!orig-tree !acc! (get ~g!replaced 0)]))

(defun replace-collect (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'collect))
                    `(~(cond/cl
                         ((matchp !el! '(collect _)) update-fn-sym)
                         ((matchp !el! '(collect _ into _))
                           (let ((sym (gensym 'update)))
                             (.append !acc! [(get !el! 3) sym])
                             sym)))
                      ~(get !el! 1))))

(defun replace-append (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'append))
                    `(~(cond/cl
                         ((matchp !el! '(append _)) update-fn-sym)
                         ((matchp !el! '(append _ into _))
                           (let ((sym (gensym 'update)))
                             (.append !acc! [(get !el! 3) sym])
                             sym)))
                      ~(get !el! 1))))

(defun replace-maximize (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'maximize))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(maximize _)) ret-sym)
                                 ((matchp !el! '(maximize _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (max ~sym ~(get !el! 1))))))

(defun replace-minimize (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'minimize))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(minimize _)) ret-sym)
                                 ((matchp !el! '(minimize _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (min ~sym ~(get !el! 1))))))

(defun replace-sum (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'sum))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(sum _)) ret-sym)
                                 ((matchp !el! '(sum _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(setv ~sym (+ ~sym ~(get !el! 1))))))

(defun replace-count (ret-sym update-fn-sym body)
  (nreplace-clauses body
                    (and (typep !el! HyExpression) (= (car !el!) 'count))
                    (let ((sym (cond/cl
                                 ((matchp !el! '(count _)) ret-sym)
                                 ((matchp !el! '(count _ into _))
                                   (.append !acc! [(get !el! 3) nil])
                                   (get !el! 3)))))
                      `(when ~(get !el! 1) 
                         (setv ~sym (+ ~sym 1))))))

(defun replace-return (body)
  (first (nreplace-clauses body
                           (and (typep !el! HyExpression)
                                (or (= (car !el!) 'return) (= (car !el!) 'return-from)))          
                           (cond/cl
                             ((matchp !el! '(return _))
                               `(raise (hyiter.core.Return ~(get !el! 1))))
                             ((matchp !el! '(return-from _ _))
                               `(raise (hyiter.core.TaggedReturn ~(get !el! 1) ~(get !el! 2))))))) )

(defun replace-continue (parsed-for body)
  (first (nreplace-clauses body
                           (and (typep !el! HyExpression) (= (car !el!) 'continue))
                           `(do
                              (setv ~@(flatten-1 parsed-for))
                              (continue)))))


(defclass Return [Exception]
  (defn __init__ [self val]
    (setf (. self val) val)))

(defclass TaggedReturn [Exception]
  (defn __init__ [self  tag val]
    (setf (. self tag) tag)
    (setf (. self val) val)))

(defun get-init-val (replacer)
  (cond/cl
    ((in replacer [replace-collect replace-append]) [])
    ((= replacer replace-minimize) 10000000000000)
    ((= replacer replace-maximize) -10000000000000)
    ((in replacer [replace-sum replace-count]) 0)))

(defun get-update-fn (replacer acc-sym)
  (cond/cl
    ((= replacer replace-collect) `(. ~acc-sym append))
    ((= replacer replace-append) `(. ~acc-sym extend))))

(defmacro/g! itr (&rest clauses)  
  (let ((g!parsed (parse-clauses clauses g!ret)))
    (let ((body (get g!parsed :body))
           (accs nil)
           (res nil)
           (init-var nil)
          (update-fn nil))
      (for [el [replace-collect replace-append
                replace-minimize replace-sum replace-maximize 
                replace-count]]   
        (setf res (el g!ret g!update body)
              body (get res 0)
              acc-ls (get res 1)
              flag (get res 2))
        (when flag
          (setf accs (append accs (mapcan (lambda (ls)
                                            (setf init-list [(get ls 0) (get-init-val el)])
                                            (when (get ls 1)
                                              (.extend init-list [(get ls 1) (get-update-fn el (get ls 0))]))
                                            init-list)
                                          acc-ls))
                init-var (get-init-val el)
                update-fn (get-update-fn el g!ret))))
      `(do
         (import hyiter.core)
         (try       
           (do         
             (setv ~g!tag ~(get g!parsed :loop-tag))
             (setv ~g!ret ~init-var
                   ~g!update ~update-fn)             
             ~@(replace-return (get g!parsed :initially))             
             (setv ~@accs)
             (try
               (do
                 (setv ~@(flatten-1 (get g!parsed :with)))             
                 (while True
                   ~(when (get g!parsed :drop)
                      `(when (and ~@(get g!parsed :drop))
                         (setv ~@(flatten-1 (get g!parsed :for)))
                         (continue)))               
                   ~@(replace-continue (get g!parsed :for)
                                       (replace-return body)) 
                   (setv ~@(flatten-1 (get g!parsed :for)))
                   (when (or ~@(get g!parsed :break))
                     (break))))
               (except [e StopIteration]
                 None))         
             ~@(replace-return (get g!parsed :finally)) 
             ~g!ret)
           (except [r hyiter.core.Return]
             (. r val))
           (except [tr hyiter.core.TaggedReturn]
             (if (= (. tr tag) ~g!tag)
                 (. tr val)
                 (raise tr))))))))
