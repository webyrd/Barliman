(define evalo
  (lambda (expr val)
    (eval-expro expr '() val)))

(define eval-expro
  (lambda (expr env val)
    (conde
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) expr)
         (eval-expro rator env `(closure ,x ,body ,env^))
         (eval-expro rand env a)
         (eval-expro body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)
         (not-in-envo 'lambda env)))
      ((symbolo expr) (lookupo expr env val)))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))

(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (== y x)
         (== v t)))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env) (=/= y x)
         (lookupo x rest t))))))
