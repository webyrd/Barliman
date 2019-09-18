(ns rai-clojure.rai
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))


;; concrete interp
(defn lookupo [x env val]
  (fresh [y v rest]
    (conso [y v] rest env)
    (conde
      ((== x y) (== v val))
      ((!= x y) (lookupo x rest val)))))

(defn eval-expro [expr env val]
  (conde
    ((fresh [x]
       (== ['var x] expr)
       (lookupo x env val)))
    ((fresh [i]
       (== ['int i] expr)
       (== ['int i] val)
       (fd/in i (fd/interval -1000 1000))))
    ((fresh [e1 e2 n1 n2 n3]
       (== ['plus e1 e2] expr)
       (== ['int n3] val)
       (fd/in n1 n2 n3 (fd/interval -1000 1000))
       (fd/+ n1 n2 n3)
       (eval-expro e1 env ['int n1])
       (eval-expro e2 env ['int n2])))
    ((fresh [e1 e2 n1 n2 n3]
       (== ['times e1 e2] expr)
       (== ['int n3] val)
       (fd/in n1 n2 n3 (fd/interval -1000 1000))
       (fd/* n1 n2 n3)
       (eval-expro e1 env ['int n1])
       (eval-expro e2 env ['int n2])))
    ((fresh [x y body]
       (== ['lam x y body] expr)
       (== ['clos x y body env] val)))
    ((fresh [e1 e2 x y body env1 env2 env3 arg]
       (== ['app e1 e2] expr)
       (eval-expro e1 env ['clos x y body env1])
       (eval-expro e2 env arg)
       (conso [x ['clos x y body env1]] env1 env2)
       (conso [y arg] env2 env3)
       (eval-expro body env3 val)))
    ((fresh [e1 e2 e3 n]
       (== ['if0 e1 e2 e3] expr)
       (fd/in n (fd/interval -1000 1000))            
       (eval-expro e1 env ['int n])
       (conde
         [(fd/== n 0)
          (eval-expro e2 env val)]
         [(fd/!= n 0)
          (eval-expro e3 env val)])))))

(defn evalo [expr val]
  (eval-expro expr [] val))