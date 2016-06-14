(load "chez-load-parse.scm")
(load "mk/test-check.scm")


(test "parse-1"
  (run 1 (q) (parseo '(begin (define append (lambda (x) cons)) (list list))))
  '(_.0))

(test "parse-2"
  (run 1 (q) (parseo '(begin (define append (lambda (x) con)) (list list))))
  '(_.0))

(test "parse-3"
  (run 1 (q) (parseo '(begin (define append (lambda (x) lambda)) (list list))))
  '())

(test "parse-4"
  (run 1 (q) (parseo '(begin (define append (lambda (x) (lambda (lambda) lambda))) (list list))))
  '(_.0))

(test "parse-5"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)) (append 5))))
  '(_.0))

(test "parse-6"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)) (appen 5))))
  '(_.0))

(test "parse-7"
  (run 1 (q) (parseo '(begin (define append (lambda x x)) (append 5))))
  '(_.0))

(test "parse-8"
  (run 1 (q) (parseo '(begin (define append (lambda x)) (append 5))))
  '())

(test "parse-9"
  (run 1 (q) (parseo '(begin (defyn append (lambda (x) x)) (append 5))))
  '())

(test "parse-10"
  (run 1 (q) (parseo '(begin (define append (lambda (x) x)))))
  '())

(test "parse-11"
  (run 1 (q) (parseo '(begin (define append (lambda (x) cons)) (list list))))
  '(_.0))

(test "parse-12"
  (run 1 (q) (parseo '(begin (define append (lambda (x) con)) (list list))))
  '(_.0))



;;; should be able to check the definition is legal with queries like:

(test "parse-defn-no-tests-1"
  (run 1 (q) (parseo `(begin (define append (lambda (x) cons)) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-2"
  (run 1 (q) (parseo `(begin (define append (lambda (x) con)) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-3"
  (run 1 (q) (parseo `(begin (define append (lambda cons)) . ,q)))
  '())

(test "parse-defn-no-tests-4"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if))) . ,q)))
  '())

(test "parse-defn-no-tests-5"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if 3 4 5))) . ,q)))
  '(((_.0) (num _.0))))

(test "parse-defn-no-tests-6"
  (run 1 (q) (parseo `(begin (define append (lambda (x) (if 3))) . ,q)))
  '())
