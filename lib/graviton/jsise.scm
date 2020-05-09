;;;
;;; jsise.scm - Javascript in S-Expression
;;;
;;;   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
;;;   All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module graviton.jsise
  (use file.util)
  (use gauche.collection)
  (use gauche.threads)
  (use rfc.json)
  (use rfc.sha)
  (use text.tree)
  (use util.digest)
  (use util.list)
  (use util.match)

  (export define-jsise-stmt
          define-jsise-macro
          define-jsvar
          define-jsfn
          inline-js
          write-js-code
          get-js-code
          js-main-module-absolute-paths
          register-js-stmt!
          import-js
          load-js
          load-js-list
          js-current-main-module
          js-vm-current-main-module))

(select-module graviton.jsise)

(define env-counter-atom (atom -1))

(define-class <js-env> ()
  ((env-id :init-keyword :env-id)
   (parent :init-keyword :parent)
   (var-alist :init-keyword :var-alist)
   (defvar-alist :init-value '())
   (next-id :init-keyword :next-id)))

;; For test.
(define (reset-env-counter!)
  (atomic-update! env-counter-atom (^x -1)))

(define (make-env parent vars)
  (define (js-var id)
    (format "_~a" (number->string id 36)))
  (let loop ((var-alist '())
             (vars vars)
             (id (if parent
                     (slot-ref parent 'next-id)
                     0)))
    (match vars
      (()
       (let1 env-id (atomic-update! env-counter-atom (cut + 1 <>))
         (make <js-env> :env-id env-id :parent parent :var-alist var-alist :next-id id)))
      ((? symbol? var)
       (loop (acons var (js-var id) var-alist) '() (+ id 1)))
      ((var . rest)
       (loop (acons var (js-var id) var-alist) rest (+ id 1))))))

(define (register-js-defvar! env var)
  (let* ((env-id (slot-ref env 'env-id))
         (defvar-alist (slot-ref env 'defvar-alist))
         (js-var (format "_g~a_~a" env-id (length defvar-alist))))
    (slot-set! env 'defvar-alist (acons var js-var defvar-alist))))

(define (resolve-js-local-var env var)
  (if env
      (or (assq-ref (slot-ref env 'var-alist) var #f)
          (assq-ref (slot-ref env 'defvar-alist) var #f)
          (resolve-js-local-var (slot-ref env 'parent) var))
        #f))

(define (resolve-js-var env var)
  (let1 parts (string-split (symbol->string var) ".")
    (cond
      ((= (length parts) 1)
       (or (resolve-js-local-var env var)
           var))
      ((< 0 (string-length (car parts)))
       (intersperse "." (cons (resolve-js-var env (string->symbol (car parts))) (cdr parts))))
      (else
       (intersperse "." parts)))))

(define *js-env-table* (make-hash-table 'equal?))

(define (find-js-env name)
  (or (hash-table-get *js-env-table* name #f)
      (let1 env (make-env #f '())
        (hash-table-put! *js-env-table* name env)
        env)))

;;;

(define print-string (with-module rfc.json print-string))

(define *unary-op-table* (make-hash-table))
(define *infix-op-table* (make-hash-table))
(define *statement-table* (make-hash-table))
(define *macro-table* (make-hash-table))

(define-syntax define-jsise-macro
  (syntax-rules ()
    ((_ name (pat body ...) ...)
     (hash-table-put! *macro-table*
                      'name
                      (match-lambda
                        (pat body ...) ...
                        (x (errorf "Invalid syntax for macro '~a': ~s" 'name (cons 'name x))))))))

(define (is-macro? name)
  (hash-table-contains? *macro-table* name))

(define-jsise-macro cond
  ((('else else ...))
   `(begin ,@else))
  (((expr then ...))
   `(if ,expr (begin ,@then)))
  (((expr then ...) ('else else ...))
   `(if ,expr (begin ,@then) (begin ,@else)))
  (((expr then ...) rest ...)
   `(if ,expr (begin ,@then) (cond ,@rest))))

(define-jsise-macro ash
  ((v (? (^x (and (integer? x) (positive? x))) s))
   `(js-code "(" ,v "<<" ,s ")"))
  ((v (? (^x (and (integer? x) (negative? x))) s))
   `(js-code "(" ,v ">>" ,(- s) ")"))
  ((v (? (cut equal? <> 0) s))
   v)
  ((v s)
   (let ((x (gensym))
         (y (gensym)))
   `(let ((,x ,v)
          (,y ,s))
     (if (<= 0 ,y)
         (js-code "(" ,x "<<" ,y ")")
         (js-code "(" ,x ">>" "(-" ,y ")" ")"))))))

(define-jsise-macro dotimes
  (((var num-expr) body ...)
   (let ((end (gensym)))
     `(let ((,var 0)
            (,end ,num-expr))
        (while (< ,var ,end)
          ,@body
          (pre++ ,var))))))

(define-jsise-macro let*
  ((() body ...)
   `(begin ,@body))
  (((var-spec rest ...) body ...)
   `(let (,var-spec)
      (let* ,rest
        ,@body))))

(define-jsise-macro let1
  ((var expr body ...)
   `(let ((,var ,expr))
      ,@body)))

(define (compile-jsise-stmt env stmt)
  (cond
    ((and (list? stmt)
          (hash-table-get *macro-table* (list-ref stmt 0) #f))
     => (lambda (translator)
          (compile-jsise-stmt env (translator (cdr stmt)))))
    ((and (list? stmt)
          (hash-table-get *statement-table* (list-ref stmt 0) #f))
     => (lambda (translator)
          (translator env (cdr stmt))))
    (else
     (list (compile-jsise-expr env stmt) ";"))))

(define (is-stmt? name)
  (hash-table-contains? *statement-table* name))

(define-syntax define-jsise-stmt
  (syntax-rules ()
    ((_ name env (pat body ...) ...)
     (hash-table-put! *statement-table*
                      'name
                      (lambda (env args)
                        (match args
                          (pat body ...) ...
                          (_ (errorf "Invalid syntax for statement '~a': ~s" 'name (cons 'name _)))))))))

(define-jsise-stmt import env
  (((? symbol? var) name)
   (list "const " var "=require('" name "');"))
  (((? list? vars) name)
   (list "const {" (intersperse "," vars) "}=require('" name "');")))

(define-jsise-stmt begin env
  ((stmt ...)
   (list "{"
         (map (cut compile-jsise-stmt env <>) stmt)
         "}")))

(define-jsise-stmt let env
  ((form body ...)
   (let1 env (make-env env (map (cut list-ref <> 0) form))
     (list "{"
           (map (match-lambda
                  ((var init)
                   (list "let " (resolve-js-var env var) "=" (compile-jsise-expr env init) ";")))
                form)
           (map (cut compile-jsise-stmt env <>) body)
           "}"))))

(define-jsise-stmt if env
  ((expr then)
   (list "if" "(" (compile-jsise-expr env expr) ")"
         (compile-jsise-stmt env then)))
  ((expr then else)
   (list "if" "(" (compile-jsise-expr env expr) ")"
         (compile-jsise-stmt env then)
         "else "
         (compile-jsise-stmt env else))))

(define-jsise-stmt when env
  ((expr body ...)
   (compile-jsise-stmt env `(if ,expr (begin ,@body)))))

(define-jsise-stmt unless env
  ((expr body ...)
   (compile-jsise-stmt env `(if (not ,expr) (begin ,@body)))))

(define-jsise-stmt while env
  ((expr body ...)
   (list "while" "(" (compile-jsise-expr env expr) ")" "{"
         (map (cut compile-jsise-stmt env <>) body)
         "}")))

(define-jsise-stmt define env
  (((name args ...) body ...)
   (compile-jsise-stmt env `(define ,name (lambda ,args ,@body))))
  ((var val)
   (register-js-defvar! env var)
   (list "let " (resolve-js-var env var) "=" (compile-jsise-expr env val) ";")))

(define-jsise-stmt for-each env
  ((proc coll)
   (list (compile-jsise-expr env coll) ".forEach(" (compile-jsise-expr env proc) ");")))

(define-jsise-stmt case env
  ((key clauses ...)
   (list "switch(" (compile-jsise-expr env key) ")" "{"
         (map (lambda (clause)
                (match clause
                  (('else body ...)
                   (list "default:" (map (cut compile-jsise-stmt env <>) body) "break;"))
                  (((data ...) body ...)
                   (list (map (lambda (datum)
                                (list "case " (compile-jsise-expr env datum) ":"))
                              data)
                         (map (cut compile-jsise-stmt env <>) body)
                         "break;"))
                  (_
                   (errorf "Invalid clause in case: ~s" clause))))
              clauses)
         "}")))

(define-syntax define-jsise-unary
  (syntax-rules ()
    ((_ op js-op)
     (hash-table-put! *unary-op-table* 'op js-op))))

(define-syntax define-jsise-infix
  (syntax-rules ()
    ((_ op js-op)
     (hash-table-put! *infix-op-table* 'op js-op))))

(define-jsise-unary not "!")
(define-jsise-unary lognot "~")
(define-jsise-unary - "-")

(define-jsise-infix or "||")
(define-jsise-infix and "&&")
(define-jsise-infix logior "|")
(define-jsise-infix logand "&")
(define-jsise-infix logxor "^")
(define-jsise-infix equal? "===")
(define-jsise-infix < "<")
(define-jsise-infix <= "<=")
(define-jsise-infix > ">")
(define-jsise-infix >= ">=")
(define-jsise-infix + "+")
(define-jsise-infix - "-")
(define-jsise-infix * "*")
(define-jsise-infix / "/")
(define-jsise-infix modulo "%")

(define (compile-jsise-expr env expr)
  (match expr
    ((? real? num)
     (list num))
    ((? string? str)
     (list (with-output-to-string
             (lambda ()
               (print-string str)))))
    (#t
     '("true"))
    (#f
     '("false"))
    ('null
     '("null"))
    ((? undefined? undef)
     '("undefined"))
    ((? symbol? var)
     (list (resolve-js-var env var)))
    ((? vector? vec)
     (list "[" (intersperse "," (map (cut compile-jsise-expr env <>) vec)) "]"))

    (((? is-macro? macro) args ...)
     (let1 translator (hash-table-get *macro-table* macro)
       (compile-jsise-expr env (translator (cdr expr)))))

    (('json obj)
     (list (construct-json-string obj)))
    (('object (key . val) ...)
     (list "{"
           (map (lambda (k v)
                  (unless (string? k)
                    (errorf "The key of the object must be string, but got ~s" k))
                  (list (with-output-to-string
                          (lambda ()
                            (print-string k)))
                        ":"
                        (compile-jsise-expr env v)))
                key val)
           "}"))
    (('js-code body ...)
     (letrec ((traverse (lambda (body)
                          (map (^x (match x
                                     ((? symbol? var)
                                      (resolve-js-var env var))
                                     ((? list? body)
                                      (traverse body))
                                     (_
                                      x)))
                               body))))
       (traverse body)))
    (('ref expr attrs ...)
     (list (compile-jsise-expr env expr) "." (string-join (map symbol->string attrs) ".")))
    (('aref expr key)
     (list (compile-jsise-expr env expr) "[" (compile-jsise-expr env key) "]"))
    (('begin expr ...)
     (list "(" (intersperse "," (map (cut compile-jsise-expr env <>) expr)) ")"))
    (('let form body ...)
     (let ((vars (map (cut list-ref <> 0) form))
           (inits (map (cut list-ref <> 1) form)))
       (compile-jsise-expr env `((lambda ,vars ,@body) ,@inits))))
    (('lambda args expr ...)
     (let1 env (make-env env args)
       (list "(" "(" (intersperse ","
                                  (let loop ((args args)
                                             (js-args '()))
                                    (match args
                                      (()
                                       (reverse js-args))
                                      ((? symbol? arg)
                                       (loop '() (cons (format "...~a" (resolve-js-var env arg)) js-args)))
                                      ((arg . rest)
                                       (loop rest (cons (resolve-js-var env arg) js-args))))))
             ")" "=>" "{"
             (receive (heads tail) (split-at expr (- (length expr) 1))
               (list (map (cut compile-jsise-stmt env <>) heads)
                     "return " (compile-jsise-expr env (list-ref tail 0)) ";"))
             "}" ")")))
    (('if expr then)
     (compile-jsise-expr env `(if ,expr ,then undefined)))
    (('if expr then else)
     (list "(" (compile-jsise-expr env expr) "?"
           (compile-jsise-expr env then) ":"
           (compile-jsise-expr env else) ")"))
    (('set! lvar expr)
     (list "(" (compile-jsise-expr env lvar) "=" (compile-jsise-expr env expr) ")"))
    (('vector-ref var i)
     (list (compile-jsise-expr env var) "[" (compile-jsise-expr env i) "]"))
    (('vector-set! (? symbol? var) i v)
     (list "(" (resolve-js-var env var) "[" (compile-jsise-expr env i) "]=" (compile-jsise-expr env v) ")"))
    (('make klass args ...)
     (list "(" "new " klass "(" (map (cut compile-jsise-expr env <>) args) ")" ")"))
    (('pre++ (? symbol? var))
     (list "(" "++" (resolve-js-var env var) ")"))
    (('pre-- (? symbol? var))
     (list "(" "--" (resolve-js-var env var) ")"))
    (('post++ (? symbol? var))
     (list "(" (resolve-js-var env var) "++" ")"))
    (('post-- (? symbol? var))
     (list "(" (resolve-js-var env var) "--" ")"))
    (((? (cut hash-table-contains? *unary-op-table* <>) op) arg)
     (list "(" (hash-table-get *unary-op-table* op) (compile-jsise-expr env arg) ")"))
    (((? (cut hash-table-contains? *infix-op-table* <>) op) args ...)
     (list "(" (intersperse (hash-table-get *infix-op-table* op) (map (cut compile-jsise-expr env <>) args)) ")"))
    (((? is-stmt? _) rest ...)
     (list "(" "()=>{" (compile-jsise-stmt env expr) "return undefined;" "}" ")" "()"))
    ((f args ...)
     (list (compile-jsise-expr env f) "(" (intersperse "," (map (cut compile-jsise-expr env <>) args)) ")"))
    (_
     (errorf "Invalid expression: ~s" expr))))

(define (scheme->js-main-module-name scheme-module-name)
  (string-append (digest-hexify (sha1-digest-string (x->string scheme-module-name))) ".mjs"))

(define (js-vm-current-main-module)
  (scheme->js-main-module-name (module-name ((with-module gauche.internal vm-current-module)))))

(define-syntax js-current-main-module
  (syntax-rules ()
    ((_)
     (scheme->js-main-module-name (module-name (current-module))))))

(define-syntax define-jsvar
  (syntax-rules ()
    ((_ name val)
     (register-js-stmt! (js-current-main-module) '(define name val)))))

(define-syntax define-jsfn
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (register-js-stmt! (js-current-main-module) '(define (name args ...) body ...)))))

(define-syntax inline-js
  (syntax-rules ()
    ((_ body ...)
     (register-js-stmt! (js-current-main-module) '(begin body ...)))))

;;;

(define (absolute-js-path js-path)
  (cond
    ((absolute-path? js-path)
     js-path)
    (else
     (build-path "/js" js-path))))

(define (compile-import-option import-options)
  (let ((as-param (get-keyword :as import-options #f))
        (only-params (map x->string (get-keyword :only import-options '())))
        (rename-params (map (match-lambda
                             ((from-sym to-sym)
                              (list (x->string from-sym) " as " (x->string to-sym))))
                           (get-keyword :rename import-options '()))))
    (cond
      ((and as-param
            (null? only-params)
            (null? rename-params))
       (list "* as " (x->string as-param)))
      ((and (not as-param)
            (not (and (null? only-params) (null? rename-params))))
       (list "{" (intersperse "," (append only-params rename-params)) "}"))
      (else
       (errorf "import-js spec must have either :as or :only/:rename, but got ~s" import-options)))))

(define (compile-import-spec import-spec)
  (match import-spec
    (((? string? module-path) import-options ...)
     `(js-code "import " ,(compile-import-option import-options) " from '" ,(absolute-js-path module-path) "';"))
    (_
     (errorf "Invalid import-spec: ~s" import-spec))))

(define-syntax import-js
  (syntax-rules ()
    ((_)
     (begin))
    ((_ import-spec rest ...)
     (begin
       (register-js-stmt! (js-current-main-module) (compile-import-spec 'import-spec))
       (import-js rest ...)))))

;;;

(define *load-js-list* '())

(define (load-js js-path)
  (push! *load-js-list* (absolute-js-path js-path)))

(define (load-js-list)
  (reverse *load-js-list*))

;;;

(define *js-code-table* (make-hash-table 'equal?))

(define (register-js-stmt! name stmt)
  (let1 env (find-js-env name)
    (hash-table-push! *js-code-table* name (compile-jsise-stmt env stmt))))

(define (write-js-code name out)
  (display "import * as Graviton from '/js/graviton/graviton.mjs';" out)
  (write-tree (reverse (hash-table-get *js-code-table* name)) out))

(define (get-js-code name)
  (cond
    ((hash-table-exists? *js-code-table* name)
     (call-with-output-string
       (lambda (out)
         (write-js-code name out))))
    (else
     #f)))

(define (js-main-module-absolute-paths)
  (map absolute-js-path (hash-table-keys *js-code-table*)))

