;;;
;;; jsffi.scm - JavaScript Foreign Function Interface
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

(define-module graviton.jsffi
  (use binary.io)
  (use data.queue)
  (use file.util)
  (use gauche.charconv)
  (use gauche.collection)
  (use gauche.hook)
  (use gauche.parameter)
  (use gauche.partcont)
  (use gauche.record)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.uvector)
  (use graviton.app)
  (use graviton.async)
  (use graviton.comm)
  (use graviton.misc)
  (use rfc.json)
  (use rfc.sha)
  (use srfi-1)
  (use srfi-13)
  (use srfi-42)
  (use text.tree)
  (use util.digest)
  (use util.list)
  (use util.match)

  (export define-jsstmt
          define-jsmacro
          define-jsvar
          define-jsfn

          get-js-code
          js-main-module-absolute-paths
          load-js+type-list

          import-js
          load-js

          define-jsenum
          jslet
          jslet/async
          jslet/await

          <jsobject>
          <jsobject-meta>
          define-jsobject-method
          define-automatic-jsobject-methods
          jsobject-id
          jsobject-property-ref
          jsobject-property-set!
          jsobject-free!
          with-jsobjects
          let-jsproperties
          make-global-jsobject-provider
          define-global-jsobject

          <jsobject-provider>

          <json-value-meta>
          <json-value-mixin>
          obj->json
          json->obj
          json-rule

          unlink-callback
          unlink-procedure
          shift-callback*
          ))

(select-module graviton.jsffi)

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

(define *statement-table* (make-hash-table))
(define *macro-table* (make-hash-table))
(define *expr-table* (make-hash-table))

(define-syntax define-jsmacro
  (syntax-rules ()
    ((_ name (pat body ...) ...)
     (hash-table-put! *macro-table*
                      'name
                      (match-lambda
                        (pat body ...) ...
                        (x (errorf "Invalid syntax for macro '~a': ~s" 'name (cons 'name x))))))))

(define (is-macro? name)
  (hash-table-contains? *macro-table* name))

(define-jsmacro cond
  ((('else else ...))
   `(begin ,@else))
  (((expr then ...))
   `(if ,expr (begin ,@then)))
  (((expr then ...) ('else else ...))
   `(if ,expr (begin ,@then) (begin ,@else)))
  (((expr then ...) rest ...)
   `(if ,expr (begin ,@then) (cond ,@rest))))

(define-jsmacro ash
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

(define-jsmacro dotimes
  (((var num-expr) body ...)
   (let ((end (gensym)))
     `(let ((,var 0)
            (,end ,num-expr))
        (while (< ,var ,end)
          ,@body
          (pre++ ,var))))))

(define-jsmacro let*
  ((() body ...)
   `(begin ,@body))
  (((var-spec rest ...) body ...)
   `(let (,var-spec)
      (let* ,rest
        ,@body))))

(define-jsmacro let1
  ((var expr body ...)
   `(let ((,var ,expr))
      ,@body)))

(define-jsmacro rlet1
  ((var expr body ...)
   `((lambda (var)
       (begin body ...)
       (return var))
     expr)))

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

(define-syntax define-jsstmt
  (syntax-rules ()
    ((_ name env (pat body ...) ...)
     (hash-table-put! *statement-table*
                      'name
                      (lambda (env args)
                        (match args
                          (pat body ...) ...
                          (_ (errorf "Invalid syntax for statement '~a': ~s" 'name (cons 'name _)))))))))

(define-jsstmt import env
  (((? symbol? var) name)
   (list "const " var "=require('" name "');"))
  (((? list? vars) name)
   (list "const {" (intersperse "," vars) "}=require('" name "');")))

(define-jsstmt begin env
  ((stmt ...)
   (list "{"
         (map (cut compile-jsise-stmt env <>) stmt)
         "}")))

(define-jsstmt let env
  ((form body ...)
   (let1 env (make-env env (map (cut list-ref <> 0) form))
     (list "{"
           (map (match-lambda
                  ((var init)
                   (list "let " (resolve-js-var env var) "=" (compile-jsise-expr env init) ";")))
                form)
           (map (cut compile-jsise-stmt env <>) body)
           "}"))))

(define-jsstmt if env
  ((expr then)
   (list "if" "(" (compile-jsise-expr env expr) ")"
         (compile-jsise-stmt env then)))
  ((expr then else)
   (list "if" "(" (compile-jsise-expr env expr) ")"
         (compile-jsise-stmt env then)
         "else "
         (compile-jsise-stmt env else))))

(define-jsstmt when env
  ((expr body ...)
   (compile-jsise-stmt env `(if ,expr (begin ,@body)))))

(define-jsstmt unless env
  ((expr body ...)
   (compile-jsise-stmt env `(if (not ,expr) (begin ,@body)))))

(define-jsstmt while env
  ((expr body ...)
   (list "while" "(" (compile-jsise-expr env expr) ")" "{"
         (map (cut compile-jsise-stmt env <>) body)
         "}")))

(define-jsstmt define env
  (((name args ...) body ...)
   (compile-jsise-stmt env `(define ,name (lambda ,args ,@body))))
  ((var val)
   (register-js-defvar! env var)
   (list "let " (resolve-js-var env var) "=" (compile-jsise-expr env val) ";")))

(define-jsstmt for-each env
  ((proc coll)
   (list (compile-jsise-expr env coll) ".forEach(" (compile-jsise-expr env proc) ");")))

(define-jsstmt case env
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

(define-jsstmt respond env
  ((vals ...)
   (cond
     ((resolve-js-local-var env '%notification-id)
      (compile-jsise-stmt env `(begin
                                 (Graviton.notifyValues %notification-id ,@vals)
                                 (set! %notification-id #f))))
     (else
      (error "respond is called outside jslet/await.")))))

(define-jsstmt return env
  (()
   (list "return;"))
  ((val)
   (list "return " (compile-jsise-expr env val) ";")))

(define-jsstmt inc! env
  ((var)
   (compile-jsise-stmt env `(set! ,var (+ ,var 1))))
  ((var delta)
   (compile-jsise-stmt env `(set! ,var (+ ,var ,delta)))))

(define-jsstmt dec! env
  ((var)
   (compile-jsise-stmt env `(set! ,var (- ,var 1))))
  ((var delta)
   (compile-jsise-stmt env `(set! ,var (- ,var ,delta)))))

(define-syntax define-jsexpr
  (syntax-rules ()
    ((_ alias name)
     (hash-table-put! *expr-table* 'alias (hash-table-get *expr-table* 'name)))
    ((_ name env (pat body ...) ...)
     (hash-table-put! *expr-table*
                      'name
                      (lambda (env args)
                        (match args
                          (pat body ...) ...
                          (x (errorf "Invalid syntax for expr '~a': ~s" 'name (cons 'name x)))))))))

(define-jsexpr vector env
  ((vals ...)
   (list "[" (intersperse "," (map (cut compile-jsise-expr env <>) vals)) "]")))

(define-jsexpr json env
  ((obj)
   (list (construct-json-string obj))))

(define-jsexpr object env
  (((key . val) ...)
   (list "{"
         (intersperse "," (map (lambda (k v)
                                 (unless (string? k)
                                   (errorf "The key of the object must be string, but got ~s" k))
                                 (list (with-output-to-string
                                         (lambda ()
                                           (print-string k)))
                                       ":"
                                       (compile-jsise-expr env v)))
                               key val))
         "}")))

(define-jsexpr js-code env
  ((body ...)
   (letrec ((traverse (lambda (body)
                        (map (^x (match x
                                   ((? symbol? var)
                                    (resolve-js-var env var))
                                   ((? list? body)
                                    (traverse body))
                                   (_
                                    x)))
                             body))))
     (traverse body))))

(define-jsexpr ref env
  ((expr attrs ...)
   (list (compile-jsise-expr env expr) (map (lambda (attr)
                                              (match attr
                                                (('quote sym)
                                                 (list "." (symbol->string sym)))
                                                (_
                                                 (list "[" (compile-jsise-expr env attr) "]"))))
                                            attrs))))

(define-jsexpr ~ ref)

(define-jsexpr begin env
  ((expr ...)
   (list "(" (intersperse "," (map (cut compile-jsise-expr env <>) expr)) ")")))

(define-jsexpr let env
  ((form body ...)
   (let ((vars (map (cut list-ref <> 0) form))
         (inits (map (cut list-ref <> 1) form)))
     (compile-jsise-expr env `((lambda ,vars ,@body) ,@inits)))))

;; Parse the arguments of lambda for jsffi.
;; Returns (arg1 arg2 ...) and (restarg). If restarg doesn't exist, the restarg list will be nil.
(define (parse-lambda-args jsargs)
  (let loop ((args '())
             (restargs '())
             (jsargs jsargs))
    (match jsargs
      (()
       (values (reverse args) (reverse restargs)))
      ((:rest (? symbol? jsrestarg))
       (loop jsarg (list jsrestarg) '()))
      ((? symbol? jsrestarg)
       (loop jsarg (list jsrestarg) '()))
      (((? symbol? jsarg) rest ...)
       (loop (cons jsarg args) restargs rest))
      (_
       (errorf "Invalid arg spec for jsffi lambda: ~s" jsargs)))))

(define-jsexpr lambda env
  ((jsargs stmt ...)
   (receive (args restargs) (parse-lambda-args jsargs)
     (let1 env (make-env env (append args restargs))
       (list "(" "(" (intersperse ","
                                  (append (map (lambda (arg)
                                                 (resolve-js-var env arg))
                                               args)
                                          (map (lambda (restarg)
                                                 (format "...~a" (resolve-js-var env restarg)))
                                               restargs)))
             ")" "=>" "{"
             (map (cut compile-jsise-stmt env <>) stmt)
             "}" ")")))))

(define-jsexpr if env
  ((expr then)
   (compile-jsise-expr env `(if ,expr ,then undefined)))
  ((expr then else)
   (list "(" (compile-jsise-expr env expr) "?"
         (compile-jsise-expr env then) ":"
         (compile-jsise-expr env else) ")")))

(define-jsexpr set! env
  ((lvar expr)
   (list "(" (compile-jsise-expr env lvar) "=" (compile-jsise-expr env expr) ")")))

(define-jsexpr vector-ref env
  ((var i)
   (list (compile-jsise-expr env var) "[" (compile-jsise-expr env i) "]")))

(define-jsexpr vector-set! env
  ((var i v)
   (list "(" (resolve-js-var env var) "[" (compile-jsise-expr env i) "]=" (compile-jsise-expr env v) ")")))

(define-jsexpr make env
  ((klass args ...)
   (list "(" "new " klass "(" (intersperse "," (map (cut compile-jsise-expr env <>) args)) ")" ")")))

(define-jsexpr new make)

(define-jsexpr pre++ env
  ((var)
   (list "(" "++" (resolve-js-var env var) ")")))

(define-jsexpr pre-- env
  ((var)
   (list "(" "--" (resolve-js-var env var) ")")))

(define-jsexpr post++ env
  ((var)
   (list "(" (resolve-js-var env var) "++" ")")))

(define-jsexpr post-- env
  ((var)
   (list "(" (resolve-js-var env var) "--" ")")))

(define-jsexpr is-a? env
  ((expr klass)
   (list "(" (compile-jsise-expr env expr) " instanceof " klass ")")))

(define-jsexpr json-value env
  ((obj rule)
   (compile-jsise-expr env `(Graviton.convertObject ,obj ,rule)))
  ((obj)
   (compile-jsise-expr env `(Graviton.convertObject ,obj))))


(define-syntax define-jsise-unary
  (syntax-rules ()
    ((_ op js-op)
     (define-jsexpr op env
       ((arg)
        (list "(" js-op (compile-jsise-expr env arg) ")"))))))

(define-syntax define-jsise-infix
  (syntax-rules ()
    ((_ op js-op)
     (define-jsexpr op env
       ((args (... ...))
        (list "(" (intersperse js-op (map (cut compile-jsise-expr env <>) args)) ")"))))))

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
  (define identifiers '((#t "true") (#f "false") (null "null") (undefined "undefined")))
  (cond
    ((list? expr)
     (let1 lsym (car expr)
       (cond
         ((is-macro? lsym)
          (let1 translator (hash-table-get *macro-table* lsym)
            (compile-jsise-expr env (translator (cdr expr)))))
         ((hash-table-get *expr-table* lsym #f)
          => (lambda (proc)
               (proc env (cdr expr))))
         (else
          (list (compile-jsise-expr env lsym) "(" (intersperse "," (map (cut compile-jsise-expr env <>) (cdr expr))) ")")))))
    ((real? expr)
     (list expr))
    ((string? expr)
     (list (with-output-to-string
             (lambda ()
               (print-string expr)))))
    ((symbol? expr)
     (list (resolve-js-var env expr)))
    ((vector? expr)
     (list "[" (intersperse "," (map (cut compile-jsise-expr env <>) expr)) "]"))
    ((undefined? expr)
     '("undefined"))
    ((assoc-ref identifiers expr #f)
     => values)
    (else
     (errorf "Invalid expression: ~s" expr))))

(define (scheme->js-main-module-name scheme-module-name)
  (string-append "/_m/" (digest-hexify (sha1-digest-string (x->string scheme-module-name))) ".mjs"))

(define (js-vm-current-main-module)
  (scheme->js-main-module-name (module-name ((with-module gauche.internal vm-current-module)))))

(define-syntax js-current-main-module
  (syntax-rules ()
    ((_)
     (scheme->js-main-module-name (module-name (current-module))))))

(define-syntax define-jsvar
  (syntax-rules ()
    ((_ name val)
     (register-jsstmt! (js-current-main-module) '(define name val)))
    ((_ name)
     (register-jsstmt! (js-current-main-module) `(define name ,(undefined))))))

(define-syntax define-jsfn
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (register-jsstmt! (js-current-main-module) '(define (name args ...) body ...)))))

(define-syntax inline-js
  (syntax-rules ()
    ((_ body ...)
     (register-jsstmt! (js-current-main-module) '(begin body ...)))))

;;;

(define *js-code-table* (make-hash-table 'equal?))

(define (register-jsstmt! name stmt)
  (let1 env (find-js-env name)
    (hash-table-push! *js-code-table* name (compile-jsise-stmt env stmt))))

(define (write-js-code name out)
  (display "import * as Graviton from '/_g/graviton.mjs';" out)
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
  (hash-table-keys *js-code-table*))


;;;

(define-constant VAL-TYPE-UNDEFINED 1)
(define-constant VAL-TYPE-NULL 2)
(define-constant VAL-TYPE-TRUE 3)
(define-constant VAL-TYPE-FALSE 4)
(define-constant VAL-TYPE-PROCEDURE 5)
(define-constant VAL-TYPE-POSITIVE-INFINITY 6)
(define-constant VAL-TYPE-NEGATIVE-INFINITY 7)
(define-constant VAL-TYPE-NAN 8)
(define-constant VAL-TYPE-STRING 9)
(define-constant VAL-TYPE-SYMBOL 10)
(define-constant VAL-TYPE-OBJECT 11)
(define-constant VAL-TYPE-DATE 12)
(define-constant VAL-TYPE-ARRAY 13)
(define-constant VAL-TYPE-INT8ARRAY 14)
(define-constant VAL-TYPE-UINT8ARRAY 15)
(define-constant VAL-TYPE-INT16ARRAY 16)
(define-constant VAL-TYPE-UINT16ARRAY 17)
(define-constant VAL-TYPE-INT32ARRAY 18)
(define-constant VAL-TYPE-UINT32ARRAY 19)
(define-constant VAL-TYPE-FLOAT32ARRAY 20)
(define-constant VAL-TYPE-FLOAT64ARRAY 21)
(define-constant VAL-TYPE-JSON 22)
(define-constant VAL-TYPE-INT8 23)
(define-constant VAL-TYPE-UINT8 24)
(define-constant VAL-TYPE-INT16 25)
(define-constant VAL-TYPE-UINT16 26)
(define-constant VAL-TYPE-INT32 27)
(define-constant VAL-TYPE-UINT32 28)
(define-constant VAL-TYPE-FLOAT64 29)

(define (encode-string str out)
  (let1 data (ces-convert-to <u8vector> str (gauche-character-encoding) 'utf-8)
    (encode-integer (u8vector-length data) out)
    (write-uvector data out)))

(define (encode-float64 v out)
  (write-s8 VAL-TYPE-FLOAT64 out)
  (write-f64 v out 'little-endian))

(define (encode-integer v out)
  (if (<= 0 v)
    ;; zero or positive
    (cond
      ((<= v #x80)
       (write-s8 (- v) out))
      ((<= v #xff)
       (write-s8 VAL-TYPE-UINT8 out)
       (write-u8 v out))
      ((<= v #xffff)
       (write-s8 VAL-TYPE-UINT16 out)
       (write-u16 v out 'little-endian))
      ((<= v #xffffffff)
       (write-s8 VAL-TYPE-UINT32 out)
       (write-u32 v out 'little-endian))
      (else
       (encode-float64 v out)))
    ;; negative
    (cond
      ((<= #x-80 v)
       (write-s8 VAL-TYPE-INT8 out)
       (write-s8 v out))
      ((<= #x-8000 v)
       (write-s8 VAL-TYPE-INT16 out)
       (write-s16 v out 'little-endian))
      ((<= #x-80000000 v)
       (write-s8 VAL-TYPE-INT32 out)
       (write-s32 v out 'little-endian))
      (else
       (encode-float64 v out)))))

(define (encode-real v out)
  (if (integer? v)
    (encode-integer (inexact->exact v) out)
    (encode-float64 v out)))

(define (encode-symbol sym out)
  (encode-string (symbol->string sym) out))

(define (encode-object obj out)
  (unless (valid-window-context? obj)
    (errorf "~s belongs to ~s, but the current window-context is ~s" obj (jsobject-window-context obj) (window-context)))
  ;; class-id isn't necessary because the object already exists in Javascript world.
  (encode-integer (jsobject-id obj) out))

(define (encode-time t out)
  (encode-real (* (time->seconds t) 1000) out))

(define (encode-array vec out)
  (let1 len (vector-length vec)
    (encode-integer len out)
    (dotimes (i len)
      (encode-value (vector-ref vec i) out))))

(define (encode-uvector uvec out)
  (encode-integer (uvector-length uvec) out)
  (write-uvector uvec out 0 -1 'little-endian))

(define (encode-json json-alist out)
  (encode-integer (length json-alist) out)
  (for-each (lambda (pair)
              (encode-value (car pair) out)
              (encode-value (cdr pair) out))
            json-alist))

(define (encode-callback callback out)
  (encode-integer (link-callback callback) out))

(define (encode-procedure proc out)
  (encode-callback (worker-callback proc) out))

(define (encode-value obj out)
  (cond
    ;; undefined
    ((undefined? obj)
     (write-s8 VAL-TYPE-UNDEFINED out))
    ;; null
    ((eq? obj 'null)
     (write-s8 VAL-TYPE-NULL out))
    ;; true
    ((or (eq? obj #t) (eq? obj 'true))
     (write-s8 VAL-TYPE-TRUE out))
    ;; false
    ((or (eq? obj #f) (eq? obj 'false))
     (write-s8 VAL-TYPE-FALSE out))
    ;; real number
    ((real? obj)
     (encode-real obj out))
    ;; positive infinity
    ((equal? obj +inf.0)
     (write-s8 VAL-TYPE-POSITIVE-INFINITY out))
    ;; negative infinity
    ((equal? obj -inf.0)
     (write-s8 VAL-TYPE-NEGATIVE-INFINITY out))
    ;; NaN
    ((and (number? obj) (nan? obj))
     (write-s8 VAL-TYPE-NAN out))
    ;; string
    ((string? obj)
     (write-s8 VAL-TYPE-STRING out)
     (encode-string obj out))
    ;; symbol
    ((symbol? obj)
     (write-s8 VAL-TYPE-SYMBOL out)
     (encode-symbol obj out))
    ;; object
    ((or (is-a? obj <jsobject>) (is-a? obj <jsobject-provider>))
     (write-s8 VAL-TYPE-OBJECT out)
     (encode-object obj out))
    ;; date
    ((is-a? obj <time>)
     (write-s8 VAL-TYPE-DATE out)
     (encode-time obj out))
    ;; array
    ((vector? obj)
     (write-s8 VAL-TYPE-ARRAY out)
     (encode-array obj out))
    ;; int8array
    ((s8vector? obj)
     (write-s8 VAL-TYPE-INT8ARRAY out)
     (encode-uvector obj out))
    ;; uint8array
    ((u8vector? obj)
     (write-s8 VAL-TYPE-UINT8ARRAY out)
     (encode-uvector obj out))
    ;; int16array
    ((s16vector? obj)
     (write-s8 VAL-TYPE-INT16ARRAY out)
     (encode-uvector obj out))
    ;; uint16array
    ((u16vector? obj)
     (write-s8 VAL-TYPE-UINT16ARRAY out)
     (encode-uvector obj out))
    ;; int32array
    ((s32vector? obj)
     (write-s8 VAL-TYPE-INT32ARRAY out)
     (encode-uvector obj out))
    ;; uint32array
    ((u32vector? obj)
     (write-s8 VAL-TYPE-UINT32ARRAY out)
     (encode-uvector obj out))
    ;; float32array
    ((f32vector? obj)
     (write-s8 VAL-TYPE-FLOAT32ARRAY out)
     (encode-uvector obj out))
    ;; float64array
    ((f64vector? obj)
     (write-s8 VAL-TYPE-FLOAT64ARRAY out)
     (encode-uvector obj out))
    ;; JSON
    ((list? obj)
     (write-s8 VAL-TYPE-JSON out)
     (encode-json obj out))
    ((worker-callback? obj)
     (write-s8 VAL-TYPE-PROCEDURE out)
     (encode-callback obj out))
    ((procedure? obj)
     (write-s8 VAL-TYPE-PROCEDURE out)
     (encode-procedure obj out))
    (else
     (errorf "Unsupported object: ~s" obj))))

(define (decode-string ctx in)
  (let1 len (decode-value ctx in)
    (if (= len 0)
      ""
      (ces-convert (read-uvector <u8vector> len in) 'utf-8))))

(define decoder-table
  (alist->hash-table
    `(
      ;; undefined
      (,VAL-TYPE-UNDEFINED . ,(^(ctx in) (undefined)))
      ;; null
      (,VAL-TYPE-NULL . ,(^(ctx in) 'null))
      ;; true
      (,VAL-TYPE-TRUE . ,(^(ctx in) #t))
      ;; false
      (,VAL-TYPE-FALSE . ,(^(ctx in) #f))
      ;; positive infinity
      (,VAL-TYPE-POSITIVE-INFINITY . ,(^(ctx in) +inf.0))
      ;; negative infinity
      (,VAL-TYPE-NEGATIVE-INFINITY . ,(^(ctx in) -inf.0))
      ;; NaN
      (,VAL-TYPE-NAN . ,(^(ctx in) +nan.0))
      ;; string
      (,VAL-TYPE-STRING . ,(^(ctx in) (decode-string ctx in)))
      ;; symbol
      (,VAL-TYPE-SYMBOL . ,(^(ctx in) (string->symbol (decode-string ctx in))))
      ;; object
      (,VAL-TYPE-OBJECT . ,(^(ctx in)
                             (let* ((class-id (decode-value ctx in))
                                    (jsobj-id (decode-value ctx in)))
                               (window-context-slot-atomic-ref ctx 'jsobject-manager
                                 (lambda (manager)
                                   (or (find-jsobject manager jsobj-id)
                                       (let1 jsobj (make (hash-table-get *id->class-table* class-id)
                                                     :id jsobj-id
                                                     :window-context ctx)
                                         (register-jsobject! manager jsobj)
                                         jsobj)))))))
      ;; date
      (,VAL-TYPE-DATE . ,(^(ctx in) (seconds->time (/. (decode-value ctx in) 1000))))
      ;; array
      (,VAL-TYPE-ARRAY . ,(^(ctx in)
                            (let1 len (decode-value ctx in)
                              (vector-ec (: _ len) (decode-value ctx in)))))
      ;; int8array
      (,VAL-TYPE-INT8ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                           (if (= len 0)
                                             #s8()
                                             (read-uvector <s8vector> len in 'little-endian)))))
      ;; uint8array
      (,VAL-TYPE-UINT8ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                            (if (= len 0)
                                              #u8()
                                              (read-uvector <u8vector> len in 'little-endian)))))
      ;; int16array
      (,VAL-TYPE-INT16ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                            (if (= len 0)
                                              #s16()
                                              (read-uvector <s16vector> len in 'little-endian)))))
      ;; uint16array
      (,VAL-TYPE-UINT16ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                             (if (= len 0)
                                               #u16()
                                               (read-uvector <u16vector> len in 'little-endian)))))
      ;; int32array
      (,VAL-TYPE-INT32ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                            (if (= len 0)
                                              #s32()
                                              (read-uvector <s32vector> len in 'little-endian)))))
      ;; uint32array
      (,VAL-TYPE-UINT32ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                             (if (= len 0)
                                               #u32()
                                               (read-uvector <u32vector> len in 'little-endian)))))
      ;; float32array
      (,VAL-TYPE-FLOAT32ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                              (if (= len 0)
                                                #f32()
                                                (read-uvector <f32vector> len in 'little-endian)))))
      ;; float64array
      (,VAL-TYPE-FLOAT64ARRAY . ,(^(ctx in) (let1 len (decode-value ctx in)
                                              (if (= len 0)
                                                #f64()
                                                (read-uvector <f64vector> len in 'little-endian)))))
      ;; JSON
      (,VAL-TYPE-JSON . ,(^(ctx in)
                           (let1 len (decode-value ctx in)
                             (list-ec (: _ (decode-value ctx in))
                                      (let* ((key (decode-value ctx in))
                                             (val (decode-value ctx in)))
                                        (cons key val))))))
      ;; int8
      (,VAL-TYPE-INT8 . ,(^(ctx in) (read-s8 in)))
      ;; uint8
      (,VAL-TYPE-UINT8 . ,(^(ctx in) (read-u8 in)))
      ;; int16
      (,VAL-TYPE-INT16 . ,(^(ctx in) (read-s16 in 'little-endian)))
      ;; uint16
      (,VAL-TYPE-UINT16 . ,(^(ctx in) (read-u16 in 'little-endian)))
      ;; int32
      (,VAL-TYPE-INT32 . ,(^(ctx in) (read-s32 in 'little-endian)))
      ;; uint32
      (,VAL-TYPE-UINT32 . ,(^(ctx in) (read-u32 in 'little-endian)))
      ;; float64
      (,VAL-TYPE-FLOAT64 . ,(^(ctx in) (read-f64 in 'little-endian))))))

(define (decode-value ctx in)
  (let1 val-type (read-s8 in)
    (cond
      ((eof-object? val-type)
       val-type)
      ((<= val-type 0)
       (- val-type))
      (else
       (let1 decoder (or (hash-table-get decoder-table val-type #f)
                         (errorf "Unsupported value type: ~a" val-type))
         (decoder ctx in))))))

;;;

(define *jsargtype-serializer-table* (make-hash-table 'eq?))
(define *jsargtype-datastream-method-table* (make-hash-table 'eq?))

(define-syntax define-jsargtype
  (syntax-rules ()
    ((_ (type-name rest ...) datastream-method serializer)
     (begin
       (define-jsargtype type-name datastream-method serializer)
       (define-jsargtype (rest ...) datastream-method serializer-body)))
    ((_ type-name datastream-method serializer)
     (begin
       (hash-table-put! *jsargtype-serializer-table* 'type-name serializer)
       (hash-table-put! *jsargtype-datastream-method-table* 'type-name 'datastream-method)))))

(define-jsargtype string getString encode-string)
(define-jsargtype symbol getSymbol encode-symbol)
(define-jsargtype object getObject encode-object)
(define-jsargtype time getDate encode-time)
(define-jsargtype array getArray encode-array)
(define-jsargtype s8vector getInt8Array encode-uvector)
(define-jsargtype u8vector getUint8Array encode-uvector)
(define-jsargtype s16vector getInt16Array encode-uvector)
(define-jsargtype u16vector getUint16Array encode-uvector)
(define-jsargtype s32vector getInt32Array encode-uvector)
(define-jsargtype u32vector getUint32Array encode-uvector)
(define-jsargtype f32vector getFloat32Array encode-uvector)
(define-jsargtype f64vector getFloat64Array encode-uvector)
(define-jsargtype json getJson encode-json)
(define-jsargtype any getAny encode-value)
(define-jsargtype list getArray (lambda (v out)
                                  (encode-integer (length v) out)
                                  (dolist (e v)
                                    (encode-value e out))))

(define (write-command-args types args out)
  ;; Use non-generic-function for-each here for performance.
  ((with-module gauche for-each) (lambda (type arg)
                                   (or (and-let1 serialize (hash-table-get *jsargtype-serializer-table* type #f)
                                         (serialize arg out)
                                         #t)
                                       (and (hash-table-contains? *enum-table* type)
                                            (write-u8 (enum-value type arg) out 'little-endian))
                                       (errorf "Invalid jsarg type: ~a" type)))
   types args))

(define (call-command command-id types args)
  (let1 out (client-request-output)
    (write-u16 command-id out 'little-endian)
    (write-command-args types args out)))

;;;

(define (parse-arg-spec spec)
  (match spec
    ((? symbol? spec)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")))))
    (((? symbol? spec))
     (let1 parts (map string->symbol (string-split (symbol->string spec) "::"))
       (cond
         ((= (length parts) 1)
          (parse-arg-spec `((,@parts any))))
         (else
          (parse-arg-spec `(,parts))))))
    (((? symbol? spec) init-val)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")) init-val)))
    (((var type) init-val)
     (list var type init-val))
    (((var type))
     (list var type var))
    (((var) init-val)
     (list var 'any init-val))))

(define binary-command-next-id (make-id-generator #xffffffff))

(define *enum-table* (make-hash-table))

(define-class <jsprocedure> ()
  ((types :init-keyword :types)
   (command-id :init-keyword :command-id)))

(define (compile-jslet arg-specs body)
  (let* ((jsmodule-name (js-vm-current-main-module))
         (var-type-list (map parse-arg-spec arg-specs))
         (command-id (binary-command-next-id))
         (name (gensym))
         (ds (gensym)))
    (register-jsstmt! jsmodule-name
                       `(begin
                          (define (,name ,ds)
                            (let ,(map (match-lambda
                                         ((var type _)
                                          `(,var ,(or (and-let1 method (hash-table-get *jsargtype-datastream-method-table* type #f)
                                                        `((~ ,ds ',method)))
                                                      (and (hash-table-contains? *enum-table* type)
                                                           `((~ ,ds 'getEnum) ,(symbol->string type)))
                                                      (errorf "Invalid jsarg type: ~a" type)))))
                                       var-type-list)
                              ,@body))
                          (Graviton.registerBinaryCommand ,command-id ,name)))
    (make <jsprocedure> :command-id command-id :types (map (cut list-ref <> 1) var-type-list))))

(define (compile-jslet/async arg-specs body)
  (compile-jslet (cons '(%notification-id) arg-specs) body))

(define-jsmacro raise
  ((err)
   `(Graviton.notifyException err)))

(define (jscall js-proc :rest args)
  (call-command (slot-ref js-proc 'command-id)
                (slot-ref js-proc 'types)
                args))

(define (jscall/async js-proc :rest args)
  (let* ((gpromise (make-grv-promise))
         (notification-id (allocate-notification-id gpromise)))
    (call-command (slot-ref js-proc 'command-id)
                  (slot-ref js-proc 'types)
                  (cons notification-id args))
    (when (disable-async-wait)
      (flush-client-request))
    gpromise))

(define (jscall/await js-proc :rest args)
  (await (apply jscall/async js-proc args)))

(define-macro (jslet arg-specs :rest body)
  `(,jscall ,(compile-jslet arg-specs body)
            ,@(map (lambda (spec)
                     (list-ref (parse-arg-spec spec) 2))
                   arg-specs)))

(define-macro (jslet/async arg-specs :rest body)
  `(,jscall/async ,(compile-jslet/async arg-specs body)
                  ,@(map (lambda (spec)
                           (list-ref (parse-arg-spec spec) 2))
                         arg-specs)))

(define-syntax jslet/await
  (syntax-rules ()
    ((_ arg-specs body ...)
     (await (jslet/async arg-specs body ...)))))

(define-class <jsenum> ()
  ((symbol->value-table :init-form (make-hash-table))
   (jsvalue->symbol-table :init-form (make-hash-table 'equal?))))

(define (make-enum jsmodule-name enum-name symbol->jsvalue-list)
  (let ((enum (make <jsenum>))
        (jsvals '()))
    (for-each-with-index (lambda (i symbol->jsvalue)
                           (match symbol->jsvalue
                             ((sym jsval)
                              (hash-table-put! (slot-ref enum 'symbol->value-table) sym i)
                              (hash-table-put! (slot-ref enum 'jsvalue->symbol-table) jsval sym)
                              (push! jsvals jsval))))
                         symbol->jsvalue-list)
    (register-jsstmt! jsmodule-name
                       `(Graviton.registerEnum ,(symbol->string enum-name) ,(list->vector (reverse jsvals))))
    enum))

(define-syntax define-jsenum
  (syntax-rules ()
    ((_ name symbol->jsvalue ...)
     (let1 enum (make-enum (js-current-main-module) 'name '(symbol->jsvalue ...))
       (hash-table-put! *enum-table* 'name enum)))))

(define (enum-value name sym)
  (let1 enum (hash-table-get *enum-table* name #f)
    (unless enum
      (errorf "enum ~a not found" enum))
    (or (hash-table-get (slot-ref enum 'symbol->value-table) sym #f)
        (errorf "enum symbol ~a not found" sym))))

(define (enum-symbol name jsval)
  (let1 enum (hash-table-get *enum-table* name #f)
    (unless enum
      (errorf "enum ~a not found" enum))
    (or (hash-table-get (slot-ref enum 'jsvalue->symbol-table) jsval #f)
        (errorf "enum jsvalue ~a not found" jsval))))

;;;

(define class-next-id (make-id-generator #xffffffff))

(define *id->class-table* (make-hash-table 'eqv?))
(define *id->jsclass-table* (make-hash-table 'eqv?))

(define (register-jsclass! jsclass klass)
  (let1 class-id (class-next-id)
    (hash-table-put! *id->class-table* class-id klass)
    (hash-table-put! *id->jsclass-table* class-id jsclass)
    (register-jsstmt! (js-vm-current-main-module) `(Graviton.registerClass ,class-id ,jsclass))))

(define-class <jsobject-meta> (<class>)
  ((method-table :init-form (make-hash-table 'eq?))
   (method-prefix)
   (jsclass)))

(define-method initialize ((jsobject-class <jsobject-meta>) initargs)
  (next-method)
  (let-keywords initargs ((jsclass #f)
                          (jsobject-method-prefix (let1 str (symbol->string (class-name jsobject-class))
                                                    (rxmatch-case str
                                                      (#/^<(.*)>$/ (#f stem)
                                                       stem)
                                                      (else
                                                       str))))
                          . #f)
    (unless (string? jsclass)
      (errorf "<string> required for :jsclass, but got ~s" jsclass))
    (set! (~ jsobject-class'method-prefix) jsobject-method-prefix)
    (register-jsclass! jsclass jsobject-class)
    (set! (~ jsobject-class'jsclass) jsclass)))

(define-method compute-get-n-set ((jsobject-class <jsobject-meta>) slot-definition)
  (let* ((name (slot-definition-name slot-definition))
         (jsproperty (slot-definition-option slot-definition :jsproperty #f))
         (read-only? (slot-definition-option slot-definition :read-only? #f))
         (cacheable? (slot-definition-option slot-definition :cacheable? #f)))
    (cond
      (jsproperty
       (unless (string? jsproperty)
         (errorf "<string> required for :jsproperty, but got ~s" jsproperty))
       (list
         ;; slot-ref
         (let1 jsgetter (compile-jslet/async '(obj::object) `((respond (~ obj ,jsproperty))))
           (if (and read-only? cacheable?)
             ;; cacheable case
             (lambda (jsobj)
               (let1 tbl (slot-ref jsobj '%jsproperty-cache)
                 (if (hash-table-contains? tbl jsproperty)
                   (hash-table-get tbl jsproperty)
                   (rlet1 v (jscall/await jsgetter jsobj)
                     (hash-table-put! tbl jsproperty v)))))
             ;; non-cacheable case
             (lambda (jsobj)
               (jscall/await jsgetter jsobj))))
         ;; slot-set!
         (if read-only?
           #f
           (let1 jssetter (compile-jslet '(obj::object val::any) `((set! (~ obj ,jsproperty) val)))
             (lambda (jsobj val)
               (jscall jssetter jsobj val))))
         #f
         #f))
      (else
       (next-method)))))

(define (register-jsobject-method! jsobject-class name gf)
  (hash-table-put! (~ jsobject-class'method-table) name gf))

(define (make-jsobject-method-name jsobject-class msg)
  (string->symbol (format "~a-~a" (~ jsobject-class'method-prefix) msg)))

(define-syntax define-jsobject-method
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ jsobject-class method args body ...)
         (let1 name (make-jsobject-method-name (global-variable-ref
                                                 ((with-module gauche.internal vm-current-module))
                                                 jsobject-class)
                                               method)
           (quasirename rename
             `(begin
                (define-method ,name
                  ,(append `((,'self ,jsobject-class)) args)
                  ,@body)
                (register-jsobject-method! ,jsobject-class ',method ,name)))))
        (_
         (errof "malformed define-jsmethod: ~s" form))))))

(define (convert-jsname jsname)
  (if (= (string-length jsname) 0)
    ""
    (let1 chars (string->list jsname)
      (let loop ((chars (cdr chars))
                 (holder `(,(car chars)))
                 (results '()))
        (cond
          ((null? chars)
           (string-join
             (map string-downcase (reverse (remove
                                             (^s (= (string-length s) 0))
                                             (cons (apply string (reverse holder))
                                                   results))))
             "-"))
          ((and (char-upper-case? (car holder))
                (char-upper-case? (car chars))
                (not (null? (cdr chars)))
                (char-lower-case? (cadr chars)))
           (loop (cdr chars) `(,(car chars)) (cons (apply string (reverse holder)) results)))
          ((and (char-lower-case? (car holder)) (char-upper-case? (car chars)))
           (loop (cdr chars) `(,(car chars)) (cons (apply string (reverse holder)) results)))
          (else
           (loop (cdr chars) (cons (car chars) holder) results)))))))

(define-syntax define-automatic-jsobject-methods
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ jsobject-class specs ...)
         (quasirename rename
           `(begin
              ,@(map (lambda (spec)
                       (match spec
                         ((? string? jsmethod)
                          `(define-jsobject-method ,jsobject-class ,(string->symbol (convert-jsname jsmethod)) (:rest args)
                             (jslet ((self::object)
                                     (args::list))
                               ((~ (~ self ,jsmethod) 'apply) self args))
                             (undefined)))
                         (((? string? jsmethod) ':result . (or (#t) ()))
                          `(define-jsobject-method ,jsobject-class ,(string->symbol (convert-jsname jsmethod)) (:rest args)
                             (jslet/await ((self::object)
                                           (args::list))
                               (respond ((~ (~ self ,jsmethod) 'apply) self args)))))
                         (((? string? jsmethod) ':result json-value-class)
                          `(define-jsobject-method ,jsobject-class ,(string->symbol (convert-jsname jsmethod)) (:rest args)
                             (json->obj ,json-value-class
                                        (jslet/await ((self::object)
                                                      (args::list)
                                                      (rule::json (json-rule ,json-value-class)))
                                          (respond (json-value ((~ (~ self ,jsmethod) 'apply) self args) rule))))))))
                     specs))))))))

(define *initial-jsobject-table-size* 128)

(define-class <jsobject-manager> ()
  ((id-vector :init-form (make-vector *initial-jsobject-table-size* #f))
   (object-vector :init-form (make-weak-vector *initial-jsobject-table-size*))
   (id->index-table :init-form (make-hash-table 'eqv?))
   (next-index :init-value 0)))

(define-window-context-slot jsobject-manager (make <jsobject-manager>))

(define (find-jsobject manager id)
  (with-slots (object-vector id->index-table) manager
    (and-let1 index (hash-table-get id->index-table id #f)
      (rlet1 jsobj (weak-vector-ref object-vector index)
        (log-framework-debug "Found jsobject: ~s" jsobj)))))

(define (register-jsobject! manager jsobj)
  (with-slots (id-vector object-vector id->index-table next-index) manager
    (let1 len (vector-length id-vector)
      (let loop ((i 0))
        (let1 index (modulo (+ next-index i) len)
          (cond
            ((<= len i)
             (expand-jsobject-table! manager)
             (register-jsobject! manager jsobj))
            ((weak-vector-ref object-vector index)
             (loop (+ i 1)))
            (else
             (and-let1 old-id (vector-ref id-vector index)
               (jslet ((old-id))
                 (Graviton.freeObjectId old-id))
               (hash-table-delete! id->index-table old-id)
               (log-framework-debug "Freed jsobject: #x~8,'0x (by GC)" old-id))
             (vector-set! id-vector index (jsobject-id jsobj))
             (weak-vector-set! object-vector index jsobj)
             (hash-table-put! id->index-table (jsobject-id jsobj) index)
             (set! next-index (modulo (+ index 1) len))
             (log-framework-debug "Allocated jsobject: #x~8,'0x (~s)" (jsobject-id jsobj) jsobj))))))))

(define (expand-jsobject-table! manager)
  (with-slots (id-vector object-vector next-index) manager
    (let* ((len (vector-length id-vector))
           (new-len (* len 2)))
      (when (< #xffffffff len)
        (errorf "Too many jsobjects"))
      (let ((new-id-vector (make-vector new-len #f))
            (new-object-vector (make-weak-vector new-len)))
        (vector-copy! new-id-vector 0 id-vector)
        (dotimes (i len)
          (weak-vector-set! new-object-vector i (weak-vector-ref object-vector i)))
        (set! id-vector new-id-vector)
        (set! object-vector new-object-vector)
        (set! next-index len))
      (log-framework-debug "Expanded jsobject table size: ~a -> ~a" len new-len))))

(define-class <jsobject> ()
  ((%id :init-keyword :id)
   (%jsproperty-cache :init-form (make-hash-table 'string=?))
   (%window-context :init-keyword :window-context))
  :metaclass <jsobject-meta>
  :jsclass "Object")

(define (find-jsobject-method jsobject-class msg)
  (any (lambda (klass)
          (cond
            ((is-a? klass <jsobject-meta>)
             (hash-table-get (~ klass'method-table) msg #f))
            (else
             #f)))
        (class-precedence-list jsobject-class)))

(define-method object-apply ((jsobj <jsobject>) (msg <symbol>) :rest args)
  (let1 gf (find-jsobject-method (class-of jsobj) msg)
    (unless gf
      (errorf "Method ~a not found in ~s" msg jsobj))
    (apply gf jsobj args)))

(define-method object-apply ((jsobj <jsobject>) (method <string>) :rest args)
  (match (reverse args)
    ((spec ':result rargs ...)
     (call-jsmethod jsobj method spec (reverse rargs)))
    ((rargs ...)
     (call-jsmethod jsobj method #t (reverse rargs)))))

(define-method slot-missing ((klass <class>) (jsobject-class <jsobject-meta>) slot :optional value)
  (or (and (undefined? value)
           (find-jsobject-method jsobject-class slot))
      (next-method)))

(define *special-stem-table*
  (alist->hash-table
    '(("2d". "2D")
      ("byob" . "BYOB")
      ("cdata" . "CDATA")
      ("css". "CSS")
      ("db" . "DB")
      ("dom" . "DOM")
      ("gatt" . "GATT")
      ("html" . "HTML")
      ("idb" . "IDB")
      ("iframe" . "IFrame")
      ("midi" . "MIDI")
      ("rtc" . "RTC")
      ("url" . "URL")
      ("vtt" . "VTT")
      ("webgl" . "WebGL")
      ("xml" . "XML")
      ("xr" . "XR"))
    'equal?))

(define (estimate-jsproperty slot)
  (match-let1 (head . rest) (string-split (symbol->string slot) "-")
    (string-join (cons head (map (lambda (stem)
                                   (or (hash-table-get *special-stem-table* stem #f)
                                       (string-titlecase stem)))
                                 rest))
                 "")))

(define-method slot-missing ((jsobject-class <jsobject-meta>) jsobj slot :optional value)
  (or (and-let* (((null? value))
                 (gf (find-jsobject-method (class-of jsobj) slot)))
        (cut gf jsobj <...>))
      (let1 prop (estimate-jsproperty slot)
        (if (undefined? value)
          (jsobject-property-ref jsobj prop)
          (jsobject-property-set! jsobj prop value)))))

(define-method jsobject-id ((jsobj <jsobject>))
  (or (slot-ref jsobj '%id)
      (error "Invalid access to freed object")))

(define-method jsobject-window-context ((jsobj <jsobject>))
  (slot-ref jsobj '%window-context))

(define-method valid-window-context? ((jsobj <jsobject>))
  (eq? (window-context) (slot-ref jsobj '%window-context)))

(define (jsobject-free! obj)
  (let ((id (slot-ref obj '%id))
        (ctx (slot-ref obj '%window-context)))
    (window-context-slot-atomic-ref ctx 'jsobject-manager
      (lambda (manager)
        (with-slots (id-vector object-vector id->index-table next-index) manager
          (jslet ((id))
            (Graviton.freeObjectId id))
          (and-let1 index (hash-table-get id->index-table id #f)
            (vector-set! id-vector index #f)
            (weak-vector-set! object-vector index #f)
            (hash-table-delete! id->index-table id)
            (set! next-index index))
          (set! (~ obj'%id) #f))
        (log-framework-debug "Freed jsobject: #x~8,'0x (by manually) (~s)" id obj)))))

(define-syntax with-jsobjects
  (syntax-rules ()
    ((_ (jsobjects ...) body ...)
     (unwind-protect
         (begin
           body ...)
       (for-each (lambda (obj)
                   (jsobject-free! obj))
                 (sort (list jsobjects ...) (^(o1 o2) (> (~ o1'%id) (~ o2'%id)))))))))

(define-method write-object ((obj <jsobject>) port)
  (format port "#<~a id:#x~8,'0x>" (class-name (class-of obj)) (~ obj'%id)))

(define-method jsobject-property-ref ((jsobj <jsobject>) property)
  (jslet/await ((obj::object jsobj)
                (property::string property))
    (respond (~ obj property))))

(define-method jsobject-property-set! ((jsobj <jsobject>) property val)
  (jslet ((obj::object jsobj)
          (property::string property)
          (val::any val))
    (set! (~ obj property) val)))

(define-method extract-jsobject-properties ((jsobj <jsobject>) (properties <list>))
  (vector->list (jslet/await ((obj::object jsobj)
                              (properties::list))
                  (respond (Graviton.extractJSObjectProperties obj properties)))))

(define-syntax let-jsproperties
  (er-macro-transformer
    (lambda (form rename id=?)
      (match form
        ((_ expr (prop-specs ...) body ...)
         (let loop ((prop-specs prop-specs)
                    (vars '())
                    (jsprops '()))
           (match prop-specs
             (()
              (quasirename rename `(let* ((jsobj ,expr)
                                          (vals (extract-jsobject-properties jsobj ',jsprops)))
                                     (jsobject-free! jsobj)
                                     (receive ,vars (apply values vals)
                                       ,@body))))
             (((? symbol? var) rest ...)
              (loop rest (cons var vars) (cons (estimate-jsproperty var) jsprops)))
             ((((? symbol? var) (? string? jsprop)) rest ...)
              (loop rest (cons var vars) (cons jsprop jsprops)))
             ((((? symbol? var) (? symbol? prop)) rest ...)
              (loop rest (cons var vars) (cons (estimate-jsproperty prop) jsprops)))
             (_
              (errorf "malformed property spec: ~s" prop-specs)))))
        (_
         (errorf "malformed with-jsproperties: ~s" form))))))

(define-method ref ((obj <jsobject>) (property <string>))
  (jsobject-property-ref obj property))

(define-method ref ((obj <jsobject>) (slot <symbol>))
  (slot-ref obj slot))

(define-method (setter ref) ((obj <jsobject>) (property <string>) value)
  (jsobject-property-set! obj property value))

(define-method (setter ref) ((obj <jsobject>) (slot <symbol>) value)
  (slot-set! obj slot value))

(define (call-jsmethod jsobj method result-spec args)
  (define (call-jsmethod/void args)
    (jslet ((obj::object jsobj)
            (method::string)
            (args::list))
      ((~ (~ obj method) 'apply) obj args))
    (undefined))
  (define (call-jsmethod/result args)
    (jslet/await ((obj::object jsobj)
                  (method::string)
                  (args::list))
      (respond ((~ (~ obj method) 'apply) obj args))))
  (define (call-jsmethod/json-value-result json-value-class args)
    (json->obj json-value-class
               (jslet/await ((obj::object jsobj)
                             (method::string)
                             (args::list)
                             (rule::json (json-rule json-value-class)))
                 (respond (json-value ((~ (~ obj method) 'apply) obj args) rule)))))
  (cond
    ((eq? result-spec #f)
     (call-jsmethod/void args))
    ((eq? result-spec #t)
     (call-jsmethod/result args))
    ((is-a? result-spec <json-value-meta>)
     (call-jsmethod/json-value-result result-spec args))
    (else
     (errorf "Invalid result-spec: ~s" result-spec))))


(define-class <jsobject-provider> ()
  ((%provider :init-keyword :provider)))

(define (provide-jsobject jsobj-provider)
  ((slot-ref jsobj-provider '%provider)))

(define-method object-apply ((jsobj-provider <jsobject-provider>) (msg <symbol>) :rest args)
  (let1 jsobj (provide-jsobject jsobj-provider)
    (apply jsobj msg args)))

(define-method object-apply ((jsobj-provider <jsobject-provider>))
  (provide-jsobject jsobj-provider))

(define-method jsobject-id ((jsobj-provider <jsobject-provider>))
  (jsobject-id (provide-jsobject jsobj-provider)))

(define-method jsobject-window-context ((jsobj-provider <jsobject-provider>))
  (jsobject-window-context (provide-jsobject jsobj-provider)))

(define-method valid-window-context? ((jsobj-provider <jsobject-provider>))
  (valid-window-context? (provide-jsobject jsobj-provider)))

(define-method jsobject-property-ref ((jsobj-provider <jsobject-provider>) property)
  (jsobject-property-ref (provide-jsobject jsobj-provider) property))

(define-method jsobject-property-set! ((jsobj-provider <jsobject-provider>) property val)
  (jsobject-property-set! (provide-jsobject jsobj-provider) val))

(define-method extract-jsobject-properties ((jsobj-provider <jsobject-provider>) (properties <list>))
  (extract-jsobject-properties (provide-jsobject jsobj-provider) properties))

(define-method ref ((jsobj-provider <jsobject-provider>) (property <string>))
  (jsobject-property-ref jsobj-provider property))

(define-method (setter ref) ((jsobj-provider <jsobject-provider>) (property <string>) value)
  (jsobject-property-set! jsobj-provider property value))

(define-method ref ((jsobj-provider <jsobject-provider>) (slot <symbol>))
  (slot-ref (provide-jsobject jsobj-provider) slot))

(define-method (setter ref) ((jsobj-provider <jsobject-provider>) (slot <symbol>) value)
  (slot-set! (provide-jsobject jsobj-provider) slot value))

(define-method slot-missing ((klass <class>) (jsobj-provider <jsobject-provider>) slot :optional value)
  (let1 jsobj (provide-jsobject jsobj-provider)
    (if (undefined? value)
      (slot-ref jsobj slot)
      (slot-set! jsobj slot value))))

(define-window-context-slot global-jsobject-table (make-hash-table 'eq?))

(define (make-global-jsobject-provider provider :optional (name #f))
  (make <jsobject-provider>
    :provider  (let1 key (gensym)
                 (lambda ()
                   (window-context-slot-atomic-ref (window-context) 'global-jsobject-table
                     (lambda (tbl)
                       (or (hash-table-get tbl key #f)
                           (rlet1 jsobj (parameterize ((disable-async-wait #t))
                                          (log-framework-debug "Try to resolve jsobject ~s, provider: ~s" name provider)
                                          (with-client-request provider))
                             (hash-table-put! tbl key jsobj)))))))))

(define-syntax define-global-jsobject
  (syntax-rules ()
    ((_ name body ...)
     (define name (make-global-jsobject-provider (lambda () body ...) (symbol->string 'name))))))

;;;

(define-class <json-value-meta> (<class>)
  ((rule :init-value #f)
   (obj->json :init-value #f)
   (json->obj :init-value #f)))

(define (make-rule slot-defs)
  (fold (lambda (rule slot-def)
          (let-keywords (car slot-defs)
              ((jsproperty #f)
               (json-value-class #f)
               . #f)
            (cond
              (jsproperty
               #f)
              (else
               rule))))
        '()
        (class-slots json-value-class)))

(define (obj->json json-value)
  (let1 proc (~ json-value'obj->json)
    (proc json-value)))

(define (json->obj json-value-class json)
  (let1 proc (~ json-value-class'json->obj)
    (proc json)))

(define (json-rule json-value-class)
  (~ json-value-class'rule))

(define (make-obj->json slot-defs)
  (lambda (json-value)
    (fold (lambda (json slot-def)
            (let ((slot-name (slot-definition-name slot-def))
                  (jsproperty (slot-definition-option slot-def :jsproperty #f))
                  (json-value-class (slot-definition-option slot-def :json-value-class #f)))
              (cond
                ((and jsproperty json-value-class)
                 (let1 proc (~ json-value-class'obj->json)
                   (assoc-set! json jsproperty (proc (~ json-value slot-name)))))
                (jsproperty
                 (assoc-set! json jsproperty (~ json-value slot-name)))
                (else
                 json))))
          '()
          slot-defs)))

(define (make-json->obj json-value-class slot-defs)
  (lambda (json)
    (let1 obj (make json-value-class)
      (for-each (lambda (slot-def)
                  (let ((slot-name (slot-definition-name slot-def))
                        (jsproperty (slot-definition-option slot-def :jsproperty #f))
                        (json-value-class (slot-definition-option slot-def :json-value-class #f)))
                    (cond
                      ((and jsproperty json-value-class)
                       (let1 proc (~ json-value-class'json->obj)
                         (set! (~ obj slot-name) (proc (assoc-ref json jsproperty)))))
                      (jsproperty
                       (set! (~ obj slot-name) (assoc-ref json jsproperty)))
                      (else
                       #t))))
                slot-defs)
      obj)))

(define (make-rule slot-defs)
  (fold (lambda (slot-def rule)
          (let-keywords (slot-definition-options slot-def)
              ((jsproperty #f)
               (json-value-class #f)
               . #f)
            (cond
              ((and jsproperty json-value-class)
               (assoc-set! rule jsproperty (~ json-value-class'rule)))
              (jsproperty
               (assoc-set! rule jsproperty #t))
              (else
               rule))))
        '()
        slot-defs))

(define-method initialize ((json-value-class <json-value-meta>) initargs)
  (next-method)
  (let1 slot-defs (class-slots json-value-class)
    (set! (~ json-value-class'rule) (make-rule slot-defs))
    (set! (~ json-value-class'json->obj) (make-json->obj json-value-class slot-defs))
    (set! (~ json-value-class'obj->json) (make-obj->json slot-defs))))

(define-class <json-value-mixin> ()
  ()
  :metaclass <json-value-meta>)

;;;

(define (decode-value-list ctx in)
  (port-map values (cut decode-value ctx in)))

(define (decode-received-binary-data ctx data)
  (call-with-input-string (u8vector->string data)
    (lambda (in)
      (let1 notification-id (decode-value ctx in)
        (notify-values ctx notification-id (lambda ()
                                             (decode-value-list ctx in)))))))

(register-binary-handler! decode-received-binary-data)

;;;

(define *initial-notification-table-size* 32)

(define-class <notification-manager> ()
  ((notification-vector :init-form (make-vector *initial-notification-table-size* #f))
   (offset :init-value 0)))

(define-window-context-slot notification-manager (make <notification-manager>))

(define (allocate-notification-id receiver)
  (window-context-slot-atomic-ref (window-context) 'notification-manager
    (lambda (notification-manager)
      (with-slots (notification-vector offset) notification-manager
          (let loop ((counter 0))
            (let1 vec-len (vector-length notification-vector)
              (cond
                ((= counter vec-len)
                 ;; Expand notification-vector if there is no room in the vector.
                 (let ((new-vector (make-vector (* vec-len 2) #f)))
                   (vector-copy! new-vector 0 notification-vector)
                   (set! notification-vector new-vector)
                   (set! offset vec-len)
                   (log-framework-debug "Expanded notification-vector: ~a -> ~a" vec-len (* vec-len 2))
                   (loop 0)))
                ((vector-ref notification-vector (modulo (+ offset counter) vec-len))
                 (loop (+ counter 1)))
                (else
                 (rlet1 index (modulo (+ offset counter) vec-len)
                   (vector-set! notification-vector index receiver)
                   (log-framework-debug "Allocated notification ID: ~a for ~s" index receiver)
                   (set! offset (modulo (+ offset counter 1) vec-len)))))))))))

(define (free-notification-id id :optional (ctx (window-context)))
  (window-context-slot-atomic-ref ctx 'notification-manager
    (lambda (notification-manager)
      (with-slots (notification-vector offset) notification-manager
        (vector-set! notification-vector id #f)
        (set! offset id)
        (log-framework-debug "Freed notification ID: ~a" id)))))

(define (notify-values ctx notification-id arg-creator)
  (window-context-slot-atomic-ref ctx 'notification-manager
    (lambda (notification-manager)
      (with-slots (notification-vector offset) notification-manager
        (let1 receiver (vector-ref notification-vector notification-id)
          (match receiver
            ((? (cut is-a? <> <grv-promise>) gpromise)
             (log-framework-debug "notification ID: ~a received. Set values to grv-promise: ~s" notification-id gpromise)
             (grv-promise-set-thunk! gpromise arg-creator)
             ;; We can free this notification-id because grv-promise is an one-time object.
             (free-notification-id notification-id ctx))
            ((? worker-callback? callback)
             (log-framework-debug "notification ID: ~a received. Invoke callback: ~s" notification-id callback)
             (invoke-worker-callback callback arg-creator))
            (else
             (errorf "[BUG] Invalid receiver for notification ID: ~a" notification-id))))))))

(define-window-context-slot callback->id-table (make-hash-table 'equal?))

(define (link-callback callback)
  (window-context-slot-atomic-ref (window-context) 'callback->id-table
    (lambda (tbl)
      (or (hash-table-get tbl callback #f)
          (rlet1 id (allocate-notification-id callback)
            (hash-table-put! tbl callback id)
            (log-framework-debug "Linked callback: ~s (notification ID: ~a)" callback id))))))

(define (unlink-callback callback)
  (window-context-slot-atomic-ref (window-context) 'callback->id-table
    (lambda (tbl)
      (and-let1 id (hash-table-get tbl callback #f)
        (jslet ((id))
          (Graviton.freeProcedure id))
        (free-notification-id id)
        (hash-table-delete! tbl callback)
        (log-framework-debug "Unlinked callback: ~s (notification ID: ~a)" callback id)))))

(define (unlink-procedure proc)
  (for-each unlink-callback
            (window-context-slot-atomic-ref (window-context) 'callback->id-table
              (lambda (tbl)
                (rlet1 result '()
                  (hash-table-for-each tbl (lambda (callback id)
                                             (when (and (is-a? callback <procedure-callback>)
                                                        (equal? (~ callback'worker) (current-worker))
                                                        (equal? (~ callback'procedure) proc))
                                               (push! result callback)))))))))

(define-syntax shift-callback*
  (syntax-rules ()
    ((_ callback expr ...)
     (let1 %cont #f
       (begin0
           (shift-callback callback
             (set! %cont callback)
             expr ...))
       (unlink-callback %cont)))))

;;;

(define (absolute-js-path js-path)
  (cond
    ((#/^https?:/i js-path)
     js-path)
    ((absolute-path? js-path)
     js-path)
    (else
     (build-path "/" js-path))))

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
     `(js-code "import " ,(compile-import-option import-options) " from '" ,(absolute-js-path module-path) "'"))
    (_
     (errorf "Invalid import-spec: ~s" import-spec))))

(define-syntax import-js
  (syntax-rules ()
    ((_)
     (begin))
    ((_ import-spec rest ...)
     (begin
       (register-jsstmt! (js-current-main-module) (compile-import-spec 'import-spec))
       (import-js rest ...)))))

;;;

(define *load-js+type-list* '())

(define (load-js js-path :key (type #f))
  (push! *load-js+type-list* (cons (absolute-js-path js-path)
                                   (or type
                                       (if (equal? (path-extension js-path) "mjs")
                                         "module"
                                         #f)))))

(define (load-js+type-list)
  (reverse *load-js+type-list*))

;;;

(define-action "logDebugMessage" (msg)
  (log-debug "~a" msg))