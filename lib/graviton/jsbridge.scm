;;;
;;; jsbridge.scm - Javascript in S-Expression
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

(define-module graviton.jsbridge
  (use binary.io)
  (use data.queue)
  (use file.util)
  (use gauche.charconv)
  (use gauche.collection)
  (use gauche.hook)
  (use gauche.parameter)
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
          load-js-list

          import-js
          load-js

          <jsobject>
          jsobject-id
          invalidate!
          invalidate?
          define-jsenum
          jslet
          jslet/result

          with-jstransaction))

(select-module graviton.jsbridge)

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

(define-jsstmt result env
  ((val)
   (cond
     ((resolve-js-local-var env '%future-id)
      (compile-jsise-stmt env `(begin
                                 (Graviton.notifyValues %future-id (vector ,val))
                                 (return))))
     (else
      (error "result is called outside jslet/result.")))))

(define-jsstmt return env
  (()
   (list "return;"))
  ((val)
   (list "return " (compile-jsise-expr env val) ";")))

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

    (('vector vals ...)
     (list "[" (intersperse "," (map (cut compile-jsise-expr env <>) vals)) "]"))
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
    (((or 'ref '~) expr attrs ...)
     (list (compile-jsise-expr env expr) (map (lambda (attr)
                                                (match attr
                                                  (('quote sym)
                                                   (list "." (symbol->string sym)))
                                                  (_
                                                   (list "[" (compile-jsise-expr env attr) "]"))))
                                              attrs)))
    (('begin expr ...)
     (list "(" (intersperse "," (map (cut compile-jsise-expr env <>) expr)) ")"))
    (('let form body ...)
     (let ((vars (map (cut list-ref <> 0) form))
           (inits (map (cut list-ref <> 1) form)))
       (compile-jsise-expr env `((lambda ,vars ,@body) ,@inits))))
    (('lambda args stmt ...)
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
             (map (cut compile-jsise-stmt env <>) stmt)
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
     (register-jsstmt! (js-current-main-module) '(define name val)))))

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
  (display "import * as Graviton from '/graviton/graviton.mjs';" out)
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

;;;

(define current-send-buffer (make-parameter #f))

(define (with-send-buffer proc)
  (cond
    ((current-send-buffer)
     (proc (current-send-buffer)))
    (else
     (let1 out (open-output-uvector)
       (proc out)
       (flush-commands out)))))

(define (flush-commands buf-out)
  (let1 output-data (get-output-uvector buf-out)
    (unless (= (uvector-length output-data) 0)
      (application-context-slot-atomic-ref 'websocket-output-port
        (lambda (out)
          (send-binary-frame out output-data))))))

(define (with-jstransaction thunk)
  (parameterize ((current-send-buffer (open-output-uvector)))
    (thunk)
    (flush-commands (current-send-buffer))))

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

(define-jsargtype f32 getFloat32 (lambda (v out)
                                   (write-f32 v out 'little-endian)))
(define-jsargtype f64 getFloat64 (lambda (v out)
                                   (write-f64 v out 'little-endian)))
(define-jsargtype s8 getInt8 (lambda (v out)
                               (write-s8 v out 'little-endian)))
(define-jsargtype s16 getInt16 (lambda (v out)
                                 (write-s16 v out 'little-endian)))
(define-jsargtype s32 getInt32 (lambda (v out)
                                 (write-s32 v out 'little-endian)))
(define-jsargtype u8 getUint8 (lambda (v out)
                                (write-u8 v out 'little-endian)))
(define-jsargtype u16 getUint16 (lambda (v out)
                                  (write-u16 v out 'little-endian)))
(define-jsargtype u32 getUint32 (lambda (v out)
                                  (write-u32 v out 'little-endian)))
(define-jsargtype u8vector getUint8Array (lambda (v out)
                                           (write-u32 (u8vector-length v) out 'little-endian)
                                           (write-uvector v out)))
(define-jsargtype f64vector getFloat64Array (lambda (v out)
                                              (write-u32 (f64vector-length v) out 'little-endian)
                                              (write-uvector v out 0 -1 'little-endian)))
(define-jsargtype boolean getBoolean (lambda (v out)
                                       (write-u8 (if v 1 0) out 'little-endian)))
(define-jsargtype future getUint32 (lambda (v out)
                                     (write-u32 (allocate-future-id v) out 'little-endian)))
(define-jsargtype object getObjectRefValue (lambda (v out)
                                             (write-u32 (jsobject-id v) out 'little-endian)))
(define-jsargtype object* getObjectRef (lambda (v out)
                                         (write-u32 (jsobject-id v) out 'little-endian)))
(define-jsargtype string getString (lambda (v out)
                                     (let1 data (ces-convert-to <u8vector> v 'utf-8)
                                       (write-u32 (u8vector-length data) out 'little-endian)
                                       (write-uvector data out))))
(define-jsargtype json getJson (lambda (v out)
                                 ;; Make a vector to make JSON to satisfy RFC4627.
                                 (let1 data (ces-convert-to <u8vector> (construct-json-string (vector v)) 'utf-8)
                                   (write-u32 (u8vector-length data) out 'little-endian)
                                   (write-uvector data out))))

(define (write-command-args types args out)
  (for-each (lambda (type arg)
              (or (and-let1 serialize (hash-table-get *jsargtype-serializer-table* type #f)
                    (serialize arg out)
                    #t)
                  (and (hash-table-contains? *enum-table* type)
                       (write-u8 (enum-value type arg) out 'little-endian))
                  (errorf "Invalid jsarg type: ~a" type)))
            types args))


(define (call-command command-id types args)
  (with-send-buffer
    (lambda (out)
      (write-u16 command-id out 'little-endian)
      (write-command-args types args out))))

;;;

(define (parse-arg-spec spec)
  (match spec
    ((? symbol? spec)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")))))
    (((? symbol? spec))
     (let1 parts (map string->symbol (string-split (symbol->string spec) "::"))
       (cond
         ((= (length parts) 1)
          (parse-arg-spec (append parts '(json))))
         (else
          (parse-arg-spec (list parts))))))
    (((? symbol? spec) init-val)
     (parse-arg-spec (list (map string->symbol (string-split (symbol->string spec) "::")) init-val)))
    (((var type) init-val)
     (list var type init-val))
    (((var type))
     (list var type var))
    (((var) init-val)
     (list var 'json init-val))))

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

(define-jsmacro raise
  ((err)
   `(Graviton.notifyException err)))

(define (jscall js-proc :rest args)
  (let1 queue (make-mtqueue)
    (call-command (slot-ref js-proc 'command-id)
                  (slot-ref js-proc 'types)
                  (cons queue args))
    (delay (dequeue/wait! queue))))

(define (jsrun js-proc :rest args)
  (call-command (slot-ref js-proc 'command-id)
                (slot-ref js-proc 'types)
                args))

(define-macro (jslet var-spec :rest body)
  (let1 jsrun jsrun
    `(,jsrun ,(compile-jslet var-spec body)
             ,@(map (lambda (spec)
                      (list-ref (parse-arg-spec spec) 2))
                    var-spec))))

(define-macro (jslet/result var-spec :rest body)
  (let1 jscall jscall
    `(,jscall ,(compile-jslet (cons '%future-id::future var-spec) body)
              ,@(map (lambda (spec)
                       (list-ref (parse-arg-spec spec) 2))
                     var-spec))))

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

(define-record-type <id-range>
  make-id-range id-range?
  (start id-range-start set-id-range-start!)
  (end id-range-end))

(define (make-free-id-list id-size)
  (cons #t
        (cons (make-id-range 1 id-size)
              #f)))

(define (allocate-id! free-id-list)
  (let1 cell (cdr free-id-list)
    (cond
      (cell
       (let* ((id-range (car cell))
              (s (id-range-start id-range)))
         (if (= (+ s 1) (id-range-end id-range))
             (set-cdr! free-id-list (cdr cell))
             (set-id-range-start! id-range (+ s 1)))
         s))
      (else
       #f))))

(define (release-id! free-id-list id)
  (let1 cell (let loop ((cell free-id-list))
               (if (or (not (cdr cell))
                       (< id (id-range-start (cadr cell))))
                   cell
                   (loop (cdr cell))))
    (set-cdr! cell (cons (make-id-range id (+ id 1)) (cdr cell)))))

(define-class <jsobject> ()
  ((%free-id-list :allocation :class
                  :init-form (atom (make-free-id-list #x100000000)))
   (id :init-value #f)))

(define-method initialize ((obj <jsobject>) initargs)
  (next-method)
  (or (and-let1 id (atomic (slot-ref obj '%free-id-list)
                     (lambda (free-id-list)
                       (allocate-id! free-id-list)))
        (slot-set! obj 'id id)
        #t)
      (error "The jsobject ID space is exhausted.")))

(define-method jsobject-id ((obj <jsobject>))
  (or (slot-ref obj 'id)
      (errorf "~s was invalidated" obj)))

(define-method release-jsobject-id! ((obj <jsobject>))
  (and-let1 id (slot-ref obj 'id)
    (slot-set! obj 'id #f)
    (atomic (slot-ref obj '%free-id-list)
      (lambda (free-id-list)
        (release-id! free-id-list id)))))

(define-method invalidate! ((obj <jsobject>))
  (jslet ((obj*::object* obj))
      (obj*.invalidate))
  (release-jsobject-id! obj))

(define-method invalidate? ((obj <jsobject>))
  (not (not (slot-ref obj 'id))))

;;;

(define (absolute-js-path js-path)
  (cond
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

(define *load-js-list* '())

(define (load-js js-path)
  (push! *load-js-list* (absolute-js-path js-path)))

(define (load-js-list)
  (reverse *load-js-list*))

