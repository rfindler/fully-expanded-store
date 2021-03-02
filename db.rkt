#lang racket
(require db racket/runtime-path)
(provide
 (contract-out
  [set-mapping! (-> path-string? bytes? bytes? void?)]
  [lookup (-> path-string? (or/c bytes? #f))]
  [update-dependencies! (-> path-string? (listof bytes?) void?)]
  [lookup-deps (-> path-string? (listof bytes?))])
 in-db)

(define-runtime-path db.sqlite "db.sqlite")

(define-syntax-rule
  (in-db stuff ...)
  (in-db/proc (λ () stuff ...)))

(define current-conn (make-parameter #f))

(define (in-db/proc thunk)
  (define conn #f)
  (dynamic-wind
   (λ () (set! conn (sqlite3-connect #:database db.sqlite)))
   (λ () (parameterize ([current-conn conn])
           ;; start transaction(?)
           (thunk)
           ;; end transaction(?)
           ))
   (λ () (disconnect conn) (set! conn #f))))

(define (initialize-db)
  (unless (file-exists? db.sqlite)
    (define conn
      (sqlite3-connect #:database db.sqlite #:mode 'create))
    (query-exec
     conn
     "create table Files (file blob primary key, expanded blob, sha blob);")
    (query-exec
     conn
     "create table Dependencies (requires blob, required blob);")
    (disconnect conn)))

(define (set-mapping! filename bytes sha)
  (define binding
    (query
     (get-conn)
     "select expanded from Files where file = $1;"
     (path->bytes filename)))
  (cond
    [(null? (rows-result-rows binding))
     (query-exec
      (get-conn)
      "insert into Files(file, expanded, sha) values ($1,$2,$3);"
      (path->bytes filename)
      bytes
      sha)]
    [else
     (query-exec
      (get-conn)
      "update Files set expanded = $2, sha = $3 where file = $1;"
      (path->bytes filename)
      bytes
      sha)])
  )

(define (update-dependencies! path deps)
  (define path-bytes (path->bytes path))
  (query-exec
   (get-conn)
   "delete from Dependencies where requires = $1;"
   path-bytes)
  (for ([dep (in-list deps)])
    (query-exec
     (get-conn)
     "insert into Dependencies(requires, required) values ($1,$2);"
     path-bytes
     dep)))

(define (lookup filename)
  (define the-rows
    (rows-result-rows
     (query
      (get-conn)
      "select expanded from Files where file = $1;"
      (path->bytes filename))))
  (cond
    [(null? the-rows) #f]
    [else
     (define the-row (list-ref the-rows 0))
     (vector-ref the-row 0)]))

(define (lookup-deps filename)
  (define the-rows
    (rows-result-rows
     (query
      (get-conn)
      "select required from Dependencies where requires = $1;"
      (path->bytes filename))))
  (for/list ([a-row (in-list the-rows)])
    (vector-ref a-row 0)))

(define (get-conn)
  (define conn (current-conn))
  (unless conn (error 'db.rkt "not in the dynamic-extent of in-db"))
  conn)

(module+ main
  (initialize-db)  
  (define tmp1.rkt (build-path "/Users/robby/tmp1.rkt"))
  (define tmp2.rkt (build-path "/Users/robby/tmp2.rkt"))
  (in-db
   (set-mapping! tmp1.rkt #"abcdef" #"sha-abcdef")
   (printf "~s\n" (lookup tmp2.rkt))
   (printf "~s\n" (lookup tmp1.rkt))
   (set-mapping! tmp1.rkt #"abcdefghi" #"sha-abcdefghi")
   (printf "~s\n" (lookup tmp1.rkt))))
