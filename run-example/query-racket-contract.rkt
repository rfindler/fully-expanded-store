#lang racket
(require racket/runtime-path "../db.rkt")

(define-values (racket/contract-bytes racket/contract-deps)
  (in-db
   (values
    (lookup (collection-file-path "contract.rkt" "racket"))
    (lookup-deps (collection-file-path "contract.rkt" "racket")))))

#;
(parameterize ([read-accept-compiled #t])
  (eval (read (open-input-bytes racket/contract-bytes))))

;racket/contract-deps

(define-runtime-path empty-hash-lang-racket.rkt "empty-hash-lang-racket.rkt")
(in-db (lookup-deps empty-hash-lang-racket.rkt))