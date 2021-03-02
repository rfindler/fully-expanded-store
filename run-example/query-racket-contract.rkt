#lang racket

(require "../db.rkt")

(define-values (racket/contract-bytes racket/contract-deps)
  (in-db
   (values
    (lookup (collection-file-path "contract.rkt" "racket"))
    (lookup-deps (collection-file-path "contract.rkt" "racket")))))

(parameterize ([read-accept-compiled #t])
  (eval (read (open-input-bytes racket/contract-bytes))))

racket/contract-deps
