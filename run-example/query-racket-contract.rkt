#lang racket

(require "../db.rkt")

(define racket/contract-bytes
  (in-db
   (lookup (collection-file-path "contract.rkt" "racket"))))

(parameterize ([read-accept-compiled #t])
  (eval (read (open-input-bytes racket/contract-bytes))))

