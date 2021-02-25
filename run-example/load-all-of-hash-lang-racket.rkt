#lang racket
#|

This file loads empty-hash-lang-racket.rkt
and all of its dependencies into the database

|#


(require "../load-use-compile-handler.rkt")
(require racket/runtime-path)
(define-runtime-path empty-hash-lang-racket.rkt "empty-hash-lang-racket.rkt")
(define ns (make-base-empty-namespace))
(namespace-attach-module (current-namespace) 'racket/base ns)
(namespace-require 'racket/base ns) ;; make quote-syntax available
(parameterize ([current-namespace ns])
  (parameterize ([current-compiled-file-roots '()]
                 [current-load/use-compiled
                  (make-save-expanded-load/use-compiled-handler)])
    (dynamic-require empty-hash-lang-racket.rkt #f)))
