#lang racket

(require file/sha1)
(require "db.rkt")

(define (make-save-expanded-load/use-compiled-handler)
  (define old-handler (current-load/use-compiled))
  (define old-current-compile (current-compile))
  (define (save-expanded-load/use-compiled-handler path mod-name)
    (parameterize ([current-compile
                    (λ (stx immediate?)
                      (define compiled (compile #`#'#,stx))
                      (define-values (in out) (make-pipe))
                      (thread
                       (lambda ()
                         (write compiled out)
                         (close-output-port out)))
                      (define expanded (expand stx))
                      (define sha (sha1-bytes in))
                      (old-current-compile expanded immediate?))])
    (old-handler path mod-name)))
  save-expanded-load/use-compiled-handler)
    
  