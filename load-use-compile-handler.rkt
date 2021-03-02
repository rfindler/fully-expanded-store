#lang racket
(require file/sha1 "db.rkt" compiler/private/cm-dep)
(provide make-save-expanded-load/use-compiled-handler)

(define (make-save-expanded-load/use-compiled-handler)
  (define old-handler (current-load/use-compiled))
  (define old-current-compile (current-compile))
  ;; do this to ensure that all dynamic loading in the db library is flushed out
  (in-db (lookup (string->path "/dne.rkt")))
  (define (save-expanded-load/use-compiled-handler path mod-name)
    (parameterize ([current-compile
                    (λ (stx immediate?)
                      (define filename (syntax-source stx))
                      (cond
                        [(path? filename)
                         (printf "~a: expanding\n" filename)
                         (define expanded (expand-syntax stx))
                         (printf "~a: marshalling\n" filename)
                         (define bp (open-output-bytes))
                         (write (old-current-compile `(,#'quote-syntax ,expanded) #f) bp)
                         (define compiled-bytes (get-output-bytes bp))
                         (printf "~a: getting sha1\n" filename)
                         (define sha1 (sha1-of-compiled stx old-current-compile))
                         (printf "~a: recurring\n" filename)
                         (define compiled (old-current-compile expanded immediate?))
                         (printf "~a: getting dependencies\n" filename)
                         (define deps (get-deps compiled filename))
                         (printf "~a: storing in db\n" filename)
                         (in-db
                          (set-mapping! filename compiled-bytes sha1)
                          (update-dependencies! filename deps))
                         (printf "~a: done\n" filename)
                         compiled]
                        [else
                         (old-current-compile stx immediate?)]))])
      (old-handler path mod-name)))
  save-expanded-load/use-compiled-handler)

;; sha1-of-compiled : syntax? -> bytes?
;; computes the sha1 of `stx`
(define (sha1-of-compiled stx old-current-compile)
  (define-values (in out) (make-pipe))
  (thread
   (lambda ()
     (define compiled (old-current-compile #`#'#,stx #f))
     (write compiled out)
     (close-output-port out)))
  (sha1-bytes in))
