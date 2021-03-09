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
                         (define-values (logger receiver) (create-accomplice-log-receiver))
                         (printf "~a: expanding\n" filename)
                         (define expanded (parameterize ([current-logger logger])
                                            (expand-syntax stx)))
                         (printf "~a: getting accomplice dependencies\n" filename)
                         (define acc-deps (get-accomplice-dependencies receiver))
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
                          (update-dependencies! filename (append acc-deps deps)))
                         (printf "~a: done\n" filename)
                         compiled]
                        [else
                         (old-current-compile stx immediate?)]))])
      (old-handler path mod-name)))
  save-expanded-load/use-compiled-handler)

(define-struct file-dependency (path module?) #:prefab)
(define-struct (file-dependency/options file-dependency) (table) #:prefab)

(define (create-accomplice-log-receiver)
  ;; Set up a logger to receive and filter accomplice events:
  (define accomplice-logger (make-logger #f (current-logger)
                                         ;; Don't propoagate 'cm-accomplice events, so that
                                         ;; enclosing compilations don't see events intended
                                         ;; for this one:
                                         'none 'cm-accomplice
                                         ;; Propagate everything else:
                                         'debug))
  (values accomplice-logger
          (make-log-receiver accomplice-logger 'info 'cm-accomplice)))

(define (get-accomplice-dependencies receiver)
  (let loop ()
    (define l (sync/timeout 0 receiver))
    (cond
      [l
       (cond
         [(and (eq? (vector-ref l 0) 'info)
               (file-dependency? (vector-ref l 2))
               (path? (file-dependency-path (vector-ref l 2))))
          (define indirect?
            (and (file-dependency/options? (vector-ref l 2))
                 (hash-ref (file-dependency/options-table (vector-ref l 2))
                           'indirect
                           #f)))
          ;; (file-dependency-module? (vector-ref l 2)) -- this tells us if
          ;; this dependenecy is on other racket code or not, but maybe
          ;; we don't care about that?
          (if indirect?
              (loop)
              (cons (path->bytes (file-dependency-path (vector-ref l 2)))
                    (loop)))]
         [else (loop)])]
      [else '()])))

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
