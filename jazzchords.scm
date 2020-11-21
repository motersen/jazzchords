;; Lilypond Jazz chord symbol parser
;;
;; Moritz Petersen, 2020

;; Example markup tree for "F#mMaj7b13"
;;   (let ((props (prepend-alist-chain 'font-family 'sans props)))
;;     (stack-stencil-line 0
;; 	                      (list
;;                          (interpret-markup layout
;;                                            (prepend-alist-chain 'font-size 2 props)
;;                                            (make-simple-markup "F"))
;;                          (interpret-markup
;;                           layout props
;;                           (list concat-markup
;;                                 (list
;;                                          (make-musicglyph-markup "accidentals.sharp")
;;                                          (make-simple-markup "m")
;;                                          (list super-markup
;;                                                (list concat-markup
;;                                                      (list
;;                                                       (make-simple-markup "Maj7")
;;                                                       (list concat-markup
;;                                                             (list
;;                                                              (make-musicglyph-markup "accidentals.flat"))
;;                                                              (make-simple-markup"13")))))))))))))

(use-modules (ice-9 regex))

(define (collect-nonempty-markups? markups?)
  ;; (()) -> list | #f
  ;; collect successful parse results or #f if none were successful
  (let ((result (fold-right (lambda (markup? acc)
                              (if (null? (car markup?))
                                  acc
                                  (cons (car markup?) acc)))
                            '()
                            markups?)))
    (and (not (null? result))
         result)))

(define (markup?->stencil? layout props markup?)
  (and (not (null? markup?))
       (interpret-markup layout props markup?)))

(define (regexp->markup regexp markup-constructor)
  (lambda (name)
    (let ((match? (regexp-exec regexp name)))
      (if (not match?)
          (cons (list) name)
          (cons (markup-constructor (match:substring match?))
                (match:suffix match?))))))

(define root-natural->markup
  (let ((match-root-basenote (make-regexp "^[A-H]" regexp/icase)))
    (regexp->markup match-root-basenote make-simple-markup)))

(define root-basenote->markup
  (lambda (name)
    (let* ((root? (root-natural->markup name))
           (accidental? (accidental->markup (cdr root?)))
           (accidental? (if (not (null? (car accidental?)))
                            (cons (make-small-markup (car accidental?))
                                  (cdr accidental?))
                            accidental?)))
      (cons (make-concat-markup
             (collect-nonempty-markups? (list root? accidental?)))
            (cdr accidental?)))))

(define accidental->markup
  (let ((match-accidental (make-regexp "^(#|b)")))
    (regexp->markup match-accidental
                    (lambda (substring)
                      (make-normal-size-super-markup
                       (case (string-ref substring 0)
                         ((#\#) (make-musicglyph-markup "accidentals.sharp"))
                         ((#\b) (make-musicglyph-markup "accidentals.flat"))))))))

(define minor->markup
  ;; don't match into maj, but do match when preceding other modifiers
  (let ((match-minor (make-regexp "^(m|-)(a[^j]|[^a]|$)")))
    (lambda (name)
      (let ((match? (regexp-exec match-minor name)))
        (if (not match?)
            (cons (list) name)
            (cons (make-simple-markup (match:substring match? 1))
                  (string-append
                   (match:substring match? 2)
                   (match:suffix match?))))))))

(define augmented->markup
  (let ((match-augmented (make-regexp "^(\\+|aug)")))
    (regexp->markup match-augmented make-simple-markup)))

(define diminished->markup
  (let ((match-diminished (make-regexp "^0|o|dim")))
    (lambda (name)
      (let ((match? (regexp-exec match-diminished name)))
        (if (not match?)
            (cons (list) name)
            (cons (list super-markup
                        (make-larger-markup "o"))
                  (match:suffix match?)))))))

(define extension->markup
  (let ((match-modifier (make-regexp "^(M|Ma|Maj|sus|add|omit)" regexp/icase))
        (match-step (make-regexp "^[1-9]{1,2}"))
        (match-alteration (make-regexp "^(alt|altered)")))
    (define modifier->markup
      (regexp->markup match-modifier make-simple-markup))
    (define step->markup
      (regexp->markup match-step make-simple-markup))
    (define alteration->markup
      (regexp->markup match-alteration make-simple-markup))
    (define (option->markup name)
      (let* ((accidental? (accidental->markup name))
             (step? (step->markup (cdr accidental?))))
        ;; an accidental makes no sense unless a step has been found
        (cons (if (null? (car step?))
                  (list)
                  (if (null? (car accidental?))
                      (car step?)
                      (list concat-markup
                            (list
                             (make-hspace-markup 0.2)
                             (car accidental?)
                             (car step?)))))
              (cdr step?))))
    (lambda (name)
      (let* ((modifier? (modifier->markup name))
             (option? (option->markup (cdr modifier?)))
             (alteration? (alteration->markup (cdr option?)))
             (extension? (collect-nonempty-markups?
                          (list modifier? option? alteration?))))
        (cons (if (null? (car option?))
                  (list)
                  (make-concat-markup extension?))
              (cdr alteration?))))))

(define* (extensions->markup name #:key (markup-acc '()))
  (let ((extension? (extension->markup name)))
    (if (null? (car extension?))
        (cons (if (null? markup-acc)
                  (list)
                  (make-normal-size-super-markup (make-concat-markup (reverse markup-acc))))
              name)
        (extensions->markup (cdr extension?)
                            #:markup-acc (cons (car extension?) markup-acc)))))

(define* (parse-sequence->markups name markup-parsers #:key (markup-acc '()))
  (if (null? markup-parsers)
      (cons (reverse markup-acc) name)
      (let ((markup? ((car markup-parsers) name)))
        (parse-sequence->markups
         (cdr markup?)
         (cdr markup-parsers)
         #:markup-acc (if (null? (car markup?))
                          markup-acc
                          (cons (car markup?) markup-acc))))))

(define (chordname->stencil layout props name)
  (let* ((props (prepend-alist-chain 'font-family 'sans props))
         (root? (root-basenote->markup name))
         (root-stencil? (markup?->stencil? layout
                                           (prepend-alist-chain 'font-size 2
                                                                props)
                                           (car root?)))
         (shoots-markup? (parse-sequence->markups (cdr root?)
                                                  (list accidental->markup
                                                        minor->markup
                                                        augmented->markup
                                                        diminished->markup
                                                        extensions->markup)))
         (shoot-stencil? (and (not (null? (car shoots-markup?)))
                                (interpret-markup
                                 layout props
                                 (make-concat-markup (car shoots-markup?))))))
    (if (not root-stencil?)
        (format #t "Chord ~S has no root!\n" name))
    (stack-stencil-line 0 (filter ly:stencil? (list root-stencil? shoot-stencil?)))))

(define-markup-command (jazzchord layout props name)
  (string?)
  (chordname->stencil layout props name))

(define jazz (define-scheme-function (name) (string?)
                #{
                  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
                  \once \override Score.RehearsalMark.font-size = #0
                  #(mark (markup #:jazzchord name))
                  #}))

(define slashchord (define-scheme-function (chord base) (string? string?)
                     #{
                       \once \override Score.RehearsalMark.self-alignment-X = #LEFT
                       \once \override Score.RehearsalMark.font-size = #0
                       #(mark (markup #:jazzchord chord #:huge "/" #:jazzchord base))
                       #}))

(define jaz
  (define-scheme-function (chord note) (string? ly:music?)
    ;; if sequential music, take last element, return full music expr
    (let ((articulations (list (make-music
                                'TextScriptEvent
                                'direction 1
                                'text (markup #:jazzchord chord)))))
      (set! (ly:music-property
             (if (music-is-of-type? note 'sequential-music)
                 (last (ly:music-property note 'elements))
                 note)
             'articulations)
        articulations))
    note))
