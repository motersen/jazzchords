;; jazzchords.scm
;; Jazz chord symbol parser and LilyPond markup generator
;;
;; Copyright 2020 Moritz Petersen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;   http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Example markup tree for "F#mMaj7b13"
;;
;; (concat-markup
;;  ((sans-markup
;;    (huge-markup
;;     (concat-markup
;;      ((simple-markup "F")
;;       (small-markup
;;        (normal-size-super-markup
;;         (musicglyph-markup "accidentals.sharp")))))))
;;   (sans-markup
;;    (concat-markup
;;     ((simple-markup "m")
;;      (normal-size-super-markup
;;       (concat-markup
;;        ((concat-markup
;;          ((simple-markup "Maj")
;;           (simple-markup "7")))
;;         (concat-markup
;;          ((concat-markup
;;            ((hspace-markup
;;              0.2)
;;             (normal-size-super-markup
;;              (musicglyph-markup "accidentals.flat"))
;;             (simple-markup "13")))))))))))))


;; ->markup parsers return pairs where
;; car = resulting markup expression
;; cdr = unprocessed part of input

(use-modules (ice-9 regex))

(define (collect-nonempty-markups markups?)
  (let ((result (fold-right (lambda (markup? acc)
                              (if (null? (car markup?))
                                  acc
                                  (cons (car markup?) acc)))
                            '()
                            markups?)))
    result))

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
      (cons (make-sans-markup
             (make-huge-markup
              (make-concat-markup
               (collect-nonempty-markups (list root? accidental?)))))
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
        (match-step (make-regexp "^[1-9]{1,2}|6/9"))
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
             (extension? (collect-nonempty-markups
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

(define (chordname->markup name)
  (let* ((root? (root-basenote->markup name))
         (shoots-markup? (parse-sequence->markups (cdr root?)
                                                  (list accidental->markup
                                                        minor->markup
                                                        augmented->markup
                                                        diminished->markup
                                                        extensions->markup))))
    (cons (make-concat-markup (list
                               (car root?)
                               (make-sans-markup
                                (make-concat-markup
                                 (car shoots-markup?)))))
          (cdr shoots-markup?))))

(define slashchord->markup
  (let ((match-slashchord (make-regexp "^/")))
    (lambda (chord-markup rest)
      (let ((match? (regexp-exec match-slashchord rest)))
        (if (not match?)
            (cons chord-markup rest)
            (let ((basenote (root-basenote->markup (match:suffix match?))))
              (cons
               (make-column-markup
                (list
                 (make-halign-markup RIGHT chord-markup)
                 (make-vspace-markup -.5)
                 (make-halign-markup CENTER (make-draw-line-markup '(3 . 3)))
                 (make-vspace-markup -.4)
                 (make-halign-markup LEFT (car basenote))))
               (cdr basenote))))))))

(define (parse-complex-chord name)
  (let* ((chord? (chordname->markup name))
         (slashchord? (slashchord->markup (car chord?) (cdr chord?))))
    (if (null? (car slashchord?))
        (car chord?)
        (car slashchord?))))

(define-markup-command (jazzchord layout props name)
  (string?)
  (let ((override-props '((thickness . 1.5)
                          (baseline-skip . 0)))
        (prepend-prop (lambda (setting props)
                        (prepend-alist-chain
                         (car setting) (cdr setting)
                         props))))
    (interpret-markup layout
                      (fold prepend-prop
                            props
                            override-props)
                      (parse-complex-chord name))))

(define jazz (define-scheme-function (name) (string?)
                #{
                  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
                  \once \override Score.RehearsalMark.font-size = #0
                  %% ensure chord symbols are not raised above volta brackets etc.
                  \once \override Score.RehearsalMark.outside-staff-priority = #550
                  %% if chord symbols collide, space out music
                  \once \markLengthOn
                  #(mark (markup #:jazzchord name))
                  #}))

(define jc
  (define-scheme-function (chord note) (string? ly:music?)
    (let ((text (make-music
                 'TextScriptEvent
                 'direction 1
                 'text (markup #:jazzchord chord)))
          (articulations (ly:music-property
                          (if (music-is-of-type? note 'sequential-music)
                              (last (ly:music-property note 'elements))
                              note)
                          'articulations)))
      (ly:music-set-property!
       (if (music-is-of-type? note 'sequential-music)
                              (last (ly:music-property note 'elements))
                              note)
       'articulations
       (cons text articulations)))
    #{
      \once \override TextScript.outside-staff-priority = #550
      %% if chord symbols collide, space out music
      \once \textLengthOn
      #note
      #}
    ))
