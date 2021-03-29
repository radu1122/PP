#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (if (null? counters)
      counters
      (if (equal? (counter-index (car counters)) index)
      (cons (f (car counters)) (update f (cdr counters) index))
      (cons (car counters) (update f (cdr counters) index))
      )))

(define (tt+ minutes)
  (lambda (C)
    (match C
    [(counter index tt et queue)
     (struct-copy counter C [tt (+ (counter-tt C) minutes)])])))

(define (et+ minutes)
  (lambda (C)
    (match C
    [(counter index tt et queue)
     (struct-copy counter C [et (+ (counter-et C) minutes)])])))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
        (struct-copy counter C [tt (+ (counter-tt C) items)] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C))])
        (struct-copy counter C [tt (+ (counter-tt C) items)] [queue (enqueue (cons name items) (counter-queue C))])
        )
    ))

(define (minimum-main f counters min emptyCare)
  (cond
    ((null? counters) min)
    ((< (f (car counters)) (cdr min)) (if (equal? emptyCare 1)
                                        (if (queue-empty? (counter-queue (car counters)))
                                            (minimum-main f (cdr counters) min emptyCare)
                                            (minimum-main f (cdr counters) (cons (counter-index (car counters)) (f (car counters))) emptyCare)
                                         )
                                       (minimum-main f (cdr counters) (cons (counter-index (car counters)) (f (car counters))) emptyCare)))
    (else
     (minimum-main f (cdr counters) min emptyCare))))

(define (min-tt counters) (minimum-main counter-tt counters (cons 0 100000000) 0)) ; folosind funcția de mai sus
(define (min-et counters) (minimum-main counter-et counters (cons 0 100000000) 1)) ; folosind funcția de mai sus


(define (tt-sum queue)
  (if (queue-empty? queue)
      0
  (+ (cdr (top queue)) (tt-sum (dequeue queue))))
  )

(define (remove-first-from-counter C)   ; testată de checker)
  (cond ((queue-empty? (counter-queue C)) (struct-copy counter C [tt 0] [et 0] [queue empty-queue]))
    ((queue-empty? (dequeue (counter-queue C))) (struct-copy counter C [tt 0] [et 0] [queue empty-queue]))
        
      (else (struct-copy counter C [tt (tt-sum (dequeue (counter-queue C)))] [et ( cdr (top (dequeue (counter-queue C))))] [queue (dequeue (counter-queue C))]))
  ))

; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
  (match C
    [(counter index tt et queue)
     (if (< (- et minutes) 0)
         (if (< (- tt minutes) 0)
             (struct-copy counter C [et 0]  [tt 0] )
             (struct-copy counter C [et 0]  [tt (- (counter-tt C) minutes)] )
         )

         (struct-copy counter C [et (- (counter-et C) minutes)]  [tt (- (counter-tt C) minutes)] )
     )
    ])))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (clients-out-list counters)
  (if (empty? counters)
      null
      (if (equal? (counter-et (car counters)) 0)
          (if (queue-empty? (counter-queue(car counters)))
              (clients-out-list (cdr counters)) 
              (append (list (cons (counter-index(car counters))  (car (top (counter-queue (car counters)))))) (clients-out-list (cdr counters)))   
              )
          (clients-out-list (cdr counters)) 
      )

  )
)

(define (get-clients-out-pass-minutes C minutes clients)
  (match C
    [(counter index tt et queue)
     (if (equal? minutes 0)
         (cons C clients)
         (if (queue-empty? queue)
                 (get-clients-out-pass-minutes ((pass-time-through-counter 1) C) (- minutes 1) clients)
             (if (equal? (- et 1) 0)
                 (if (queue-empty? (dequeue queue))
                     (get-clients-out-pass-minutes (struct-copy counter C [et 0]  [tt 0] [queue (dequeue queue)]) (- minutes 1) (append clients (list (cons index (car (top queue))))))
                     (get-clients-out-pass-minutes (struct-copy counter C [et (cdr (top (dequeue queue)))]  [tt (- tt 1)] [queue (dequeue queue)]) (- minutes 1) (append clients (list (cons index (car (top queue))))))
                  )
                 (get-clients-out-pass-minutes (struct-copy counter C [et (- et 1)]  [tt (- tt 1)]) (- minutes 1) clients)
             
                 )
          )
     )
 ]))


(define (my-map-counters counters minutes counters-final clients)
  (if (empty? counters)
      (cons counters-final clients)
      (match (car counters)
        [(counter index tt et queue)
         (let*
             (
              (output (get-clients-out-pass-minutes (car counters) minutes '()))
              (counter (car output))
              (clientsX (cdr output)))
           (my-map-counters (cdr counters) minutes (append counters-final (list counter)) (append clients clientsX)))
       ])
   )
  
)
  


(define (counters-tt-map counters)
  (cond
    ((null? counters) (list 0))
    ((counter? counters) (list (counter-tt counters)))
  (else (append (list (counter-tt (car counters))) (counters-tt-map (cdr counters)))))
)


(define (serve requests fast-counters slow-counters)
  (serve-main requests fast-counters slow-counters '()))

(define (serve-main requests fast-counters slow-counters clients)
 (if (null? requests)
      (cons clients (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) (if (> ( / (apply + (counters-tt-map (append fast-counters slow-counters))) (- (length(counters-tt-map (append fast-counters slow-counters))) 1)) average)
                                    (serve-main requests fast-counters (append slow-counters (list (empty-counter (length(counters-tt-map (append fast-counters slow-counters)))))) clients)
                                    (serve-main (cdr requests) fast-counters slow-counters clients)
                                    )]
        [(list 'delay index minutes) (serve-main (cdr requests) (update (et+ minutes) (update (tt+ minutes) fast-counters index) index)    (update (et+ minutes) (update (tt+ minutes) slow-counters index) index) clients)]
        [(list name n-items) (if (<= n-items ITEMS)
                                 (serve-main (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))    (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters)))) clients )
                                 (serve-main (cdr requests) fast-counters    (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) clients )
                                 )]
        [(var minutes) (if (equal? minutes 0)
                           (serve-main (cdr requests) fast-counters slow-counters clients)
                           (let*
                               (
                                (output-fast (my-map-counters fast-counters 1 '() '()))
                                (output-slow (my-map-counters slow-counters 1 '() '()))
                                (output-clients (append (cdr output-fast) (cdr output-slow)))
                                )
                         
                             (serve-main (cons (- minutes 1)(cdr requests)) (car output-fast) (car output-slow) (append clients output-clients))
                             
                        )
                           )
        ]
      )
   ))
        
