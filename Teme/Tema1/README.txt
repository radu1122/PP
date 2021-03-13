Copyright Radu-Andrei Dumitrescu 2021

Timp de realizare: aproximativ 60min


Functii implementate: 
    1. empty-counter
        - am creat o structura un indexul primit ca parametru, tt = 0 si un queue gol
    2. tt+
        - am returnat o structura noua de tip counter care avea tt modificat cu + minutes
          fata de structura initiala
    3. min-tt
        - am ales sa implementez o solutie cu recursivitate pe coada
        - a fost nevoie de o noua functie care mai avea un parametru current min element
        - primul apel din functia min-tt al lui minimum-main a fost cu min considerat primul element din lista
    4. add-to-counter
        - am returnat o structura noua care avea modificat tt-ul cu + n-items si queue la care s-a facut append perechea noua (name . items)
    5. serve
        - implementat cu apel recursiv al lui serve
        - am avut 2 mari cazuri, cand era element de delay si cand era client care trebuia adaugat
        - pentru primul caz cam facut un cond in care am verificat care este indexul cerut si apoi am facut apel recursiv pe serve
        - pentru al 2lea caz am folosit tot un cond, dar inbricat intr-un if care examina conditia casei C1


Probleme intampinate:
    - nu am intampinat niciun fel de problema la aceasta etapa
    