; Paula Villanueva
;;;;;;; JUGADOR DE 4 en RAYA ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;; Version de 4 en raya clásico: Tablero de 6x7, donde se introducen fichas por arriba
;;;;;;;;;;;;;;;;;;;;;;; y caen hasta la posicion libre mas abajo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Hechos para representar un estado del juego

;;;;;;; (Turno M|J)   representa a quien corresponde el turno (M maquina, J jugador)
;;;;;;; (Tablero Juego ?i ?j _|M|J) representa que la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)

;;;;;;;;;;;;;;;; Hechos para representar estado del analisis
;;;;;;; (Tablero Analisis Posicion ?i ?j _|M|J) representa que en el analisis actual la posicion i,j del tablero esta vacia (_), o tiene una ficha propia (M) o tiene una ficha del jugador humano (J)
;;;;;;; (Sondeando ?n ?i ?c M|J)  ; representa que estamos analizando suponiendo que la ?n jugada h sido ?i ?c M|J
;;;

;;;;;;;;;;;;; Hechos para representar una jugadas

;;;;;;; (Juega M|J ?columna) representa que la jugada consiste en introducir la ficha en la columna ?columna 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIALIZAR ESTADO


(deffacts Estado_inicial
(Tablero Juego 1 1 _) (Tablero Juego 1 2 _) (Tablero Juego 1 3 _) (Tablero Juego  1 4 _) (Tablero Juego  1 5 _) (Tablero Juego  1 6 _) (Tablero Juego  1 7 _)
(Tablero Juego 2 1 _) (Tablero Juego 2 2 _) (Tablero Juego 2 3 _) (Tablero Juego 2 4 _) (Tablero Juego 2 5 _) (Tablero Juego 2 6 _) (Tablero Juego 2 7 _)
(Tablero Juego 3 1 _) (Tablero Juego 3 2 _) (Tablero Juego 3 3 _) (Tablero Juego 3 4 _) (Tablero Juego 3 5 _) (Tablero Juego 3 6 _) (Tablero Juego 3 7 _)
(Tablero Juego 4 1 _) (Tablero Juego 4 2 _) (Tablero Juego 4 3 _) (Tablero Juego 4 4 _) (Tablero Juego 4 5 _) (Tablero Juego 4 6 _) (Tablero Juego 4 7 _)
(Tablero Juego 5 1 _) (Tablero Juego 5 2 _) (Tablero Juego 5 3 _) (Tablero Juego 5 4 _) (Tablero Juego 5 5 _) (Tablero Juego 5 6 _) (Tablero Juego 5 7 _)
(Tablero Juego 6 1 _) (Tablero Juego 6 2 _) (Tablero Juego 6 3 _) (Tablero Juego 6 4 _) (Tablero Juego 6 5 _) (Tablero Juego 6 6 _) (Tablero Juego 6 7 _)
(Jugada 0)
)

(defrule Elige_quien_comienza
=>
(printout t "Quien quieres que empieze: (escribre M para la maquina o J para empezar tu) ")
(assert (Turno (read)))
)

;;;;;;;;;;;;;;;;;;;;;;; MUESTRA POSICION ;;;;;;;;;;;;;;;;;;;;;;;
(defrule muestra_posicion
(declare (salience 10))
(muestra_posicion)
(Tablero Juego 1 1 ?p11) (Tablero Juego 1 2 ?p12) (Tablero Juego 1 3 ?p13) (Tablero Juego 1 4 ?p14) (Tablero Juego 1 5 ?p15) (Tablero Juego 1 6 ?p16) (Tablero Juego 1 7 ?p17)
(Tablero Juego 2 1 ?p21) (Tablero Juego 2 2 ?p22) (Tablero Juego 2 3 ?p23) (Tablero Juego 2 4 ?p24) (Tablero Juego 2 5 ?p25) (Tablero Juego 2 6 ?p26) (Tablero Juego 2 7 ?p27)
(Tablero Juego 3 1 ?p31) (Tablero Juego 3 2 ?p32) (Tablero Juego 3 3 ?p33) (Tablero Juego 3 4 ?p34) (Tablero Juego 3 5 ?p35) (Tablero Juego 3 6 ?p36) (Tablero Juego 3 7 ?p37)
(Tablero Juego 4 1 ?p41) (Tablero Juego 4 2 ?p42) (Tablero Juego 4 3 ?p43) (Tablero Juego 4 4 ?p44) (Tablero Juego 4 5 ?p45) (Tablero Juego 4 6 ?p46) (Tablero Juego 4 7 ?p47)
(Tablero Juego 5 1 ?p51) (Tablero Juego 5 2 ?p52) (Tablero Juego 5 3 ?p53) (Tablero Juego 5 4 ?p54) (Tablero Juego 5 5 ?p55) (Tablero Juego 5 6 ?p56) (Tablero Juego 5 7 ?p57)
(Tablero Juego 6 1 ?p61) (Tablero Juego 6 2 ?p62) (Tablero Juego 6 3 ?p63) (Tablero Juego 6 4 ?p64) (Tablero Juego 6 5 ?p65) (Tablero Juego 6 6 ?p66) (Tablero Juego 6 7 ?p67)
=>
(printout t crlf)
(printout t ?p11 " " ?p12 " " ?p13 " " ?p14 " " ?p15 " " ?p16 " " ?p17 crlf)
(printout t ?p21 " " ?p22 " " ?p23 " " ?p24 " " ?p25 " " ?p26 " " ?p27 crlf)
(printout t ?p31 " " ?p32 " " ?p33 " " ?p34 " " ?p35 " " ?p36 " " ?p37 crlf)
(printout t ?p41 " " ?p42 " " ?p43 " " ?p44 " " ?p45 " " ?p46 " " ?p47 crlf)
(printout t ?p51 " " ?p52 " " ?p53 " " ?p54 " " ?p55 " " ?p56 " " ?p57 crlf)
(printout t ?p61 " " ?p62 " " ?p63 " " ?p64 " " ?p65 " " ?p66 " " ?p67 crlf)
(printout t  crlf)
)


;;;;;;;;;;;;;;;;;;;;;;; RECOGER JUGADA DEL CONTRARIO ;;;;;;;;;;;;;;;;;;;;;;;
(defrule mostrar_posicion
(declare (salience 9999))
(Turno J)
=>
(assert (muestra_posicion))
)

(defrule jugada_contrario
?f <- (Turno J)
=>
(printout t "en que columna introduces la siguiente ficha? ")
(assert (Juega J (read)))
(retract ?f)
)

(defrule juega_contrario_check_entrada_correcta
(declare (salience 1))
?f <- (Juega J ?c)
(test (and (neq ?c 1) (and (neq ?c 2) (and (neq ?c 3) (and (neq ?c 4) (and (neq ?c 5) (and (neq ?c 6) (neq ?c 7))))))))
=>
(printout t "Tienes que indicar un numero de columna: 1,2,3,4,5,6 o 7" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_check_columna_libre
(declare (salience 1))
?f <- (Juega J ?c)
(Tablero Juego 1 ?c ?X) 
(test (neq ?X _))
=>
(printout t "Esa columna ya esta completa, tienes que jugar en otra" crlf)
(retract ?f)
(assert (Turno J))
)

(defrule juega_contrario_actualiza_estado
?f <- (Juega J ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego ?i ?c J))
)

(defrule juega_contrario_actualiza_estado_columna_vacia
?f <- (Juega J ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno M) (Tablero Juego 6 ?c J))
)


;;;;;;;;;;; ACTUALIZAR  ESTADO TRAS JUGADA DE CLISP ;;;;;;;;;;;;;;;;;;

(defrule juega_clisp_actualiza_estado
?f <- (Juega M ?c)
?g <- (Tablero Juego ?i ?c _)
(Tablero Juego ?j ?c ?X) 
(test (= (+ ?i 1) ?j))
(test (neq ?X _))
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego ?i ?c M))
)

(defrule juega_clisp_actualiza_estado_columna_vacia
?f <- (Juega M ?c)
?g <- (Tablero Juego 6 ?c _)
=>
(retract ?f ?g)
(assert (Turno J) (Tablero Juego 6 ?c M))
)

;;;;;;;;;;; CLISP JUEGA SIN CRITERIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule elegir_jugada_aleatoria
(declare (salience -9998))
?f <- (Turno M)
=>
(assert (Jugar (random 1 7)))
(retract ?f)
)

(defrule comprobar_posible_jugada_aleatoria
?f <- (Jugar ?c)
(Tablero Juego 1 ?c M|J)
=>
(retract ?f)
(assert (Turno M))
)

(defrule clisp_juega_sin_criterio
(declare (salience -9999))
?f<- (Jugar ?c)
=>
(printout t "JUEGO en la columna (sin criterio) " ?c crlf)
(retract ?f)
(assert (Juega M ?c))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;  Comprobar si hay 4 en linea ;;;;;;;;;;;;;;;;;;;;;

(defrule cuatro_en_linea_horizontal
(declare (salience 9999))
(Tablero ?t ?i ?c1 ?jugador)
(Tablero ?t ?i ?c2 ?jugador) 
(test (= (+ ?c1 1) ?c2))
(Tablero ?t ?i ?c3 ?jugador)
(test (= (+ ?c1 2) ?c3))
(Tablero ?t ?i ?c4 ?jugador)
(test (= (+ ?c1 3) ?c4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador horizontal ?i ?c1))
)

(defrule cuatro_en_linea_vertical
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i1 ?c ?jugador)
(Tablero ?t ?i2 ?c ?jugador)
(test (= (+ ?i1 1) ?i2))
(Tablero ?t ?i3 ?c  ?jugador)
(test (= (+ ?i1 2) ?i3))
(Tablero ?t ?i4 ?c  ?jugador)
(test (= (+ ?i1 3) ?i4))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador vertical ?i1 ?c))
)

(defrule cuatro_en_linea_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (+ ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (+ ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (+ ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_directa ?i ?c))
)

(defrule cuatro_en_linea_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Tablero ?t ?i ?c ?jugador)
(Tablero ?t ?i1 ?c1 ?jugador)
(test (= (+ ?i 1) ?i1))
(test (= (- ?c 1) ?c1))
(Tablero ?t ?i2 ?c2  ?jugador)
(test (= (+ ?i 2) ?i2))
(test (= (- ?c 2) ?c2))
(Tablero ?t ?i3 ?c3  ?jugador)
(test (= (+ ?i 3) ?i3))
(test (= (- ?c 3) ?c3))
(test (or (eq ?jugador M) (eq ?jugador J) ))
=>
(assert (Cuatro_en_linea ?t ?jugador diagonal_inversa ?i ?c))
)

;;;;;;;;;;;;;;;;;;;; DESCUBRE GANADOR
(defrule gana_fila
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador horizontal ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la fila " ?i crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_columna
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador vertical ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la columna " ?c crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_directa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_directa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal que empieza la posicion " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 

(defrule gana_diagonal_inversa
(declare (salience 9999))
?f <- (Turno ?X)
(Cuatro_en_linea Juego ?jugador diagonal_inversa ?i ?c)
=>
(printout t ?jugador " ha ganado pues tiene cuatro en linea en la diagonal hacia arriba que empieza la posicin " ?i " " ?c   crlf)
(retract ?f)
(assert (muestra_posicion))
) 


;;;;;;;;;;;;;;;;;;;;;;;  DETECTAR EMPATE

(defrule empate
(declare (salience -9999))
(Turno ?X)
(Tablero Juego 1 1 M|J)
(Tablero Juego 1 2 M|J)
(Tablero Juego 1 3 M|J)
(Tablero Juego 1 4 M|J)
(Tablero Juego 1 5 M|J)
(Tablero Juego 1 6 M|J)
(Tablero Juego 1 7 M|J)
=>
(printout t "EMPATE! Se ha llegado al final del juego sin que nadie gane" crlf)
)

;;;;;;;;;;;;;;;;;;;;;; CONOCIMIENTO EXPERTO ;;;;;;;;;;
;;;;; ¡¡¡¡¡¡¡¡¡¡ Añadir conocimiento para que juege como vosotros jugariais !!!!!!!!!!!!

; Ejercicio 1: reglas para deducir la posicion siguiente y anterior a una posicion
; Regla para deducir la siguiente posicion en horizontal a una posicion
(defrule deducir_siguiente_horizontal
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test (neq ?c1 7)) ; no puede ser la ultima columna
(test
    (and 
        (= ?f1 ?f2) ; misma fila
        (= ?c2 (+ ?c1 1)) ; las columnas c1 y c2 son contiguas
    )
)
=>
(assert (siguiente ?f2 ?c2 h ?f1 ?c1))
)

; Regla para deducir la siguiente posicion en vertical a una posicion
(defrule deducir_siguiente_vertical
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test (neq ?f1 1)) ; f1 no puede ser la fila de arriba
(test
    (and 
        (= ?c1 ?c2) ; misma columna
        (= ?f2 (- ?f1 1)) ; f1 es la fila de abajo
    )
)
=>
(assert (siguiente ?f2 ?c2 v ?f1 ?c1))
)

; Regla para deducir la siguiente posicion en diagonal a una posicion
(defrule deducir_siguiente_diagonal
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (or
        (neq ?f1 1) ; f1 no puede ser la fila de arriba
        (neq ?c1 7) ; no puede ser la ultima columna
    )
)
(test
    (and 
        (= ?c2 (+ ?c1 1)) ; c2 es la siguiente columna
        (= ?f2 (- ?f1 1)) ; f1 es la fila de abajo
    )
)
=>
(assert (siguiente ?f2 ?c2 d1 ?f1 ?c1))
)

; Regla para deducir la siguiente posicion en diagonal inversa a una posicion
(defrule deducir_siguiente_diagonal_inversa
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (or
        (neq ?f1 1) ; f1 no puede ser la fila de arriba
        (neq ?c1 1) ; no puede ser la primera columna
    )
)
(test
    (and 
        (= ?c2 (- ?c1 1)) ; c2 es la columna anterior
        (= ?f2 (- ?f1 1)) ; f1 es la fila de abajo
    )
)
=>
(assert (siguiente ?f2 ?c2 d2 ?f1 ?c1))
)

; Regla para deducir la anterior posicion en horizontal a una posicion
(defrule deducir_anterior_horizontal
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test (neq ?c1 1)) ; no puede ser la primera columna
(test
    (and 
        (= ?f1 ?f2) ; misma fila
        (= ?c2 (- ?c1 1)) ; las columnas c1 y c2 son contiguas
    )
)
=>
(assert (anterior ?f2 ?c2 h ?f1 ?c1))
)
; Regla para deducir la anterior posicion en vertical a una posicion
(defrule deducir_anterior_vertical
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test (neq ?f1 6)) ; f1 no puede ser la fila de abajo
(test
    (and 
        (= ?c1 ?c2) ; misma columna
        (= ?f2 (+ ?f1 1)) ; f2 es la fila de abajo
    )
)
=>
(assert (anterior ?f2 ?c2 v ?f1 ?c1))
)

; Regla para deducir la anterior posicion en diagonal a una posicion
(defrule deducir_anterior_diagonal
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (or
        (neq ?f1 6) ; f1 no puede ser la fila de abajo
        (neq ?c1 1) ; no puede ser la primera columna
    )
)
(test
    (and 
        (= ?c2 (- ?c1 1)) ; c1 es la siguiente columna
        (= ?f2 (+ ?f1 1)) ; f2 es la fila de abajo
    )
)
=>
(assert (anterior ?f2 ?c2 d1 ?f1 ?c1))
)

; Regla para deducir la anterior posicion en diagonal inversa a una posicion
(defrule deducir_anterior_diagonal_inversa
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (or
        (neq ?f1 6) ; f1 no puede ser la fila de abajo
        (neq ?c1 7) ; no puede ser la ultima columna
    )
)
(test
    (and 
        (= ?c2 (+ ?c1 1)) ; c1 es la columna anterior
        (= ?f2 (+ ?f1 1)) ; f1 es la fila de abajo
    )
)
=>
(assert (anterior ?f2 ?c2 d2 ?f1 ?c1))
)


; Ejercicio 2: reglas para deducir donde caeria una ficha si se juega en la columna c

; Regla para deducir la posicion en donde caeria si jugamos en la columna c
(defrule deducir_caeria
(declare (salience 999))
(Turno M)
(Tablero Juego ?f1 ?c ?v1)
(Tablero Juego ?f2 ?c ?v2)
(test
    (or
        (= ?f1 6) ; ultima fila
        (= (+ ?f1 1) ?f2) ; f2 fila de abajo (para poder comparar si hay ficha)
    )
)
(test
    (or
        (and
            (= ?f1 6) ; ultima fila
            (eq ?v1 _) ; no tiene ficha (luego la columna esta vacia)
        )
        (and
            (neq ?v2 _) ; hay ficha
            (eq ?v1 _) ; no hay ficha (luego la fila de abajo tiene ficha y la coloco en la primera fila de arriba sin fichas)
        )
    )          
)
=>
(assert (caeria ?f1 ?c))
;(printout t "Caeria" ?f1 ?c crlf)
)

; Ejercicio 3: reglas para deducir si hay dos fichas de un mismo jugador en linea en una direccion y posiciones concretas

; Regla para deducir si hay dos fichas conectadas de forma horizontal
(defrule deducir_conectado_horizontal
(declare (salience 998))
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (neq ?v1 _) ; hay una ficha
)
(test
    (and
        (and
            (= ?c2 (+ ?c1 1)) ; las columnas c1 y c2 son contiguas
            (eq ?v1 ?v2) ; mismo jugador
        )
        (= ?f1 ?f2) ; estamos en la misma fila
    )
)
=>
(assert (conectado Juego h ?f1 ?c1 ?f2 ?c2 ?v1))
)

; Regla para deducir si hay dos fichas conectadas de forma vertical
(defrule deducir_conectado_vertical
(declare (salience 998))
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (neq ?v1 _) ; hay una ficha
)
(test
    (and
        (and
            (= ?f1 (+ ?f2 1)) ; f2 es la fila de arriba de f1
            (eq ?v1 ?v2) ; mismo jugador
        )
        (= ?c1 ?c2) ; estamos en la misma columna
    )
)
=>
(assert (conectado Juego v ?f1 ?c1 ?f2 ?c2 ?v1))
)

; Regla para deducir si hay dos fichas conectadas de forma diagonal
(defrule deducir_conectado_diagonal
(declare (salience 998))
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (neq ?v1 _) ; hay una ficha
)
(test
    (and
        (and
            (= ?f1 (+ ?f2 1)) ; f2 es la fila de arriba de f1
            (eq ?v1 ?v2) ; mismo jugador
        )
        (= ?c2 (+ ?c1 1)) ; las columnas c1 y c2 son contiguas
    )
)
=>
(assert (conectado Juego d1 ?f1 ?c1 ?f2 ?c2 ?v1))
)

; Regla para deducir si hay dos fichas conectadas de forma diagonal inversa
(defrule deducir_conectado_diagonal_inversa
(declare (salience 998))
(Turno M)
(Tablero Juego ?f1 ?c1 ?v1)
(Tablero Juego ?f2 ?c2 ?v2)
(test
    (neq ?v1 _) ; hay una ficha
)
(test
    (and
        (and
            (= ?f1 (+ ?f2 1)) ; f2 es la fila de arriba de f1
            (eq ?v1 ?v2) ; mismo jugador
        )
        (= ?c1 (+ ?c2 1)) ; las columnas c1 y c2 son contiguas
    )
)
=>
(assert (conectado Juego d2 ?f1 ?c1 ?f2 ?c2 ?v1))
)

; Ejercicio 4: reglas para deducir si hay tres fichas de un mismo jugador en linea en una direccion y posiciones concretas

; Regla para deducir si hay tres fichas conectadas de forma horizontal
(defrule deducir_3_en_linea_horizontal
(declare (salience 999))
(Turno M)
?h1 <- (conectado Juego h ?f1 ?c1 ?f2 ?c2 ?v1) 
?h2 <- (conectado Juego h ?f3 ?c3 ?f4 ?c4 ?v2)
(test (eq ?v1 ?v2))
(test
    (and
        (= ?f1 ?f2) ; f1 y f2 misma fila
        (and
            (= ?f3 ?f4) ; f3 y f4 misma fila
            (= ?f1 ?f3) ; f1 y f3 misma fila (luego f1, f2, f3 y f4 misma fila)
        )
    )
)
(test
    (and
        (= ?c2 (+ ?c1 1)) ; las columnas c1 y c2 son contiguas
        (and
            (= ?c2 ?c3) ; las columnas c2 y c3 son las mismas
            (= ?c4 (+ ?c3 1)) ; las columnas c3 y c4 son contiguas
        )  
    )
)
=>
(assert (3_en_linea Juego h ?f1 ?c1 ?f4 ?c4 ?v1))
(retract ?h1) ; ya sabemos que dos fichas estan en linea
(retract ?h2) ; ya sabemos que dos fichas estan en linea
)

; Regla para deducir si hay tres fichas conectadas de forma vertical
(defrule deducir_3_en_linea_vertical
(declare (salience 999))
(Turno M)
?h1 <- (conectado Juego v ?f1 ?c1 ?f2 ?c2 ?v1)
?h2 <- (conectado Juego v ?f3 ?c3 ?f4 ?c4 ?v2)
(test (eq ?v1 ?v2))
(test
    (and
        (= ?c1 ?c2) ; c1 y c2 misma columna
        (and
            (= ?c3 ?c4) ; c3 y c4 misma columna
            (= ?c1 ?c3) ; c1 y c3 misma columna (luego c1, c2, c3 y c4 misma fila)
        )
    )
)
(test
    (and
        (= ?f1 (+ ?f2 1)) ; las filas f1 y f2 son contiguas
        (and
            (= ?f2 ?f3) ; las filas f2 y f3 son las mismas
            (= ?f3 (+ ?f4 1)) ; las filas f3 y f4 son contiguas
        )
        
    )
)
=>
(assert (3_en_linea Juego v ?f1 ?c1 ?f4 ?c4 ?v1))
(retract ?h1) ; ya sabemos que dos fichas estan en linea
(retract ?h2) ; ya sabemos que dos fichas estan en linea
)

; Regla para deducir si hay tres fichas conectadas de forma diagonal
(defrule deducir_3_en_linea_diagonal
(declare (salience 999))
(Turno M)
?h1 <- (conectado Juego d1 ?f1 ?c1 ?f2 ?c2 ?v1)
?h2 <- (conectado Juego d1 ?f3 ?c3 ?f4 ?c4 ?v2)
(test (eq ?v1 ?v2))
(test
    (and
        (= ?c2 (+ ?c1 1)) ; las columnas c1 y c2 son contiguas
        (and
            (= ?c2 ?c3) ; las columnas c2 y c3 son las mismas
            (= ?c4 (+ ?c3 1)) ; las columnas c3 y c4 son contiguas
        )
        
    )
)
(test
    (and
        (= ?f1 (+ ?f2 1)) ; las filas f1 y f2 son contiguas
        (and
            (= ?f2 ?f3) ; las filas f2 y f3 son las mismas
            (= ?f3 (+ ?f4 1)) ; las filas f3 y f4 son contiguas
        )
        
    )
)
=>
(assert (3_en_linea Juego d1 ?f1 ?c1 ?f4 ?c4 ?v1))
(retract ?h1) ; ya sabemos que dos fichas estan en linea
(retract ?h2) ; ya sabemos que dos fichas estan en linea
)

; Regla para deducir si hay tres fichas conectadas de forma diagonal inversa
(defrule deducir_3_en_linea_diagonal_inversa
(declare (salience 999))
(Turno M)
?h1 <- (conectado Juego d2 ?f1 ?c1 ?f2 ?c2 ?v1)
?h2 <- (conectado Juego d2 ?f3 ?c3 ?f4 ?c4 ?v2)
(test (eq ?v1 ?v2))
(test
    (and
        (= ?c1 (+ ?c2 1)) ; las columnas c1 y c2 son contiguas
        (and
            (= ?c2 ?c3) ; las columnas c2 y c3 son las mismas
            (= ?c3 (+ ?c4 1)) ; las columnas c3 y c4 son contiguas
        )
        
    )
)
(test
    (and
        (= ?f1 (+ ?f2 1)) ; las filas f1 y f2 son contiguas
        (and
            (= ?f2 ?f3) ; las filas f2 y f3 son las mismas
            (= ?f3 (+ ?f4 1)) ; las filas f3 y f4 son contiguas
        )
        
    )
)
=>
(assert (3_en_linea Juego d2 ?f1 ?c1 ?f4 ?c4 ?v1))
(retract ?h1) ; ya sabemos que dos fichas estan en linea
(retract ?h2) ; ya sabemos que dos fichas estan en linea
)

; Ejercicio 5: reglas para deducir si un jugador gana si juega en una columna

; Regla para deducir si un jugador gana de forma horizontal
(defrule deducir_ganaria_horizontal
(declare (salience 999))
(Turno M)
(3_en_linea Juego h ?f1 ?c1 ?f2 ?c2 ?v)
(Tablero Juego ?f3 ?c3 ?val)
(caeria ?f3 ?c3)
(test (eq ?val _))
(test
    (or
        (= ?c1 (+ ?c3 1)) ; o, las columnas c1 y c3 son contiguas
        (= ?c2 (- ?c3 1)) ; o, las columnas c2 y c3 son contiguas
    )
)
(test
    (and
        (= ?f1 ?f2) ; las filas f1 y f2 son las mismas
        (= ?f2 ?f3) ; las filas f2 y f3 son las mismas
    )
)
=>
(assert (ganaria ?v ?c3))
)

; Regla para deducir si un jugador gana de forma vertical
(defrule deducir_ganaria_vertical
(declare (salience 999))
(Turno M)
(3_en_linea Juego v ?f1 ?c1 ?f2 ?c2 ?v)
(Tablero Juego ?f3 ?c3 ?val)
(caeria ?f3 ?c3)
(test (eq ?val _))
(test (= ?f2 (+ ?f3 1))) ; las filas f2 y f3 son contiguas
(test
    (and
        (= ?c1 ?c2) ; las columnas c1 y c2 son las mismas
        (= ?c2 ?c3) ; las columnas c2 y c3 son las mismas
    )
)
=>
(assert (ganaria ?v ?c3))
)

; Regla para deducir si un jugador gana de forma diagonal
(defrule deducir_ganaria_diagonal
(declare (salience 999))
(Turno M)
(3_en_linea Juego d1 ?f1 ?c1 ?f2 ?c2 ?v)
(Tablero Juego ?f3 ?c3 ?val)
(caeria ?f3 ?c3)
(test (eq ?val _))
(test
    (or
        (and
            (= ?f1 (- ?f3 1))
            (= ?c1 (+ ?c3 1))
        )
        (and
            (= ?f2 (+ ?f3 1))
            (= ?c2 (- ?c3 1))
        )
    )
)
=>
(assert (ganaria ?v ?c3))
)

; Regla para deducir si un jugador gana de forma diagonal inversa
(defrule deducir_ganaria_diagonal_inversa
(declare (salience 999))
(Turno M)
(3_en_linea Juego d2 ?f1 ?c1 ?f2 ?c2 ?v)
(Tablero Juego ?f3 ?c3 ?val)
(caeria ?f3 ?c3)
(test (eq ?val _))
(test
    (or
        (and
            (= ?f1 (- ?f3 1))
            (= ?c1 (- ?c3 1))
        )
        (and
            (= ?f2 (+ ?f3 1))
            (= ?c2 (+ ?c3 1))
        )
    )
)
=>
(assert (ganaria ?v ?c3))
)

; REGLAS para describir mi forma de jugar

; Regla para deducir si puedo ganar, entonces juego en esa columna
(defrule ganar_con_criterio
(declare (salience 900))
?f <- (Turno M)
(ganaria M ?c)
=>
(assert (Juega M ?c))
(printout t "M gana CON CRITERIO" crlf)
(retract ?f)
)

; Regla para deducir si J puede ganar, entonces juego en esa columna y lo saboteo
(defrule sabotear_con_criterio
(declare (salience 800))
?f <- (Turno M)
?g <- (ganaria J ?c)
=>
(assert (Juega M ?c))
(printout t "M sabotea CON CRITERIO la jugada a J en la columna" ?c crlf)
(retract ?f)
(retract ?g)
)

; Regla para contectar tres fichas de forma horizontal si ya habia dos
(defrule jugar_3_con_criterio_horizontal
(declare (salience 700))
?f <- (Turno M)
(conectado Juego h ?f1 ?c1 ?f2 ?c2 M)
(caeria ?f3 ?c3)
(Tablero Juego ?f3 ?c3 _) ; no hay ficha donde caeria
(test
    (and
        (or
            (= ?c3 (+ ?c2 1)) ; columnas contiguas
            (= ?c3 (- ?c1 1)) ; columnas contiguas
        )
        (= ?f3 ?f1) ; misma fila
    )
)
=>
(assert (Juega M ?c3))
(printout t "M juega CON CRITERIO en la columna " ?c3 crlf)
(retract ?f)
)

; Regla para contectar tres fichas de forma vertical si ya habia dos
(defrule jugar_3_con_criterio_vertical
(declare (salience 699))
?f <- (Turno M)
(conectado Juego v ?f1 ?c1 ?f2 ?c2 M)
(caeria ?f3 ?c3)
(Tablero Juego ?f3 ?c3 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f2 (+ ?f3 1)) ; filas contiguas
        (= ?c3 ?c1) ; misma columna
    )
)
=>
(assert (Juega M ?c3))
(printout t "M juega CON CRITERIO en la columna " ?c3 crlf)
(retract ?f)
)

; Regla para contectar tres fichas de forma diagonal si ya habia dos
(defrule jugar_3_con_criterio_diagonal
(declare (salience 698))
?f <- (Turno M)
(conectado Juego d1 ?f1 ?c1 ?f2 ?c2 M)
(caeria ?f3 ?c3)
(Tablero Juego ?f3 ?c3 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f2 (+ ?f3 1)) ; filas contiguas
        (or
            (= ?c3 (+ ?c2 1)) ; columnas contiguas
            (= ?c3 (- ?c1 1)) ; columnas contiguas
        )
    )
)
=>
(assert (Juega M ?c3))
(printout t "M juega CON CRITERIO en la columna " ?c3 crlf)
(retract ?f)
)

; Regla para contectar tres fichas de forma diagonal inversa si ya habia dos
(defrule jugar_3_con_criterio_diagonal_inversa
(declare (salience 697))
?f <- (Turno M)
(conectado Juego d2 ?f1 ?c1 ?f2 ?c2 M)
(caeria ?f3 ?c3)
(Tablero Juego ?f3 ?c3 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f2 (+ ?f3 1)) ; filas contiguas
        (or
            (= ?c3 (- ?c2 1)) ; columnas contiguas
            (= ?c3 (+ ?c1 1)) ; columnas contiguas
        )
    )
)
=>
(assert (Juega M ?c3))
(printout t "M juega CON CRITERIO en la columna " ?c3 crlf)
(retract ?f)
)

; Regla para contectar dos fichas de forma horizontal si ya habia una
(defrule jugar_2_con_criterio_horizontal
(declare (salience 600))
?f <- (Turno M)
(Tablero Juego ?f1 ?c1 M)
(caeria ?f2 ?c2)
(Tablero Juego ?f2 ?c2 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f2 ?f1) ; misma fila
        (or
            (= ?c2 (- ?c1 1)) ; las columnas son contiguas
            (= ?c2 (+ ?c1 1)) ; las columnas son contiguas
        )
    )
)
=>
(assert (Juega M ?c2))
(printout t "M juega CON CRITERIO en la columna " ?c2 crlf)
(retract ?f)
)

; Regla para contectar dos fichas de forma vertical si ya habia una
(defrule jugar_2_con_criterio_vertical
(declare (salience 599))
?f <- (Turno M)
(Tablero Juego ?f1 ?c1 M)
(caeria ?f2 ?c2)
(Tablero Juego ?f2 ?c2 _) ; no hay ficha donde caeria
(test
    (and
        (= ?c2 ?c1) ; misma columna
        (= ?f2 (- ?f1 1)) ; f2 es la fila de arriba a f1
    )
)
=>
(assert (Juega M ?c2))
(printout t "M juega CON CRITERIO en la columna " ?c2 crlf)
(retract ?f)
)

; Regla para contectar dos fichas de forma diagonal si ya habia una
(defrule jugar_2_con_criterio_diagonal
(declare (salience 598))
?f <- (Turno M)
(Tablero Juego ?f1 ?c1 M)
(caeria ?f2 ?c2)
(Tablero Juego ?f2 ?c2 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f1 (+ ?f2 1)) ; f1 es la fila de abajo a f2
        (= ?c2 (+ ?c1 1)) ; c2 es la columna siguiente a c1
    )
)
=>
(assert (Juega M ?c2))
(printout t "M juega CON CRITERIO en la columna " ?c2 crlf)
(retract ?f)
)

; Regla para contectar dos fichas de forma diagonal inversa si ya habia una
(defrule jugar_2_con_criterio_diagonal_inversa
(declare (salience 597))
?f <- (Turno M)
(Tablero Juego ?f1 ?c1 M)
(caeria ?f2 ?c2)
(Tablero Juego ?f2 ?c2 _) ; no hay ficha donde caeria
(test
    (and
        (= ?f1 (+ ?f2 1)) ; f1 es la fila de abajo a f2
        (= ?c2 (- ?c1 1)) ; c1 es la columna siguiente a c2
    )
)
=>
(assert (Juega M ?c2))
(printout t "M juega CON CRITERIO en la columna " ?c2 crlf)
(retract ?f)
)