; Paula Villanueva Nuñez

;;;;;;;;;;;;;;;; Representacion ;;;;;;;;;;;;;;;;

; (FactorCerteza ?h si|no ?f) representa que ?h se ha deducido con factor de certeza ?f
; ?h podra ser
;   - problema_starter
;   - problema_bujias
;   - problema_bateria
;   - motor_llega_gasolina

; (Evidencia ?e si|no) representa el hecho de si evidencia ?e se da
; ?e podra ser
;   - hace_intentos_arrancar
;   - hay_gasolina_en_deposito
;   - encienden_las_luces
;   - gira_motor

; (Motivo FactorCerteza ?h si|no ?f ?explicacion) representa la explicacion del factor


;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;

; Convertimos cada evidencia en una afirmacion sobre su factor de certeza
(defrule certeza_evidencias
    (declare (salience 999))
    (Evidencia ?e ?r)
    =>
    (assert (FactorCerteza ?e ?r 1))
    (assert (Motivo FactorCerteza ?e ?r 1 "me lo has dicho"))
)
; Tambien podriamos considerar evidencias con una cierta incertidumbre:
; al preguntar por la evidencia, pedir y recoger directamente el grado de certeza





;;;;;;;;;;;;;;;; Funcion encadenado ;;;;;;;;;;;;;;;;

(deffunction encadenado (?fc_antecedente ?fc_regla)
    (if (> ?fc_antecedente 0)
        then
            (bind ?rv (* ?fc_antecedente ?fc_regla))
        else
            (bind ?rv 0)
    )
    ?rv
)

;;;;;;;;;;;;;;;; Combinar distintas deducciones ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Funcion combinacion ;;;;;;;;;;;;;;;;

(deffunction combinacion (?fc1 ?fc2)
    (if (and (> ?fc1 0) (> ?fc2 0))
        then 
            (bind ?rv (- (+ ?fc1 ?fc2) (* ?fc1 ?fc2)))
    else 
            (if (and (< ?fc1 0) (< ?fc2 0))
                then 
                    (bind ?rv (+ (+ ?fc1 ?fc2) (* ?fc1 ?fc2)))
            else 
                    (bind ?rv (/ (+ ?fc1 ?fc2) (- 1 (min (abs ?fc1) (abs ?fc2)))))
            )
    )
    ?rv
)

; Combinar misma deduccion por distintos caminos
(defrule combinar
    (declare (salience 1))
    ?f <- (FactorCerteza ?h ?r ?fc1)
    ?g <- (FactorCerteza ?h ?r ?fc2)
    ?x <- (Motivo FactorCerteza ?h ?r ?fc1 ?expl1)
    ?y <- (Motivo FactorCerteza ?h ?r ?fc2 ?expl2)
    (test (neq ?fc1 ?fc2))
    =>
    (retract ?f ?g ?x ?y)
    (assert (FactorCerteza ?h ?r (combinacion ?fc1 ?fc2)))
    (assert (Motivo FactorCerteza ?h ?r (combinacion ?fc1 ?fc2) (str-cat ?expl1 " y " ?expl2)))
)


;;;;;;;;;;;;;;;; Certeza de las hipotesis ;;;;;;;;;;;;;;;;

(defrule combinar_signo
    (declare (salience 2))
    (FactorCerteza ?h si ?fc1)
    (FactorCerteza ?h no ?fc2)
    =>
    (assert (Certeza ?h (- ?fc1 ?fc2)))
)


;;;;;;;;;;;;;;;; Ejercicio ;;;;;;;;;;;;;;;;
;   - Preguntar por las posibles evidencias
;   - Añadir el resto de las reglas
;   - Tras razonar quedarse con las hipotesis con mayor certeza
;   - Añadir o modificar las reglas para que el sistema explique el por que de las afirmaciones

;;;;;;;;;;;;;;;; Preguntar por las posibles evidencias ;;;;;;;;;;;;;;;;

(defrule preguntar_hace_intentos_arrancar
    (declare (salience 99))
    (not (Evidencia hace_intentos_arrancar ?r))
    =>

    (printout t "Hace intentos de arrancar?" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia hace_intentos_arrancar ?respuesta))
)

(defrule preguntar_hay_gasolina_en_deposito
    (declare (salience 99))
    (not (Evidencia hay_gasolina_en_deposito ?r))

    =>

    (printout t "Hay gasolina en el deposito?" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia hay_gasolina_en_deposito ?respuesta))
)

(defrule preguntar_encienden_las_luces
    (declare (salience 99))
    (not (Evidencia encienden_las_luces ?r))

    =>

    (printout t "Encienden las luces?" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia encienden_las_luces ?respuesta))
)

(defrule preguntar_gira_motor
    (declare (salience 99))
    (not (Evidencia gira_motor ?r))

    =>

    (printout t "Gira el motor?" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gira_motor ?respuesta))
)

; Compruebo que la entrada sea valida
(defrule comprobar_entrada
    (declare (salience 999))
    ?x <- (Evidencia ?e ~si & ~no)
    
    =>

    (retract ?x)
    (printout t "Perdona no he entendido tu respuesta, introduce [si | no]")
)

;;;;;;;;;;;;;;;; Añadir el resto de las reglas ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Traducir reglas ;;;;;;;;;;;;;;;;

; R1: SI el motor obtiene gasolina Y el motor gira ENTONCES problemas con las bujias con certeza 0.7
(defrule R1
    (FactorCerteza motor_llega_gasolina si ?f1)
    (FactorCerteza gira_motor si ?f2)
    (test
        (and
            (> ?f1 0)
            (> ?f2 0)
        )
    )
    =>
    (assert (FactorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7)))
    (assert (Motivo FactorCerteza problema_bujias si (encadenado (* ?f1 ?f2) 0.7) "porque el motor obtiene gasolina y el motor gira"))
)

; R2: SI NO gira el motor ENTONCES problema con el starter con certeza 0.8
(defrule R2
    (FactorCerteza gira_motor no ?f1)
    (test (> ?f1 0))

    => 

    (assert (FactorCerteza problema_starter si (encadenado ?f1 0.8))) ; DUDA hay que llamar a funcion?
    (assert (Motivo FactorCerteza problema_starter si (encadenado ?f1 0.8) "porque no gira el motor"))
)

; R3: SI NO encienden las luces ENTONCES problemas con la bateria con certeza 0.9
(defrule R3
    (FactorCerteza encienden_las_luces no ?f1)
    (test (> ?f1 0))

    => 

    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.9))) ; DUDA hay que llamar a funcion?
    (assert (Motivo FactorCerteza problema_bateria si (encadenado ?f1 0.9) "porque no encienden las luces"))
)

; R4: SI hay gasolina en el deposito ENTONCES el motor obtiene gasolina con certeza 0.9
(defrule R4
    (FactorCerteza hay_gasolina_en_deposito si ?f1)
    (test (> ?f1 0))

    => 

    (assert (FactorCerteza motor_llega_gasolina si (encadenado ?f1 0.9))) ; DUDA hay que llamar a funcion?
    (assert (Motivo FactorCerteza motor_llega_gasolina si (encadenado ?f1 0.9) "porque hay gasolina en el deposito"))
)

; R5: SI hace intentos de arrancar ENTONCES problema con el starter con certeza -0.6
(defrule R5
    (FactorCerteza hace_intentos_arrancar si ?f1)
    (test (> ?f1 0))

    => 

    (assert (FactorCerteza problema_starter si (encadenado ?f1 -0.6))) ; DUDA hay que llamar a funcion?
    (assert (Motivo FactorCerteza problema_starter si (encadenado ?f1 -0.6) "porque hace intentos de arrancar"))
)

; R6: SI hace intentos de arrancar ENTONCES problema con la bateria 0.5
(defrule R6
    (FactorCerteza hace_intentos_arrancar si ?f1)
    (test (> ?f1 0))

    => 

    (assert (FactorCerteza problema_bateria si (encadenado ?f1 0.5))) ; DUDA hay que llamar a funcion?
    (assert (Motivo FactorCerteza problema_bateria si (encadenado ?f1 0.5) "porque hace intentos de arrancar"))
)


;;;;;;;;;;;;;;;; Tras razonar quedarse con las hipotesis con mayor certeza ;;;;;;;;;;;;;;;;
(defrule mayor_certeza
    (declare (salience -1))
    (FactorCerteza ?h1 ?r1 ?f1)
    ?x <- (FactorCerteza ?h2 ?r2 ?f2)
    (test
        (and
            (> ?f1 ?f2)
            (< ?f1 1)
        )
    )

    =>
    (printout t "Voy a eliminar que " ?r2 " ocurra que " ?h2 " porque tiene poca certeza (" ?f2 ")" crlf)
    (retract ?x)
    
)

;;;;;;;;;;;;;;;; Deducciones ;;;;;;;;;;;;;;;;
(defrule deducciones
    (declare (salience -2))
    ?x <- (FactorCerteza ?h ?r ?f)
    ?y <- (Motivo FactorCerteza ?h ?r ?f ?expl)
    
    =>
    (printout t "El factor deducido es que " ?r " ocurre que " ?h " con certeza " ?f " porque " ?expl crlf)
    (retract ?x ?y)
)