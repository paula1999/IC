; Paula Villanueva Nuñez

;;;;;;;;;;;;;; Representacion ;;;;;;;;;;;;;;
; (ave ?x) representa "?x es un ave"
; (animal ?x) representa "?x es un animal"
; (vuela ?x si|no seguro|por_defecto) representa "?x vuela si|no con esa certeza"


;;;;;;;;;;;;;; Hechos ;;;;;;;;;;;;;;
; Las aves y los mamíferos son animales
; Los gorriones, las palomas, las águilas y los pingüinos son aves
; La vaca, los perros y los caballos son mamíferos
; Los pingüinos no vuelan

(deffacts datos
    (ave gorrion)
    (ave paloma)
    (ave aguila)
    (ave pinguino)
    (mamifero vaca)
    (mamifero perro)
    (mamifero caballo)
    (vuelva pinguino no seguro)
)

;;;;;;;;;;;;;; Reglas seguras ;;;;;;;;;;;;;;

; Las aves son animales
(defrule aves_son_animales
    (ave ?x)
    =>
    (assert (animal ?x))
    (bind ?expl (str-cat "sabemos que un " ?x " es un animal porque la saves son un tipo de animal"))
    (assert (explicacion animal ?x ?expl))
)
; añadimos un hecho que contiene la explicacion de la deduccion


; Los mamiferos son animales (A3)
(defrule mamiferos_son_animales
    (mamifero ?x)
    =>
    (assert (animal ?x))
    (bind ?expl (str-cat "sabemos que un " ?x " es un animal porque los mamiferos son un tipo de animal"))
    (assert (explicacion animal ?x ?expl))
)
; añadimos un hecho que contiene la explicacion de la deduccion


;;;;;;;;;;;;;;;; Regla por defecto: añade ;;;;;;;;;;;;;;;;

;;; Casi todos las aves vuela --> puedo asumir por defecto que las aves vuelan

; Asumimos por defecto
(defrule ave_vuela_por_defecto
    (declare (salience -1)) ; para disminuir probabilidad de añadir erroneamente
    (ave ?x)
    =>
    (assert (vuela ?x si por_defecto))
    (bind ?expl (str-cat "asumo que un " ?x " vuela, porque casi todas las aves vuelan"))
    (assert (explicacion vuela ?x ?expl))
)

;;;;;;;;;;;;;;;; Regla por defecto: retracta ;;;;;;;;;;;;;;;;

; Retractamos cuando hay algo en contra
(defrule retracta_vuela_por_defecto
    (declare (salience 1))  ; para retractar antes de inferir cosas erroneamente
    ?f <- (vuela ?x ?r por_defecto)
    (vuela ?x ?s seguro)
    =>
    (retract ?f)
    (bind ?expl (str-cat "retractamos que un " ?x ?r " vuela por defecto, porque sabemos seguro que " ?x ?s " vuela"))
    (assert (explicacion retracta_vuela ?x ?expl))
)
;;; COMENTARIO: esta regla tambien elimina los por defecto cuando ya esta seguro


;;;;;;;;;;;;;;;; Regla por defecto para razonar con informacion incompleta ;;;;;;;;;;;;;;;;
;;; La mayor parte de los animales no vuelan --> puede interesarme asumir por defecto que un animal no va a volar

(defrule mayor_parte_animales_no_vuelan
    (declare (salience -2))  ; es mas arriesgado, mejor despues de otros razonamientos
    (animal ?x)
    (not (vuela ?x ? ?))
    =>
    (assert (vuela ?x no por_defecto))
    (bind ?expl (str-cat "asumo que " ?x " no vuela, porque la mayor parte de los animales no vuelan"))
    (assert (explicacion vuela ?x ?expl))
)


;;;;;;;;;;;;;;;; Ejercicio ;;;;;;;;;;;;;;;;
; Completar esta base de conocimiento para que el sistema pregunte
; que de qué animal esta interesado en obtener información sobre si
; vuela y:
;;;;;;; - Si es uno de los recogidos en el conocimiento indique si vuela o no
;;;;;;; - Si no es uno de los recogidos pregunte si es un ave o un mamífero y
;;;;;;;  según la respuesta indique si vuela o no.
;;;;;;; - Si no se sabe si es un mamífero o un ave también responda según el
;;;;;;;  razonamiento por defecto indicado


; Preguntar el animal
(defrule pregunta_animal
    (declare (salience -3))
    (not (preguntado ?))
    (not (respondido SI))
    =>
    (printout t "Dime un animal para obtener informacion sobre si vuela" crlf)
    (bind ?animal (read))
    (assert (preguntado ?animal))
)

; Si es uno de los recogidos en el conocimiento indique si vuela o no
(defrule animal_recogido
    (declare (salience -4))
    (preguntado ?animal)
    (animal ?animal)
    (vuela ?resp ?x ?certeza)
    (explicacion vuela ?animal ?expl)
    (not (respondido SI))
    =>
    (assert (respondido SI))
    (printout t "El animal " ?animal " " ?x " vuela porque " ?expl crlf)
)


; Si no es uno de los recogidos pregunte si es un ave o un mamífero y según la respuesta indique si vuela o no
(defrule animal_no_recogido
    (declare (salience -5))
    (preguntado ?animal)
    (not (animal ?animal))
    (not (tipo ?animal ?t))
    =>
    (printout t "Dime si el animal " ?animal " es un ave o un mamifero [ave | mamifero]" crlf)
    (bind ?respuesta (read))
    (assert (tipo ?animal ?respuesta))
)

; Si no es uno de los recogidos pero es un ave
(defrule animal_ave
    (declare (salience -6))
    ?f <- (tipo ?animal ?t)
    (test (eq ave ?t))
    =>
    (retract ?f)
    (assert (ave ?animal))
)

; Si no es uno de los recogidos pero es un mamifero
(defrule animal_mamifero
    (declare (salience -6))
    ?f <- (tipo ?animal ?t)
    (test (eq mamifero ?t))
    =>
    (retract ?f)
    (assert (mamifero ?animal))
)

; Comprobar que la entrada sea valida
(defrule comprobar_entrada
    (declare (salience 999))
    ?f <- (tipo ?animal ?t)
    (test
        (and
            (neq ?t ave)
            (neq ?t mamifero)
        )
    )
    =>
    (retract ?f)
    (printout t "Perdona pero no he entendido su respuesta, por favor introduzca una respuesta valida [ave | mamifero]" crlf)
)