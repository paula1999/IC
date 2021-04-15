; Paula Villanueva Núñez

;;;;;;;;;;;;; PROPIEDADES USADAS ;;;;;;;;;;;;;

;;;; El sistema utiliza el gusto por las matemáticas, tomando valores de Si o No,
;;;; y se representa por 
;;;; (Gusta_matematicas Si | No)

;;;; El sistema utiliza el trabajo preferido, tomando valores de Docencia, Empresa Pública,
;;;; Empresa Privada o le da igual y se representa por
;;;; (Quiere_trabajar Docencia | Empresa_publica | Empresa_privada | Igual)

;;;; El sistema utiliza la calificación media obtenida, tomando valores de Alta, Media
;;;; o Baja, y se representa por 
;;;; (Calificacion_media Alta | Media | Baja)

;;;; El sistema utiliza el gusto por el hardware, tomando valores de Si o No,
;;;; y se representa por 
;;;; (Gusta_hardware Si | No)

;;;; El sistema utiliza si es trabajador, tomando valores de Mucho, Normal o Poco,
;;;; y se representa por 
;;;; (Es_trabajador Mucho | Normal | Poco)

;;;; El sistema utiliza el gusto por la programación, tomando valores de Si o No,
;;;; y se representa por 
;;;; (Gusta_programar Si | No)

;;;; El sistema utiliza la preferencia por la teoria o la practica, tomando valores de Teoricas o Practicas,
;;;; y se representa por 
;;;; (Tipo_clases Teoricas | Practicas)


; Hechos para representar las ramas
(deffacts Ramas
    (Rama Computacion_y_Sistemas_Inteligentes)
    (Rama Ingenieria_del_Software)
    (Rama Ingenieria_de_Computadores)
    (Rama Sistemas_de_Informacion)
    (Rama Tecnologias_de_la_Informacion)
)

; Hecho para representar que el sistema aconseja elegir una rama por un motivo
; (Consejo <nombre de la rama> "<texto del motivo>" "apodo del experto")

;;;;;;;; INICIO

(defrule inicio
    (declare (salience 9999))
    
    =>
    
    (printout t "Hola, te voy a asesorar sobre que rama elegir." crlf)
    (printout t "Para ello, te voy a hacer una serie de cuestiones y al final te dare el resultado." crlf)
    (focus ModuloPreguntas)
)

; Preguntas
(defmodule ModuloPreguntas (export ?ALL))

(deffacts Preguntas
    (Modulo MPreguntas)
    (pregunta matematicas)
    (pregunta trabajo)
    (pregunta nota)
    (pregunta hardware)
    (pregunta trabajador)
    (pregunta programacion)
    (pregunta tipoClases)
)

(defrule pMatematicas
    (Modulo MPreguntas)
    ?r <- (pregunta matematicas)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gustan las matematicas? (Si | No | NS)" crlf)
    (assert (comprobar matematicas (read)))
)

(defrule comprobarSiNoNS
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (comprobar ?pregunta ?respuesta)
    (test
        (or
            (and
                (neq (upcase ?respuesta) SI)
                (and
                    (neq (upcase ?respuesta) NO)
                    (neq (upcase ?respuesta) NS) 
                )
            )
            (neq (type ?respuesta) SYMBOL)
        )
    )

    =>

    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta ?pregunta))
)



(defrule pTrabajo
    ?r <- (pregunta trabajo)
    (Modulo MPreguntas)

    =>
    
    (retract ?r)
    (printout t "¿De que te gustaria trabajar? (Docencia | Empresa_publica | Empresa_privada | Igual | NS)" crlf)
    (assert (rTrabajo (read)))
)

(defrule comprobarTrabajo
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTrabajo ?tr)
    (test
        (and
            (neq (upcase ?tr) DOCENCIA)
            (and
                (neq (upcase ?tr) EMPRESA_PUBLICA)
                (and
                    (neq (upcase ?tr) EMPRESA_PRIVADA)
                    (and
                        (neq (upcase ?tr) IGUAL)
                        (neq (upcase ?tr) NS)
                    )
                )
            )
        )
    )

    =>
    
    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta trabajo))
)

(defrule pNota
    (Modulo MPreguntas)
    ?r <- (pregunta nota)
    
    =>
    
    (retract ?r)
    (printout t "¿Cual es tu calificacion media? (Numero | Alta | Media | Baja | NS)" crlf)
    (assert (rNota (read)))
)

(defrule comprobarNota
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; Comprobar si es alta, media o baja
    (test (eq (type ?nota) SYMBOL))
    (test 
        (and 
            (neq (upcase ?nota) ALTA) 
            (and
                (neq (upcase ?nota) MEDIA)
                (and
                    (neq (upcase ?nota) BAJA)
                    (neq (upcase ?nota) NS)
                )
            )
        )
    )

    =>
    
    (retract ?r)
    (printout t "ERROR. Introduce de nuevo una nota valida" crlf)
    (assert (pregunta nota))
)

(defrule comprobarNotaNumerica
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; Solo si es un numero
    (test 
        (or 
            (eq (type ?nota) INTEGER) 
            (eq (type ?nota) FLOAT)
        )
    )
    (test 
        (or
            (> ?nota 10.0) 
            (< ?nota 0.0)
        )
    )

    =>
    
    (retract ?r)
    (printout t "ERROR. Introduce de nuevo una nota valida" crlf)
    (assert (pregunta nota))
)

; Si la nota esta entre 0 y 6, la consideramos como nota Baja
(defrule ajustarNotaBaja
    (declare (salience 998))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; Solo si es un numero
    (test 
        (or 
            (eq (type ?nota) INTEGER) 
            (eq (type ?nota) FLOAT)
        )
    )
    (test 
        (and
            (> ?nota 0.0)
            (<= ?nota 6.0)
        )
    )

    =>

    (retract ?r)
    (assert (notaMedia Baja))
)

; Si la nota esta entre 6 y 8, la consideramos como nota Normal
(defrule ajustarNotaMedia
    (declare (salience 998))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; Solo si es un numero
    (test 
        (or 
            (eq (type ?nota) INTEGER) 
            (eq (type ?nota) FLOAT)
        )
    )
    (test 
        (and
            (> ?nota 6.0)
            (<= ?nota 8.0)
        )
    )

    =>

    (retract ?r)
    (assert (notaMedia Normal))
)

; Si la nota esta entre 8 y 10, la consideramos como nota Alta
(defrule ajustarNotaAlta
    (declare (salience 998))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; Solo si es un numero
    (test 
        (or 
            (eq (type ?nota) INTEGER) 
            (eq (type ?nota) FLOAT)
        )
    )
    (test 
        (and
            (> ?nota 8.0)
            (<= ?nota 10.0)
        )
    )

    =>
    
    (retract ?r)
    (assert (notaMedia Alta))
)

(defrule pHardware
    (Modulo MPreguntas)
    ?r <- (pregunta hardware)

    =>
    
    (retract ?r)
    (printout t "¿Te gusta el hardware? (Si | No | NS)" crlf)
    (assert (comprobar hardware (read)))
)

(defrule pTrabajador
    (Modulo MPreguntas)
    ?r <- (pregunta trabajador)
    
    =>
    
    (retract ?r)
    (printout t "¿Te consideras trabajador? (Mucho | Normal | Poco | NS)" crlf)
    (assert (rTrabajador (read)))
)

(defrule comprobarTrabajador
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTrabajador ?tr)
    (test
        (and
            (neq (upcase ?tr) MUCHO)
            (and
                (neq (upcase ?tr) NORMAL)
                (and
                    (neq (upcase ?tr) POCO)
                    (neq (upcase ?tr) NS)
                )
            )
        )
    )
    
    =>
    
    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta trabajador))
)


(defrule pProgramacion
    (Modulo MPreguntas)
    ?r <- (pregunta programacion)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gusta programar? (Si | No | NS)" crlf)
    (assert (comprobar programacion (read)))
)

(defrule pTipoClases
    (Modulo MPreguntas)
    ?r <- (pregunta tipoClases)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gustan las clases teoricas o practicas? (Teoricas | Practicas | NS)" crlf)
    (assert (rTipoClases (read)))
)

(defrule comprobarTipoClases
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTipoClases ?tc)
    (test
        (and
            (neq (upcase ?tc) TEORICAS)
            (and
                (neq (upcase ?tc) PRACTICAS)
                (neq (upcase ?tc) NS)
            )
        )
    )

    =>

    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta tipoClases))
)


; Fin de las preguntas

(defrule noHayMasPreguntas
    (declare (salience 100))
    ?m <- (Modulo MPreguntas)
    (comprobar matematicas ?r1)
    (rTrabajo ?r2)
    (rNota ?r3)
    (comprobar hardware ?r4)
    (rTrabajador ?r5)
    (comprobar programacion ?r6)
    (rTipoClases ?r7)

    =>

    (retract ?m)
    (focus ModuloCalcular)
    (assert (Modulo MCalcular))
)


;;;;;;;; CALCULO DE LA RAMA
(defmodule ModuloCalcular (export ?ALL) (import ModuloPreguntas ?ALL))




;;;;;;;; CONSEJO FINAL
(defmodule ModuloConsejo (import ModuloCalcular ?ALL))

(defrule fin
    (declare (salience 9999))
    (Consejo ?rama ?motivo ?experto)
    
    =>
    
    (printout t "El consejo del experto " ?experto " es que deberias escoger la rama " ?rama " porque " ?motivo crlf)
    
)