; Paula Villanueva Núñez

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROPIEDADES USADAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; El sistema utiliza el gusto por las matemáticas, tomando valores de Si, No o no se,
;;;; y se representa por 
;;;; (respuesta matematicas SI | NO | NS)

;;;; El sistema utiliza el trabajo preferido, tomando valores de Docencia, Empresa Pública,
;;;; Empresa Privada, le da igual o no se y se representa por
;;;; (rTrabajo DOCENCIA | EMPRESA_PUBLICA | EMPRESA_PRIVADA | IGUAL | NS)

;;;; El sistema utiliza la calificación media obtenida, tomando valores de Alta, Media,
;;;; Baja o no se y se representa por 
;;;; (rNota ALTA | MEDIA | BAJA | NS)

;;;; El sistema utiliza el gusto por el hardware, tomando valores de Si, No o no se
;;;; y se representa por 
;;;; (respuesta hardware SI | NO | NS)

;;;; El sistema utiliza si es trabajador, tomando valores de Mucho, Normal, Poco o no se,
;;;; y se representa por 
;;;; (rTrabajador MUCHO | NORMAL | POCO | NS)

;;;; El sistema utiliza el gusto por la programación, tomando valores de Si, No o no se,
;;;; y se representa por 
;;;; (respuesta programacion SI | NO | NS)

;;;; El sistema utiliza la preferencia por la teoria o la practica, tomando valores de Teoricas, Practicas o no se,
;;;; y se representa por 
;;;; (rTipoClases TEORICAS | PRACTICAS | NS)

; Hecho para representar que el sistema aconseja elegir una rama por un motivo
; (Consejo <nombre de la rama> "<texto del motivo>" "apodo del experto")

; Hechos para representar las ramas
(deffacts Ramas
    (Rama Computacion_y_Sistemas_Inteligentes)
    (Rama Ingenieria_del_Software)
    (Rama Ingenieria_de_Computadores)
    (Rama Sistemas_de_Informacion)
    (Rama Tecnologias_de_la_Informacion)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mensaje de inicio del programa
(defrule inicio
    (declare (salience 9999))
    
    =>
    
    (printout t "Hola, te voy a asesorar sobre que rama elegir." crlf)
    (printout t "Para ello, te voy a hacer una serie de cuestiones y al final te dare el resultado." crlf)
    (printout t "Si en algun momento no te apetece seguir respondiendo a mis preguntas, escribe PARAR." crlf)
    (focus ModuloPreguntas)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloPreguntas (export ?ALL))

; Hecho para representar las preguntas
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

; Comprueba que la entrada de datos sea valida (Si | No | NS)
(defrule respuestaSiNoNS
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (respuesta ?pregunta ?resp)
    (test
        (or
            (and
                (neq ?resp SI)
                (and
                    (neq ?resp NO)
                    (neq ?resp NS) 
                )
            )
            (neq (type ?resp) SYMBOL)
        )
    )

    =>

    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta ?pregunta))
)

;;;;;;;;;;;;;;;;;;;;;;;; MATEMATICAS ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su gusto por las matematicas
(defrule pMatematicas
    (Modulo MPreguntas)
    ?r <- (pregunta matematicas)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gustan las matematicas? (Si | No | NS)" crlf)
    (assert (respuesta matematicas (upcase (read))))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararR
    (declare (salience 9999))
    ?r <- (respuesta ?pregunta ?resp)
    (test (eq ?resp PARAR))

    =>

    (retract ?r)
    (printout t "Vale, no te hare mas preguntas...")
    (assert (rParar SI))
)

;;;;;;;;;;;;;;;;;;;;;;;; TRABAJO ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su gusto por el trabajo preferido
(defrule pTrabajo
    ?r <- (pregunta trabajo)
    (Modulo MPreguntas)

    =>
    
    (retract ?r)
    (printout t "¿De que te gustaria trabajar? (Docencia | Empresa_publica | Empresa_privada | Igual | NS)" crlf)
    (assert (rTrabajo (upcase (read))))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararTrabajo
    (declare (salience 9999))
    ?r <- (rTrabajo ?tr)
    (test (eq ?tr PARAR))

    =>

    (retract ?r)
    (printout t "Vale, no te hare mas preguntas...")
    (assert (rParar SI))
)

; Comprueba que la entrada de datos sea valida
(defrule respuestaTrabajo
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTrabajo ?tr)
    (test
        (and
            (neq ?tr DOCENCIA)
            (and
                (neq ?tr EMPRESA_PUBLICA)
                (and
                    (neq ?tr EMPRESA_PRIVADA)
                    (and
                        (neq ?tr IGUAL)
                        (neq ?tr NS)
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

;;;;;;;;;;;;;;;;;;;;;;;; CALIFICACIONES ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su calificacion media
(defrule pNota
    (Modulo MPreguntas)
    ?r <- (pregunta nota)
    
    =>
    
    (retract ?r)
    (printout t "¿Cual es tu calificacion media? (Numero | Alta | Media | Baja | NS)" crlf)
    (assert (rNota (read)))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararNota
    (declare (salience 9999))
    ?r <- (rNota ?nota)
    (test (eq (type ?nota) SYMBOL))
    (test (eq (upcase ?nota) PARAR))

    =>

    (retract ?r)
    (printout t "Vale, no te hare mas preguntas...")
    (assert (rParar SI))
)

; Comprueba que la entrada de datos sea valida
(defrule respuestaNota
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
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

; Comprueba que la entrada de datos sea valida y la convierte a mayusculas
(defrule ajustarNotaMayusculas
    (declare (salience 998))
    (Modulo MPreguntas)
    ?r <- (rNota ?nota)
    ; respuesta si es alta, media o baja
    (test (eq (type ?nota) SYMBOL))
    (test 
        (or 
            (eq (upcase ?nota) ALTA) 
            (or
                (eq (upcase ?nota) MEDIA)
                (or
                    (eq (upcase ?nota) BAJA)
                    (eq (upcase ?nota) NS)
                )
            )
        )
    )

    =>
    
    (assert (rNota (upcase ?nota) ))
)

; Comprueba que la entrada de datos sea valida (NUMERICA)
(defrule respuestaNotaNumerica
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
    (assert (rNota BAJA))
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
    (assert (rNota NORMAL))
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
    (assert (rNota ALTA))
)

;;;;;;;;;;;;;;;;;;;;;;;; HARDWARE ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su gusto por el hardware
(defrule pHardware
    (Modulo MPreguntas)
    ?r <- (pregunta hardware)

    =>
    
    (retract ?r)
    (printout t "¿Te gusta el hardware? (Si | No | NS)" crlf)
    (assert (respuesta hardware (upcase (read))))
)

;;;;;;;;;;;;;;;;;;;;;;;; TRABAJADOR ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre si se considera trabajador
(defrule pTrabajador
    (Modulo MPreguntas)
    ?r <- (pregunta trabajador)
    
    =>
    
    (retract ?r)
    (printout t "¿Te consideras trabajador? (Mucho | Normal | Poco | NS)" crlf)
    (assert (rTrabajador (upcase (read))))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararTrabajador
    (declare (salience 9999))
    ?r <- (rTrabajador ?tr)
    (test (eq ?tr PARAR))

    =>

    (retract ?r)
    (printout t "Vale, no te hare mas preguntas...")
    (assert (rParar SI))
)


; Comprueba que la entrada de datos sea valida
(defrule respuestaTrabajador
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTrabajador ?tr)
    (test
        (and
            (neq  ?tr MUCHO)
            (and
                (neq ?tr NORMAL)
                (and
                    (neq ?tr POCO)
                    (neq ?tr NS)
                )
            )
        )
    )
    
    =>
    
    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta trabajador))
)

;;;;;;;;;;;;;;;;;;;;;;;; PROGRAMACION ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su gusto por las programacion
(defrule pProgramacion
    (Modulo MPreguntas)
    ?r <- (pregunta programacion)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gusta programar? (Si | No | NS)" crlf)
    (assert (respuesta programacion (upcase (read))))
)

;;;;;;;;;;;;;;;;;;;;;;;; TIPO DE CLASES ;;;;;;;;;;;;;;;;;;;;;;;;

; Preguntas para obtener informacion sobre su gusto por el tipo de las clases
(defrule pTipoClases
    (Modulo MPreguntas)
    ?r <- (pregunta tipoClases)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gustan las clases teoricas o practicas? (Teoricas | Practicas | NS)" crlf)
    (assert (rTipoClases (upcase (read))))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararTipoClases
    (declare (salience 9999))
    ?r <- (rTipoClases ?tr)
    (test (eq ?tr PARAR))

    =>

    (retract ?r)
    (printout t "Vale, no te hare mas preguntas...")
    (assert (rParar SI))
)

; Comprueba que la entrada de datos sea valida
(defrule respuestaTipoClases
    (declare (salience 999))
    (Modulo MPreguntas)
    ?r <- (rTipoClases ?tc)
    (test
        (and
            (neq ?tc TEORICAS)
            (and
                (neq ?tc PRACTICAS)
                (neq ?tc NS)
            )
        )
    )

    =>

    (retract ?r)
    (printout t "ERROR. Introduce de nuevo la respuesta." crlf)
    (assert (pregunta tipoClases))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fin de las preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Comprueba si no quedan mas preguntas por responder
(defrule noHayMasPreguntas
    (declare (salience 100))
    ?m <- (Modulo MPreguntas)
    (respuesta matematicas ?r1)
    (rTrabajo ?r2)
    (rNota ?r3)
    (respuesta hardware ?r4)
    (rTrabajador ?r5)
    (respuesta programacion ?r6)
    (rTipoClases ?r7)

    =>

    (retract ?m)
    (focus ModuloCalcular)
    (assert (Modulo MCalcular))
)

; Comprueba si el usuario quiere parar
(defrule pararPreguntas
    (declare (salience 9999))
    ?m <- (Modulo MPreguntas)
    (rParar SI)

    =>

    (retract ?m)
    (focus ModuloCalcular)
    (assert (Modulo MCalcular))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CALCULO DE LA RAMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloCalcular (export ?ALL) (import ModuloPreguntas ?ALL))

;;;;;;;;;;;;;;;;;;;;;;;; CSI ;;;;;;;;;;;;;;;;;;;;;;;;

; Aconseja la rama CSI por gustarle las matematicas y ser trabajador
(defrule aconsejarCSI_matematicas_trabajador
    (declare (salience 100))
    (Modulo MCalcular)
    (respuesta hardware NO)
    (rTrabajador MUCHO)
    (respuesta matematicas SI)

    =>

    (assert (Rama Computacion_y_Sistemas_Inteligentes))
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "esta rama requiere mucho trabajo y tambien esta muy relacionada con las matematicas. Ademas, tambien puedes dedicarte al trabajo que te gusta." "Paula"))
)

; Aconseja la rama CSI por gustarle las matematicas y la programacion
(defrule aconsejarCSI_matematicas_programacion
    (declare (salience 100))
    (Modulo MCalcular)
    (rTrabajo IGUAL)
    (rNota MEDIA)
    (respuesta matematicas SI)
    (respuesta programacion SI)

    =>

    (assert (Rama Computacion_y_Sistemas_Inteligentes))
    (assert (Consejo Computacion_y_Sistemas_Inteligentes "en esta rama se utilizan las matematicas y tambien disfrutaras programando. Ademas, con tus buenas calificaciones no tendras mucho problema." "Paula"))
)

;;;;;;;;;;;;;;;;;;;;;;;; IS ;;;;;;;;;;;;;;;;;;;;;;;;

; Aconseja la rama IS por gustarle la programacion y ser trabajador
(defrule aconsejarIS_trabajador_programacion
    (declare (salience 100))
    (Modulo MCalcular)
    (respuesta hardware NO)
    (rTrabajo DOCENCIA)
    (rTrabajador NORMAL)
    (respuesta programacion SI)
    (rNota ?n)
    (test
        (or 
            (eq ?n MEDIA)
            (eq ?n ALTA)
        )
    )
    =>

    (assert (Rama Ingenieria_del_Software))
    (assert (Consejo Ingenieria_del_Software "esta rama requiere trabajo y si te gusta programar, aqui lo podras disfrutar. Ademas, con tus buenas calificaciones no tendras mucho problema y puede estar relacionada con el trabajo que te gusta." "Paula"))
)

; Aconseja la rama IS por no gustarle las matematicas y ser trabajador
(defrule aconsejarIS_trabajador_matematicas
    (declare (salience 100))
    (Modulo MCalcular)
    (respuesta matematicas NO)
    (rTrabajador NORMAL)

    =>

    (assert (Rama Ingenieria_del_Software))
    (assert (Consejo Ingenieria_del_Software "esta rama no requiere demasiadas matematicas, aunque si necesitas trabajar. Ademas, puede estar relacionada con el trabajo que te gusta." "Paula"))
)

;;;;;;;;;;;;;;;;;;;;;;;; SI ;;;;;;;;;;;;;;;;;;;;;;;;

; Aconseja la rama SI por no gustarle el hardware y ser poco trabajador
(defrule aconsejarSI_trabajador
    (declare (salience 100))
    (Modulo MCalcular)
    (respuesta hardware NO)
    (rTrabajador POCO)

    =>

    (assert (Rama Sistemas_de_Informacion))
    (assert (Consejo Sistemas_de_Informacion "esta rama requiere poco trabajo y puede estar relacionada con el trabajo que te gusta." "Paula"))
)

;;;;;;;;;;;;;;;;;;;;;;;; TI ;;;;;;;;;;;;;;;;;;;;;;;;

; Aconseja la rama TI por gustarle la programacion
(defrule aconsejarTI_programacion
    (declare (salience 100))
    (Modulo MCalcular)
    (respuesta programacion SI)

    =>

    (assert (Rama Tecnologias_de_la_Informacion))
    (assert (Consejo Tecnologias_de_la_Informacion "en esta rama vas a poder programar y hay muchas clases practicas. Ademas, puede estar relacionada con el trabajo que te gusta." "Paula"))
)

; Aconseja la rama TI por tener calificaciones bajas
(defrule aconsejarTI_nota
    (declare (salience 90))
    (Modulo MCalcular)
    (rNota BAJA)

    =>

    (assert (Rama Tecnologias_de_la_Informacion))
    (assert (Consejo Tecnologias_de_la_Informacion "si te da igual en que trabajar y no tienes muy buenas calificaciones, lo mejor es que elijas esta rama para que no te cueste mucho esfuerzo aprobar." "Paula"))
)

; Aconseja la rama TI por no gustarle el hardware
(defrule aconsejarTI_hardware
    (declare (salience 90))
    (Modulo MCalcular)
    (respuesta hardware NO)

    =>

    (assert (Rama Tecnologias_de_la_Informacion))
    (assert (Consejo Tecnologias_de_la_Informacion "no te gusta el hardware, aunque no puedo decirtelo con certeza pues no me has proporcionado demasiada informacion, aunque suele ser la opcion mas segura" "Paula"))
)

; Aconseja la rama TI por falta de informacion
(defrule aconsejarTI_noInfo
    (declare (salience 50))
    (Modulo MCalcular)

    =>

    (assert (Rama Tecnologias_de_la_Informacion))
    (assert (Consejo Tecnologias_de_la_Informacion "no me has proporcionado demasiada informacion y no puedo decirtelo con certeza, aunque y suele ser la opcion mas segura" "Paula"))
)

;;;;;;;;;;;;;;;;;;;;;;;; IC ;;;;;;;;;;;;;;;;;;;;;;;;

; Aconseja la rama IC por gustarle el hardware
(defrule aconsejarIC_hardware
    (declare (salience 101))
    (Modulo MCalcular)
    (respuesta hardware SI)

    =>

    (assert (Rama Ingenieria_de_Computadores))
    (assert (Consejo Ingenieria_de_Computadores "es la mejor opcion si te gusta el hardware." "Paula"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fin de calcular la rama ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Terminar si ya se ha aconsejado
(defrule noHayMasCalculos
    (declare (salience 200))
    ?m <- (Modulo MCalcular)
    (Rama ?rama)
    (Consejo ?rama ?motivo ?experto)

    =>

    (retract ?m)
    (focus ModuloConsejo)
    (assert (Modulo MConsejo))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSEJO FINAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloConsejo (import ModuloCalcular ?ALL))

; Mostrar el consejo final
(defrule fin
    (declare (salience 9999))
    (Modulo MConsejo)
    (Rama ?rama)
    (Consejo ?rama ?motivo ?experto)
    
    =>
    
    (printout t "El consejo del experto " ?experto " es que deberias escoger la rama " ?rama " porque " ?motivo crlf)
)