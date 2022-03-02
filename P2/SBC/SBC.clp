; Paula Villanueva Núñez

; Mensaje de inicio del programa
(defrule inicio
    (declare (salience 9999))
    
    =>
    
    (printout t "Hola, ¿quieres que te asesore sobre la rama a elegir o sobre dos asignaturas? (1=rama, 2=asignaturas)." crlf)
    (bind ?respuesta (read))
    (if (= ?respuesta 1) then (focus ModuloRama)
    else (if (= ?respuesta 2) then (focus ModuloAsignatura)
        else (printout t "Hasta luego." crlf) ))
    (printout t crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACONSEJAR RAMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmodule ModuloRama (export ?ALL))

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
;;;; (rtipoClases TEORICAS | PRACTICAS | NS)

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
(defrule inicioRama
    (declare (salience 9999))
    
    =>
    
    (printout t "Vale, te voy a asesorar sobre que rama elegir." crlf)
    (printout t "Para ello, te voy a hacer una serie de cuestiones y al final te dare el resultado." crlf)
    (printout t "Si en algun momento no te apetece seguir respondiendo a mis preguntas, escribe PARAR." crlf)
    (focus ModuloPreguntasRama)
    (assert (Modulo MPreguntas))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloPreguntasRama (export ?ALL))

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
    (printout t "ERROR. Introduce de nuevo una nota valida" ?nota  (type ?nota) crlf)
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

; Si la nota esta entre 6 y 8, la consideramos como nota Media
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
    (assert (rNota MEDIA))
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
(defrule ptipoClases
    (Modulo MPreguntas)
    ?r <- (pregunta tipoClases)
    
    =>
    
    (retract ?r)
    (printout t "¿Te gustan las clases teoricas o practicas? (Teoricas | Practicas | NS)" crlf)
    (assert (rtipoClases (upcase (read))))
)

; Comprueba si el usuario quiere parar
(defrule comprobarParartipoClases
    (declare (salience 9999))
    ?r <- (rtipoClases ?tr)
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
    ?r <- (rtipoClases ?tc)
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
    (rtipoClases ?r7)

    =>

    (retract ?m)
    (focus ModuloCalcularRama)
    (assert (Modulo MCalcular))
)

; Comprueba si el usuario quiere parar
(defrule pararPreguntas
    (declare (salience 9999))
    ?m <- (Modulo MPreguntas)
    (rParar SI)

    =>

    (retract ?m)
    (focus ModuloCalcularRama)
    (assert (Modulo MCalcular))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CALCULO DE LA RAMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloCalcularRama (export ?ALL) (import ModuloPreguntasRama ?ALL))

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
    (focus ModuloConsejoRama)
    (assert (Modulo MConsejo))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSEJO FINAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloConsejoRama (import ModuloCalcularRama ?ALL))

; Mostrar el consejo final
(defrule fin
    (declare (salience 9999))
    (Modulo MConsejo)
    (Rama ?rama)
    (Consejo ?rama ?motivo ?experto)
    
    =>
    
    (printout t "El consejo del experto " ?experto " es que deberias escoger la rama " ?rama " porque " ?motivo crlf)
)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACONSEJAR ASIGNATURA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmodule ModuloAsignatura (export ?ALL))

; Asignaturas
(deffacts Asignaturas
    ; Primero
    (Asignatura B ALEM "Algebra lineal y estructuras matematicas")
    (Asignatura B CAL "Calculo")
    (Asignatura B FS "Fundamentos del software")
    (Asignatura B FP "Fundamentos de programacion")
    (Asignatura B FFT "Fundamentos fisicos y tecnologicos")
    (Asignatura B EST "Estadistica")
    (Asignatura B IES "Ingenieria, empresa y sociedad")
    (Asignatura B LMD "Logica y metodos discretos")
    (Asignatura B MP "Metodologia de la programacion")
    (Asignatura B TOC "Tecnologia y organizacion de computadores")
    ; Segundo
    (Asignatura O EC "Estructura de Computadores")
    (Asignatura O ED "Estructuras de datos")
    (Asignatura O PDOO "Programacion y diseño orientado a objetos")
    (Asignatura O SCD "Sistemas concurrentes y distribuidos")
    (Asignatura O SO "Sistemas operativos")
    (Asignatura O ALG "Algoritmica")
    (Asignatura O AC "Arquitectura de computadores")
    (Asignatura O FBD "Fundamentos de bases de datos")
    (Asignatura O FIS "Fundamentos de ingenieria del software")
    (Asignatura O IA "Inteligencia artificial")
    ; Tercero
    (Asignatura O DDSI "Diseño y desarrollo de sistemas de informacion")
    (Asignatura O FR "Fundamentos de redes")
    (Asignatura O IG "Informatica grafica")
    (Asignatura O ISE "Ingenieria de servidores")
    (Asignatura O MC "Modelos de computacion")
    ; CSI
    (Asignatura CSI AA "Aprendizaje automatico")
    (Asignatura CSI IC "Ingenieria del conocimiento")
    (Asignatura CSI MH "Metaheuristicas")
    (Asignatura CSI MAC "Modelos avanzados de computacion")
    (Asignatura CSI TSI "Tecnicas de los sistemas inteligentes")
    ; IS
    (Asignatura IS DSD "Desarrollo de sistemas distribuidos")
    (Asignatura IS DS "Desarrollo de software")
    (Asignatura IS DIU "Diseño de interfaces de usuario")
    (Asignatura IS SIBW "Sistemas de informacion basados en web")
    (Asignatura IS SG "Sistemas graficos")
    ; IC
    (Asignatura IC AS "Arquitectura de sistemas")
    (Asignatura IC ACAP "Arquitectura y computacion de altas prestaciones")
    (Asignatura IC DHD "Desarrollo de hardware digital")
    (Asignatura IC DSE "Diseño de sistemas electronicos")
    (Asignatura IC SCM "Sistemas con microprocesadores")
    ; SI
    (Asignatura SI ABD "Administracion de bases de datos")
    (Asignatura SI ISI "Ingenieria de sistemas de informacion")
    (Asignatura SI PW "Programacion web")
    (Asignatura SI SIE "Sistemas de informacion para empresas")
    (Asignatura SI SM "Sistemas multidimensionales")
    ; TI
    (Asignatura TI CUIA "Computacion ubicua e inteligencia ambiental")
    (Asignatura TI SWAP "Servidores web de altas prestaciones")
    (Asignatura TI SMM "Sistemas multimedia")
    (Asignatura TI TW "Tecnologias web")
    (Asignatura TI TDRC "Transmision de datos y redes de computadores")
    ; Cuarto
    ; CSI
    (Asignatura CSI NPI "Nuevos paradigmas de interaccion")
    (Asignatura CSI PL "Procesadores de lenguajes")
    (Asignatura CSI VC "Vision por computador")
    ; IS
    (Asignatura IS DBA "Desarrollo basado en agentes")
    (Asignatura IS DGP "Direccion y gestion de proyectos")
    (Asignatura IS MDA "Metodologias de desarrollo agiles")
    ; IC
    (Asignatura IC CPD "Centros de procesamiento de datos")
    (Asignatura IC SE "Sistemas empotrados")
    (Asignatura IC TR "Tecnologias de red")
    ; SI
    (Asignatura SI BDD "Bases de datos distribuidas")
    (Asignatura SI IN "Inteligencia de negocio")
    (Asignatura SI RI "Recuperacion de la informacion")
    ; TI
    (Asignatura TI DAI "Desarrollo de aplicaciones para internet")
    (Asignatura TI IV "Infraestructura virtual")
    (Asignatura TI SPSI "Seguridad y proteccion de sistemas informaticos")
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Mensaje de inicio del programa
(defrule inicioAsignatura
    (declare (salience 9999))
    
    =>
    
    (printout t "Vale, te voy a asesorar sobre que asignatura elegir." crlf)
    (printout t "Para ello, te voy a hacer una serie de cuestiones y al final te dare el resultado." crlf)
    (printout t "Si en algun momento no te apetece seguir respondiendo a mis preguntas, escribe PARAR." crlf)
    (focus ModuloPreguntasAsignatura)
    (assert (Modulo MPreguntas))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preguntas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloPreguntasAsignatura (export ?ALL) (import ModuloAsignatura ?ALL))


; Añado factores de certeza

;;;;;;;;;;;;;;;; Representacion ;;;;;;;;;;;;;;;;

; (FactorCerteza ?h si|no ?f) representa que ?h se ha deducido con factor de certeza ?f
; ?h

; (Evidencia ?e si|no) representa el hecho de si evidencia ?e se da
; ?e podra ser
;   - gusta_matematicas
;   - gusta_programacion
;   - prefiere_poca_carga_trabajo
;   - gusta_hardware
;   - calificaciones_altas
;   - calificaciones_medias
;   - calificaciones_bajas
;   - gusta_alto_nivel
;   - gusta_bajo_nivel
;   - tipo_basica
;   - tipo_obligatoria
;   - tipo_optativa

; (Motivo FactorCerteza ?h si|no ?f ?explicacion) representa la explicacion del factor


;;;;;;;;;;;;;;;; Reglas ;;;;;;;;;;;;;;;;



; Convertimos cada evidencia en una afirmacion sobre su factor de certeza
(defrule certeza_evidencias
    (declare (salience 999))
    (Modulo MPreguntas)
    (Evidencia ?e ?r)
    =>
    (assert (FactorCerteza ?e ?r 1))
    (assert (Motivo FactorCerteza ?e ?r 1 "me lo has dicho"))
)
; Tambien podriamos considerar evidencias con una cierta incertidumbre:
; al preguntar por la evidencia, pedir y recoger directamente el grado de certeza

; Convertimos el tipo de asignatura en una afirmacion sobre su factor de certeza
(defrule certeza_tipo_B
    (declare (salience 999))
    (Modulo MPreguntas)
    (AsignaturaR B ? ?)
    =>
    (assert (FactorCerteza tipo_basica si 1))
    (assert (FactorCerteza tipo_obligatoria no 1))
    (assert (FactorCerteza tipo_optativa no 1))
    (assert (Motivo FactorCerteza tipo_basica si 1 "es basica"))
)

(defrule certeza_tipo_O
    (declare (salience 999))
    (Modulo MPreguntas)
    (AsignaturaR O ? ?)
    =>
    (assert (FactorCerteza tipo_basica no 1))
    (assert (FactorCerteza tipo_obligatoria si 1))
    (assert (FactorCerteza tipo_optativa no 1))
    (assert (Motivo FactorCerteza tipo_basica si 1 "es obligatoria"))
)

(defrule certeza_tipo_optativa
    (declare (salience 999))
    (Modulo MPreguntas)
    (AsignaturaR CSI|IC|IS|SI|TI ? ?)
    =>
    (assert (FactorCerteza tipo_basica no 1))
    (assert (FactorCerteza tipo_obligatoria no 1))
    (assert (FactorCerteza tipo_optativa si 1))
    (assert (Motivo FactorCerteza tipo_basica si 1 "es optativa"))
)



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
    (Modulo MPreguntas)
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
    (Modulo MPreguntas)
    (FactorCerteza ?h si ?fc1)
    (FactorCerteza ?h no ?fc2)
    =>
    (assert (Certeza ?h (- ?fc1 ?fc2)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;; Eleccion de las dos asignaturas

; Introducir dos asignaturas
(defrule elegirAsignaturas
    (declare (salience 9999))
    (Modulo MPreguntas)
    (not (asig1 ?a1))
    (not (asig2 ?a2))

    =>

    (printout t "Elige dos asignaturas de la siguiente lista (pulsa ENTER para introducirlas por separado)" crlf)
    (printout t "Asignaturas de primero: [ALEM/CAL/FS/FP/FFT/IES/EST/LMD/MP/TOC] " crlf)
    (printout t "Asignaturas de segundo: [ED/EC/PDOO/SCD/SO/ALG/AC/FBD/IA]" crlf)
    (printout t "Asignaturas de tercero: [DDSI/FR/IG/ISE/MC/AA/IC/MH/MAC/TSI/DSD/DS/DIU/SIBW/SG/AS/ACAP/DHD/DSE/SCM/ABD/ISI/PW/SIE/SM/CUIA/SWAP/SMM/TW/TDRC]" crlf)
    (printout t "Asignaturas de cuarto: [NPI/PL/VC/DBA/DGP/MDA/CPD/SE/TR/BDD/IN/RI/DAI/IV/SPSI]" crlf)
    (assert (asig1 (upcase (read))))
    (assert (asig2 (upcase (read))))
)

; Validar que la entrada es correcta
(defrule comprobarAsignatura
    (declare (salience 9998))
    (Modulo MPreguntas)
    ?f <- (asig1 ?asig1)
    ?g <- (asig2 ?asig2)
    (or (test (eq ?asig1 ?asig2)) (or (not (Asignatura ? ?asig1 ?)) (not (Asignatura ? ?asig2 ?))))
    
    =>

    (retract ?f ?g)
    (printout t "Error. Introduce de nuevo dos asignaturas de la lista, por favor." crlf)
)

; Si las asignaturas son correctas
(defrule asignarAsignaturas
    (declare (salience 9997))
    (Modulo MPreguntas)
    (asig1 ?asig1)
    (asig2 ?asig2)
    (test (neq ?asig1 ?asig2))
    (Asignatura ?o1 ?asig1 ?n1)
    (Asignatura ?o2 ?asig2 ?n2)

    =>

    (assert (AsignaturaR ?o1 ?asig1 ?n1))
    (assert (AsignaturaR ?o2 ?asig2 ?n2))
)


;;;;;;;;;;;;;;;; Preguntar por las posibles evidencias ;;;;;;;;;;;;;;;;

(defrule preguntar_gusta_matematicas
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia gusta_matematicas ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))
    =>

    (printout t "¿Te gustan las matematicas? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gusta_matematicas (lowcase ?respuesta)))
)

(defrule preguntar_gusta_programacion
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia gusta_programacion ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Te gusta la programacion? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gusta_programacion (lowcase ?respuesta)))
)

(defrule preguntar_prefiere_poca_carga_trabajo
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia prefiere_poca_carga_trabajo ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Prefieres tener poca carga de trabajo? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia prefiere_poca_carga_trabajo (lowcase ?respuesta)))
)

(defrule preguntar_calificaciones
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia calificaciones_bajas ?r))
    (not (Evidencia calificaciones_altas ?r))
    (not (Evidencia calificaciones_medias ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Que calificaciones tienes? (1=altas | 2=medias | 3=bajas | 4=NS)" crlf)
    (bind ?respuesta (read))

    (if (= ?respuesta 1) 
        then 
        (assert (Evidencia calificaciones_altas si))
        (assert (Evidencia calificaciones_medias no))
        (assert (Evidencia calificaciones_bajas no))
    else 
        (if (= ?respuesta 2) 
            then 
            (assert (Evidencia calificaciones_altas no))
            (assert (Evidencia calificaciones_medias si))
            (assert (Evidencia calificaciones_bajas no))
        else
            (if (= ?respuesta 3)
                then 
                (assert (Evidencia calificaciones_altas no))
                (assert (Evidencia calificaciones_medias no))
                (assert (Evidencia calificaciones_bajas si))
            else
                (if (= ?respuesta 4)
                    then
                    (assert (Evidencia calificaciones_altas ns))
                    (assert (Evidencia calificaciones_medias ns))
                    (assert (Evidencia calificaciones_bajas ns))
                else 
                    (printout t "Error. No he podido entender tu respuesta, introducela de nuevo." crlf)
                )
            )
        )
    )
)

(defrule preguntar_gusta_hardware
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia gusta_hardware ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Te gusta el hardware? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gusta_hardware (lowcase ?respuesta)))
)

(defrule preguntar_gusta_alto_nivel
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia gusta_alto_nivel ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Te gusta el alto nivel? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gusta_alto_nivel (lowcase ?respuesta)))
)

(defrule preguntar_gusta_bajo_nivel
    (declare (salience 99))
    (Modulo MPreguntas)
    (not (Evidencia gusta_bajo_nivel ?r))
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)
    (not (rParar SI))

    =>

    (printout t "¿Te gusta el bajo nivel? (Si | No | NS)" crlf)
    (bind ?respuesta (read))
    (assert (Evidencia gusta_bajo_nivel (lowcase ?respuesta)))
)

; Comprueba si el usuario quiere parar
(defrule comprobarPararR
    (declare (salience 9999))
    (Modulo MPreguntas)
    ?f <- (Evidencia ?e ?r)
    (test (eq ?r parar))

    =>

    (retract ?f)
    (printout t "Vale, no te hare mas preguntas..." crlf)
    (assert (rParar SI))
)

; Compruebo que la entrada sea valida
(defrule comprobar_entrada
    (declare (salience 999))
    (Modulo MPreguntas)
    ?x <- (Evidencia ?e ~si & ~no & ~ns & ~parar)
    
    =>

    (retract ?x)
    (printout t "Perdona no he entendido tu respuesta, introduce [si | no]" crlf)
)

; No hay mas preguntas
(defrule noHayMasPreguntas
    (declare (salience -200))
    ?f <- (Modulo MPreguntas)
    (AsignaturaR ?o1 ?asig1 ?n1)
    (AsignaturaR ?o2 ?asig2 ?n2)

    =>
    (retract ?f)
    (focus ModuloCalcularAsignatura)
    (assert (Modulo MCalcular))
)

; El usuario quiere parar
(defrule finPreguntas
    (declare (salience 9999))
    ?f <- (Modulo MPreguntas)
    (rParar SI)
    =>
    (retract ?f)
    (focus ModuloCalcularAsignatura)
    (assert (Modulo MCalcular))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CALCULO DE LA ASIGNATURA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmodule ModuloCalcularAsignatura (export ?ALL) (import ModuloPreguntasAsignatura ?ALL))

; Combinar misma deduccion por distintos caminos
(defrule combinar
    (declare (salience 1))
    (Modulo MCalcular)
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
    (Modulo MCalcular)
    (FactorCerteza ?h si ?fc1)
    (FactorCerteza ?h no ?fc2)
    =>
    (assert (Certeza ?h (- ?fc1 ?fc2)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas para asignaturas basicas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; R1: SI una asignatura es básica Y la otra asignatura no ENTONCES recomendar la basica
(defrule R1
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza tipo_basica no 1)
    (AsignaturaR B ?id ?n)
    (AsignaturaR ~B ? ?)
    =>
    (assert (FactorCerteza ?n si (encadenado 1 0.99)))
    (assert (Motivo FactorCerteza ?n si (encadenado 1 0.99) "es una asignatura basica"))
)

; R2: SI ambas asignaturas son basicas Y le gustan las matematicas Y una asignatura esta relacionada con las matematicas ENTONCES recomendar esta ultima asignatura
(defrule R2
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_matematicas si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B ALEM|CAL|EST|LMD ?n) ; asignaturas que estan relacionadas con las matematicas
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.8)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.8) "te gustan las matematicas"))
)

; R3: SI ambas asignaturas son basicas Y le gusta la programacion Y una asignatura esta relacionada con la programacion ENTONCES recomendar esta ultima asignatura
(defrule R3
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_programacion si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B FP|MP ?n) ; asignaturas que estan relacionadas con la programacion
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "te gusta la programacion"))
)

; R4: SI ambas asignaturas son basicas Y prefiere poca carga de trabajo Y una asignatura esta relacionada con la poca carga de trabajo ENTONCES recomendar esta ultima asignatura
(defrule R4
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza prefiere_poca_carga_trabajo si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B FFT|EST|IES|MP|TOC ?n) ; asignaturas que estan relacionadas con la poca carga de trabajo
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.3)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.3) "prefieres poca carga de trabajo"))
)

; R5: Si ambas asignaturas son basicas Y le gusta el hardware Y una asignatura esta relacionada con el hardware ENTONCES recomendar esta ultima asignatura
(defrule R5
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_hardware si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B TOC|FFT ?n) ; asignaturas que estan relacionadas con el hardware
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.95)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.95) "te gusta el hardware"))
)

; R6: Si ambas asignaturas son basicas Y tiene calificaciones altas Y una asignatura esta relacionada con las calificaciones altas ENTONCES recomendar esta ultima asignatura
(defrule R6
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza calificaciones_altas si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B CAL|ALEM|FS ?n) ; asignaturas que requieren sacar buenas notas porque son mas complicadas
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "tienes calificaciones altas"))
)

; R7: Si ambas asignaturas son basicas Y tiene calificaciones medias Y una asignatura esta relacionada con las calificaciones medias ENTONCES recomendar esta ultima asignatura
(defrule R7
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza calificaciones_medias si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B FP|FFT|EST|LMD ?n) ; asignaturas que requieren no sacar notas bajas
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.5)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.5) "tienes calificaciones medias"))
)

; R8: Si ambas asignaturas son basicas Y tiene calificaciones bajas Y una asignatura esta relacionada con las calificaciones bajas ENTONCES recomendar esta ultima asignatura
(defrule R8
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza calificaciones_bajas si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B IES|MP|TOC ?n) ; asignaturas que requieren no sacar mucha nota
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.85)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.85) "tienes calificaciones bajas"))
)

; R9: Si ambas asignaturas son basicas Y le gusta el alto nivel Y una asignatura esta relacionada con el alto nivel ENTONCES recomendar esta ultima asignatura
(defrule R9
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_alto_nivel si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B FS|FP|MP ?n) ; asignaturas que estan relacionadas con el alto nivel
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.35)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.35) "te gusta el alto nivel"))
)

; R10: Si ambas asignaturas son basicas Y le gusta el bajo nivel Y una asignatura esta relacionada con el bajo nivel ENTONCES recomendar esta ultima asignatura
(defrule R10
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_bajo_nivel si ?f)
    (AsignaturaR B ?id1 ?n)
    (AsignaturaR B FFT|TOC ?n) ; asignaturas que estan relacionadas con el bajo nivel
    (AsignaturaR B ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.4)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.4) "te gusta el bajo nivel"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas para asignaturas obligatorias

; R11: Si la asignatura es obligatoria Y la otra no es ni basica ni obligatoria ENTONCES recomendar la obligatoria
(defrule R11
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza tipo_obligatoria no 1)
    (AsignaturaR O ?id ?n)
    (AsignaturaR ~O ? ?)
    =>
    (assert (FactorCerteza ?n si (encadenado 1 0.98)))
    (assert (Motivo FactorCerteza ?n si (encadenado 1 0.98) "es una asignatura obligatoria"))
)

; R12: SI ambas asignaturas son obligatoria Y le gustan las matematicas Y una asignatura esta relacionada con las matematicas ENTONCES recomendar esta ultima asignatura
(defrule R12
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza gusta_matematicas si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O FBD|ED|IG|MC ?n) ; asignaturas que estan relacionadas con las matematicas
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.8)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.8) "te gustan las matematicas"))
)

; R13: SI ambas asignaturas son obligatoria Y le gusta la programacion Y una asignatura esta relacionada con la programacion ENTONCES recomendar esta ultima asignatura
(defrule R13
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza gusta_programacion si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O ED|PDOO|SCD|ALG|FBD|IA|IG ?n) ; asignaturas que estan relacionadas con la programacion
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "te gusta la programacion"))
)
; R14: SI ambas asignaturas son obligatoria Y prefiere poca carga de trabajo Y una asignatura esta relacionada con la poca carga de trabajo ENTONCES recomendar esta ultima asignatura
(defrule R14
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza prefiere_poca_carga_trabajo si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O EC|ED|PDOO|SCD|FIS|DDSI|FR ?n) ; asignaturas que estan relacionadas con la poca carga de trabajo
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.3)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.3) "prefieres poca carga de trabajo"))
)
; R15: Si ambas asignaturas son obligatoria Y le gusta el hardware Y una asignatura esta relacionada con el hardware ENTONCES recomendar esta ultima asignatura
(defrule R15
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza gusta_hardware si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O EC|AC|FR|ISE ?n) ; asignaturas que estan relacionadas con el hardware
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.95)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.95) "te gusta el hardware"))
)
; R16: Si ambas asignaturas son obligatoria Y tiene calificaciones altas Y una asignatura esta relacionada con las calificaciones altas ENTONCES recomendar esta ultima asignatura
(defrule R16
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza calificaciones_altas si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O PDOO|AC|FBD|IA|IG|MC ?n) ; asignaturas que requieren sacar buenas notas porque son mas complicadas
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "tienes calificaciones altas"))
)
; R17: Si ambas asignaturas son obligatoria Y tiene calificaciones medias Y una asignatura esta relacionada con las calificaciones medias ENTONCES recomendar esta ultima asignatura
(defrule R17
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza calificaciones_medias si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O ED|SO|ALG|FR|ISE ?n) ; asignaturas que requieren no sacar notas bajas
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.5)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.5) "tienes calificaciones medias"))
)
; R18: Si ambas asignaturas son obligatoria Y tiene calificaciones bajas Y una asignatura esta relacionada con las calificaciones bajas ENTONCES recomendar esta ultima asignatura
(defrule R18
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza calificaciones_bajas si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O EC|SCD|FIS|DDSI ?n) ; asignaturas que requieren no sacar mucha nota
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.85)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.85) "tienes calificaciones bajas"))
)
; R19: Si ambas asignaturas son obligatoria Y le gusta el alto nivel Y una asignatura esta relacionada con el alto nivel ENTONCES recomendar esta ultima asignatura
(defrule R19
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza gusta_alto_nivel si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O ED|PDOO|ALG|FBD|IA|DDSI|IG ?n) ; asignaturas que estan relacionadas con el alto nivel
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.35)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.35) "te gusta el alto nivel"))
)
; R20: Si ambas asignaturas son obligatoria Y le gusta el bajo nivel Y una asignatura esta relacionada con el bajo nivel ENTONCES recomendar esta ultima asignatura
(defrule R20
    (Modulo MCalcular)
    (FactorCerteza tipo_obligatoria si 1)
    (FactorCerteza gusta_bajo_nivel si ?f)
    (AsignaturaR O ?id1 ?n)
    (AsignaturaR O EC|SCD|SO|AC|FIS|ISE ?n) ; asignaturas que estan relacionadas con el bajo nivel
    (AsignaturaR O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.4)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.4) "te gusta el bajo nivel"))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reglas para asignaturas optativas

; R21: Si ambas asignaturas son optativas Y le gustan las matematicas Y una asignatura esta relacionada con las matematicas ENTONCES recomendar esta ultima asignatura
(defrule R21
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza gusta_matematicas si ?f)
    (AsignaturaR ~B & ~O ?id1 ?n)
    (AsignaturaR ~B & ~O AA|MAC|TSI|SG|NPI|VC|PL|SPSI|ISI ?n) ; asignaturas que estan relacionadas con las matematicas
    (AsignaturaR ~B & ~O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.8)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.8) "te gustan las matematicas"))
)

; R22: Si ambas asignaturas son optativas Y le gusta la programacion Y una asignatura esta relacionada con la programación ENTONCES recomendar esta ultima asignatura
(defrule R22
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza gusta_programacion si ?f)
    (AsignaturaR ~B & ~O ?id1 ?n)
    (AsignaturaR ~B & ~O AA|MH|TSI|SIBW|SG|ABD|PW|SM|SMM|TW|PL|VC|DBA|IN|RI|DAI ?n) ; asignaturas que estan relacionadas con la programacion
    (AsignaturaR ~B & ~O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "te gusta la programacion"))
)

; R23: Si ambas asignaturas son optativas Y prefiere poca carga de trabajo Y una asignatura esta relacionada con la poca carga de trabajo ENTONCES recomendar esta ultima asignatura
(defrule R23
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza prefiere_poca_carga_trabajo si ?f)
    (AsignaturaR IC|TI ?id1 ?n) ; asignaturas que estan relacionadas con la poca carga de trabajo
    (AsignaturaR ~B & ~O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.3)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.3) "prefieres poca carga de trabajo"))
)
; R24: Si ambas asignaturas son optativas Y le gusta el hardware Y una asignatura esta relacionada con el hardware ENTONCES recomendar esta ultima asignatura
(defrule R24
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza gusta_hardware si ?f)
    (AsignaturaR IC ?id1 ?n) ; asignaturas que estan relacionadas con el hardware
    (AsignaturaR ~B & ~ O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.95)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.95) "te gusta el hardware"))
)
; R25: Si ambas asignaturas son optativas Y tiene calificaciones altas Y una asignatura esta relacionada con las calificaciones altas ENTONCES recomendar esta ultima asignatura
(defrule R25
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza calificaciones_altas si ?f)
    (AsignaturaR ~B & ~ O ?id1 ?n)
    (AsignaturaR ~B & ~ O IV|SPSI|BDD|NPI|VC|SMM|SIBW|DS|DSD|SG|AA|MAC ?n) ; asignaturas que requieren sacar buenas notas porque son mas complicadas
    (AsignaturaR ~B & ~ O  ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.75)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.75) "tienes calificaciones altas"))
)
; R26: Si ambas asignaturas son optativas Y tiene calificaciones medias Y una asignatura esta relacionada con las calificaciones medias ENTONCES recomendar esta ultima asignatura
(defrule R26
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza calificaciones_medias si ?f)
    (AsignaturaR ~B & ~ O ?id1 ?n)
    (AsignaturaR ~B & ~ O IC|MH|TSI|AS|DHD|SCM|PW|SM|CUIA|DBA|SE|IN|DAI ?n) ; asignaturas que requieren no sacar notas bajas
    (AsignaturaR ~B & ~ O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.5)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.5) "tienes calificaciones medias"))
)
; R27: Si ambas asignaturas son optativas Y tiene calificaciones bajas Y una asignatura esta relacionada con las calificaciones bajas ENTONCES recomendar esta ultima asignatura
(defrule R27
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza calificaciones_bajas si ?f)
    (AsignaturaR ~B & ~ O ?id1 ?n)
    (AsignaturaR ~B & ~ O DAI|RI|TR|CPD|MDA|DGP|DBA|PL|TDRC|TW|SWAP|CUIA|SIE|ISI|ABD|DSE|ACAP|DIU ?n) ; asignaturas que requieren no sacar mucha nota
    (AsignaturaR ~B & ~ O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.85)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.85) "tienes calificaciones bajas"))
)
; R28: Si ambas asignaturas son optativas Y le gusta el alto nivel Y una asignatura esta relacionada con el alto nivel ENTONCES recomendar esta ultima asignatura
(defrule R28
    (Modulo MCalcular)
    (FactorCerteza tipo_optativa si 1)
    (FactorCerteza gusta_alto_nivel si ?f)
    (AsignaturaR CSI|SI ?id1 ?n) ; asignaturas que estan relacionadas con el alto nivel
    (AsignaturaR ~B & ~ O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.35)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.35) "te gusta el alto nivel"))
)
; R29: Si ambas asignaturas son optativas Y le gusta el bajo nivel Y una asignatura esta relacionada con el bajo nivel ENTONCES recomendar esta ultima asignatura
(defrule R29
    (Modulo MCalcular)
    (FactorCerteza tipo_basica si 1)
    (FactorCerteza gusta_bajo_nivel si ?f)
    (AsignaturaR IS|IC|TI ?id1 ?n) ; asignaturas que estan relacionadas con el bajo nivel
    (AsignaturaR ~B & ~ O ?id2 ?)
    (test (neq ?id1 ?id2))
    =>
    (assert (FactorCerteza ?n si (encadenado ?f 0.4)))
    (assert (Motivo FactorCerteza ?n si (encadenado ?f 0.4) "te gusta el bajo nivel"))
)

; R30: asignar por defecto la primera asignatura
(defrule R30
    (Modulo MCalcular)
    (AsignaturaR ?t1 ?id1 ?n1)
    (AsignaturaR ?t2 ?id2 ?n2)
    (test (neq ?id1 ?id2))
    (not (and (Motivo FactorCerteza ?h1 ?r1 ?f1 ?expl1) (test (< ?f1 1))))
    
    =>
    (assert (FactorCerteza ?n1 si (encadenado 1 0.99)))
    (assert (Motivo FactorCerteza ?n1 si (encadenado 1 0.99) "no me has aportado informacion"))
)


;;;;;;;;;;;;;;;; Tras razonar quedarse con las hipotesis con mayor certeza ;;;;;;;;;;;;;;;;
(defrule mayor_certeza
    (declare (salience -1))
    (Modulo MCalcular)
    (FactorCerteza ?h1 ?r1 ?f1)
    ?x <- (FactorCerteza ?h2 ?r2 ?f2)
    (test
        (and
            (> ?f1 ?f2)
            (< ?f1 1)
        )
    )

    =>
    (printout t "Voy a eliminar que " ?r2 " ocurra que " ?h2 " porque tiene menos certeza (" ?f2 ")" crlf)
    (retract ?x)
    
)

; Terminar si ya se ha aconsejado
(defrule noHayMasCalculos
    (declare (salience -10))
    ?m <- (Modulo MCalcular)
    (FactorCerteza ?h1 ?r1 ?f1)
    (Motivo FactorCerteza ?h1 ?r1 ?f1 ?expl1)

    =>

    (retract ?m)
    (focus ModuloConsejoAsignatura)
    (assert (Modulo MConsejo))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSEJO FINAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule ModuloConsejoAsignatura (import ModuloCalcularAsignatura ?ALL))

;;;;;;;;;;;;;;;; Deducciones ;;;;;;;;;;;;;;;;

; No hay diferencia entre ambas
(defrule deduccionesIgual
    (declare (salience -2))
    (Modulo MConsejo)
    ?x1 <- (FactorCerteza ?h1 ?r1 ?f1)
    ?y1 <- (Motivo FactorCerteza ?h1 ?r1 ?f1 ?expl1)
    ?x2 <- (FactorCerteza ?h2 ?r2 ?f2)
    ?y2 <- (Motivo FactorCerteza ?h2 ?r2 ?f2 ?expl2)
    (test (= ?f1 ?f2))
    (test (< ?f1 1))
    (test (neq ?h1 ?h2))
    =>
    (printout t "El factor deducido es que te recomiendo ambas por igual, no he encontrado gran diferencia." crlf)
    (printout t "El factor deducido es que " ?r1 " te recomiendo " ?h1 " con certeza " ?f1 " porque " ?expl1 crlf)
    (printout t "El factor deducido es que " ?r2 " te recomiendo " ?h2 " con certeza " ?f2 " porque " ?expl2 crlf)    
    (retract ?x1 ?y1 ?x2 ?y2)
)


(defrule deducciones
    (declare (salience -3))
    (Modulo MConsejo)
    ?x <- (FactorCerteza ?h ?r ?f)
    ?y <- (Motivo FactorCerteza ?h ?r ?f ?expl)
    (test (< ?f 1))
    =>
    (printout t "El factor deducido es que " ?r " te recomiendo " ?h " con certeza " ?f " porque " ?expl crlf)
    (retract ?x ?y)
)