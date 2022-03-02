; Paula Villanueva
;;;; HECHOS GENERALES DEL SISTEMA ;;;;;
;;;;(seran validos para todas las ejecuciones del sistema ;;;;

; Listado de personas de la familia en cuestion introduccidas con la propiedad unaria de hombre o mujer

(deffacts personas
   (hombre Federico) ; "Federico es un hombre"
   (hombre Jorge)
   (hombre Sebastian)
   (hombre Florencio)
   (hombre JoseRamon)
   (hombre Federete)
   (mujer Nuria)         ; Nuria es una mujer
   (mujer Paula)
   (mujer Marta)
   (mujer Irene)
   (mujer Josefina)
   (mujer Amparo)
   (mujer Flor)
   (mujer Maria)
   (mujer Carmen)
   (mujer MariCarmen)
   (mujer Almudena)
   (mujer Blanca)
   (mujer Montserrat) )

;;;;; Plantilla tipica de Relaciones binarias, ajustada a relaciones de parentesco restringiendo los valores de tipo de relacion a estas. Se usa para registrar "El <sujeto> es <tipo de relacion> de <objeto>"

(deftemplate Relacion 
  (slot tipo (type SYMBOL) (allowed-symbols HIJO PADRE ABUELO NIETO HERMANO ESPOSO PRIMO TIO SOBRINO  CUNIADO YERNO SUEGRO))
  (slot sujeto)
  (slot objeto))

;;;;; Datos de la relacion HIJO y ESPOSO en mi familia que es suficiente para el problema, pues el resto se deduce de estas

(deffacts relaciones
   (Relacion (tipo HIJO) (sujeto Nuria) (objeto Federico)) ; "Nuria es HIJO de Federico"
   (Relacion (tipo HIJO) (sujeto Paula) (objeto Federico))
   (Relacion (tipo HIJO) (sujeto Federico) (objeto Federete))
   (Relacion (tipo HIJO) (sujeto JoseRamon) (objeto Federete))
   (Relacion (tipo HIJO) (sujeto MariCarmen) (objeto Federete))
   (Relacion (tipo HIJO) (sujeto Josefina) (objeto Florencio))
   (Relacion (tipo HIJO) (sujeto Amparo) (objeto Florencio))
   (Relacion (tipo HIJO) (sujeto Flor) (objeto Florencio))
   (Relacion (tipo HIJO) (sujeto Irene) (objeto Sebastian))
   (Relacion (tipo HIJO) (sujeto Jorge) (objeto Sebastian))
   (Relacion (tipo HIJO) (sujeto Marta) (objeto Sebastian))
   (Relacion (tipo ESPOSO) (sujeto Federico) (objeto Josefina)) ; "Federico es ESPOSO de Josefina"
   (Relacion (tipo ESPOSO) (sujeto Florencio) (objeto Maria)) 
   (Relacion (tipo ESPOSO) (sujeto Sebastian) (objeto Amparo))
   (Relacion (tipo ESPOSO) (sujeto JoseRamon) (objeto Montserrat))
   (Relacion (tipo ESPOSO) (sujeto Federete) (objeto Carmen)))

;;;;;;; Cada relacion tiene una relacion dual que se produce al cambiar entre si objeto y sujeto. Por ejemplo, Si x es HIJO de y, y es PADRE de x". Para poder deducirlo con una sola regla metemos esa informacion como hechos con la etiqueta dual, "Dual de HIJO PADRE", y asi con todas las relaciones consideradas
 
(deffacts duales
(dual HIJO PADRE) (dual ABUELO NIETO) (dual HERMANO HERMANO) (dual ESPOSO ESPOSO) (dual PRIMO PRIMO) (dual TIO SOBRINO) (dual CUNIADO CUNIADO) (dual YERNO SUEGRO))

;;;;;; Para deducir las reglas que se aplican son de composicion, del tipo "el HERMANO del PADRE es un TIO". Por comodidad, en lugar de crear una regla por cada posible composicion, metemos como hechos la relacion que se obtiene por composicion. Solo metemos unas cuantas composiciones que sean suficientes para deducir cualquier cosa

(deffacts compuestos
(comp HIJO HIJO NIETO) (comp PADRE PADRE ABUELO) (comp ESPOSO PADRE PADRE)(comp HERMANO PADRE TIO) (comp HERMANO ESPOSO CUNIADO) (comp ESPOSO HIJO YERNO) (comp ESPOSO HERMANO CUNIADO) (comp HIJO PADRE HERMANO) (comp ESPOSO CUNIADO CUNIADO) (comp ESPOSO TIO TIO)  (comp HIJO TIO PRIMO)  ) 

;;;;;; Para que cuando digamos por pantalla el parentesco lo espresemos correctamente, y puesto que el nombre que hemos puesto a cada relacion es el caso masculino, vamos a meter como hechos como se diaria esa relacion en femenino mediante la etiqueta femenino

(deffacts femenino
(femenino HIJO HIJA) (femenino PADRE MADRE) (femenino ABUELO ABUELA) (femenino NIETO NIETA) (femenino HERMANO HERMANA) (femenino ESPOSO ESPOSA) (femenino PRIMO PRIMA) (femenino TIO TIA) (femenino SOBRINO SOBRINA) (femenino CUNIADO CUNIADA) (femenino YERNO NUERA) (femenino SUEGRO SUEGRA)) 

;;;;; REGLAS DEL SISTEMA ;;;;;

;;;; La dualidad es simetrica: si r es dual de t, t es dual de r. Por eso solo metimos como hecho la dualidad en un sentidos, pues en el otro lo podiamos deducir con esta regla

(defrule autodualidad
      (razonar)
      (dual ?r ?t)
=> 
   (assert (dual ?t ?r)))


;;;; Si  x es R de y, entonces y es dualdeR de x

(defrule dualidad
   (razonar)
   (Relacion (tipo ?r) (sujeto ?x) (objeto ?y))
   (dual ?r ?t)
=> 
   (assert (Relacion (tipo ?t) (sujeto ?y) (objeto ?x))))

;;;; Si  y es R de x, y x es T de z entonces y es RoT de z
;;;; aniadimos que z e y sean distintos para evitar que uno resulte hermano de si mismo y cosas asi.

(defrule composicion
   (razonar)
   (Relacion (tipo ?r) (sujeto ?y) (objeto ?x))
   (Relacion (tipo ?t) (sujeto ?x) (objeto ?z))
   (comp ?r ?t ?u)
   (test (neq ?y ?z))
=> 
   (assert (Relacion (tipo ?u) (sujeto ?y) (objeto ?z))))

;;;;; Como puede deducir que tu hermano es tu cuniado al ser el esposo de tu cuniada, eliminamos los cuniados que sean hermanos

(defrule limpiacuniados
    (Relacion (tipo HERMANO) (sujeto ?x) (objeto ?y))
    ?f <- (Relacion (tipo CUNIADO) (sujeto ?x) (objeto ?y))
=>
	(retract ?f) )

;;;;; Solicitamos el parentesco de la persona sobre la que se desea informacion y guardamos y aniadimos ese hecho 

(defrule preguntaparentesco
(declare (salience 1000))
=>
   (printout t "Dime el parentesco de la persona de la Familia Villanueva sobre la que quieres informacion [hijo, padre, abuelo, nieto, hermano, esposo, primo, tio, sobrino, cuniado, yerno, suegro]: " crlf)
   (assert (parentesco (upcase (read))))
)

;;;;; Solicitamos el nombre de la persona sobre el que se desea informacion y guardamos y aniadimos ese hecho 

(defrule preguntanombre
(declare (salience 100))
=>
   (printout t "Dime el nombre de la persona de la Familia Villanueva sobre la que quieres informacion (escribe solo el nombre): " crlf)
   (assert (personanombre (read)))
   (assert (razonar))
)

;;;;; Hacemos que nos diga por pantalla las personas que tienen la relacion introducida con la persona introducida

; Si el sujeto es un hombre y tiene una relacion con la persona
(defrule relacionparentescomasculino
   (parentesco ?x)		
   (personanombre ?y)
   (Relacion (tipo ?x) (sujeto ?z) (objeto ?y))
   (hombre ?z)
 =>
   (printout t "El " ?x " de " ?y " es " ?z crlf)
)

; Si el sujeto es una mujer y tiene una relacion con la persona, ponemos la relacion en femenino
(defrule relacionparentescofemenino
   (parentesco ?x)		
   (personanombre ?y)
   (Relacion (tipo ?x) (sujeto ?z) (objeto ?y))
   (mujer ?z)
   (femenino ?x ?t)
 =>
   (printout t "La " ?t " de " ?y " es " ?z crlf)
)

; Si nadie tiene relacion con la persona
(defrule norelacionparentesco
   (parentesco ?x)		
   (personanombre ?y)
   (not (Relacion (tipo ?x) (sujeto ?z) (objeto ?y)))
 =>
   (printout t ?y " no tiene " ?x "S" crlf)
)