;seccion de declaracion de variables globales
(defglobal
	?*intPreguntasPrimeraRonda* = 1 ;para cambiar el comportamiento de cuantas preguntas para la primera ronda
	?*intPreguntasSegundaRonda* = 1 ;para cambiar el comportamiento de cuantas preguntas para la segunda ronda
	?*intPreguntasRealizadasPrimeraRonda* = 0 ;contador para saber cuantas preguntas se han realizado en la primera ronda
	?*intPreguntasRealizadasSegundaRonda* = 0 ;contador para saber cuantas preguntas se han realizado en la primera ronda	
	?*intNumeroTotalPreguntas* = 3 ;me dice el numero total de preguntas que tengo para que el random genere en el intervalo
	?*strElecccion* = "";variable para almacenar la eleccion del usuario
	?*intPersonajesEnLista* = 83 ;para ir contando cuantos personajes quedan en la fact-list
	?*intSeguir* = 1 ;1 indica que debe seguir preguntado, 0 indica que debe parar
	?*intEncontroPersonaje* = 0 ;indica si encontro personaje, si tiene 0 al final quiere decir que fallo
	?*strNombreEncontrado* = "" ;almacena el nombre del personaje que coincidio
	?*intResetearFacts* = 0 ;indica si se le da (reset) para volver a iniciar el programa,usado cuando despues de adiviar se vuelve a intentar
	?*intEjecutarSetup* = 1 ;indica si se debe ejecutar el codigo de el rule setup, solo
	?*strMasProbable* = "" ;cuando se busque cual es el mas probable de salir su nombre se almacenara aca
	?*intPunteoMayor* = -10000 ; para hacer una comparacion e ir eligiendo el que tiene mayor pprobabilidad de salir
	?*intPreguntasEnLista* = 15
)
;seccion para el deftemlate de personaje, defino las caracteristicas que cada uno tendrá
(deftemplate personaje
	(slot nombre);el nombre del personaje
	(slot canta);propiedad que indica si canta o no
	(slot sexo);propiedad que indica si es masculino o femenino
	(slot existe);propiedad que indica si un personaje es o fue real
	(slot origen); para la nacionalidad del personaje en caso de ser real, o de donde lo hicieron en caso de ser ficticio
	(slot superheroe);para indicar si el personaje en cuestion es un superheroe
	(slot escritor)
	(slot animal)
	(slot instrumento)
	(slot millonario)
	(slot punteo);punteo o probabilidad de que el personaje sea el elegido.
)

;seccion para el deftemplate de preguntas, defino propiedades de cada pregunta
(deftemplate pregunta
	(slot question) ;para almacenar el texto en si de la pregunta, en ingles por que no se puede poner "pregunta" como slot de "pregunta" jaja
	(slot indice) ;para tratar la fact-list como un "array", cada pregunta tiene un indice, asi puedo preguntar en random(en teoria)
	(slot atributo);dice que atributo del personaje tiene relacionado
	(slot valor);dice que valor para ese atributo tiene asociado
)

;elimina una pregunta de la fact-list,puesto que todas tienen un indice distinto, se pasa el indice de cual quiero eliminar.
(deffunction eliminarPregunta(?indice)
	=>
	(delayed-do-for-all-facts ((?elemento pregunta)) ;recorre toda la lista de facts y cada fact de tipo pregunta la almacena en ?elemento
		(if(eq ?elemento:indice ?indice) then
			(retract ?elemento)
			(bind ?*intPreguntasEnLista* (- ?*intPreguntasEnLista* 1))
		)
	)
	;contar cuantas preguntas quedan
	
)

;segun la respuesta modifica el punteo de similitud de cada personaje
(deffunction modificarPunteo(?atributo ?valor ?respuesta)
	(bind ?punteo 0)
	(switch ?respuesta  ;para asignar un punteo en base a la respuesta seleccionada
		(case "si" then
			(bind ?punteo 100)
		)
		(case "no" then
			(bind ?punteo 0)
		)
		(case "no lo se" then
			(bind ?punteo 0)
		)
		(case "probablemente si" then
			(bind ?punteo 75)
		)
		(case "probablemente no" then
			(bind ?punteo -25)
		)
		(default then)
	)
	
	(switch  ?atributo ;para ir filtrando por atributos
			(case "sexo" then ;si la pregunta es acerca del sexo
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:sexo ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "canta" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:canta ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "existe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:existe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "origen" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:origen ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "superheroe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:superheroe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "escritor" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:escritor ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(case "millonario" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:millonario ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(modify ?elemento(punteo (+ ?elemento:punteo ?punteo)))
				)
			)
			(default then)
	)
	
)



;elimina personajes que para un atributo dado(parametro) este tenga un valor(parametro)
(deffunction eliminarPersonajeSegunAtributo(?atributo ?valor)
	(switch  ?atributo ;para ir filtrando por atributos
			(case "sexo" then ;si la pregunta es acerca del sexo
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:sexo ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "canta" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:canta ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "existe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:existe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "origen" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:origen ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "superheroe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:superheroe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "escritor" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:escritor ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "millonario" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(eq ?elemento:millonario ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(default then)
	)
)

;elimina personajes que para un atributo dado(parametro) este no tenga un valor(parametro)
(deffunction eliminarPersonajeSegunNoAtributo(?atributo ?valor)
	(switch  ?atributo ;para ir filtrando por atributos
			(case "sexo" then ;si la pregunta es acerca del sexo
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:sexo ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "canta" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:canta ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "existe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:existe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "origen" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:origen ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "superheroe" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:superheroe ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "escritor" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:escritor ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(case "millonario" then
				(delayed-do-for-all-facts ((?elemento personaje));busca todos los personajes y a continuacion seleciconar solo para ceirta propiedad
					(neq ?elemento:millonario ?valor) ;si para cierto personaje el sexo es igual que el solicitado por la pregunta
					(retract ?elemento)
					(bind ?*intPersonajesEnLista* (- ?*intPersonajesEnLista*  1))
				)
			)
			(default then)
	)
)

;verifica la fact-list, si solo hay un elemento quiere decir que ya encontro el personaje
(deffunction verificarEncontrado ();cambiarle algo para que ya no siga preguntando
	(if(eq ?*intPersonajesEnLista* 1) then
		(delayed-do-for-all-facts((?elemento personaje))
			(bind ?*strNombreEncontrado* ?elemento:nombre)
			(bind ?*intSeguir* 0)
			(bind ?*intEncontroPersonaje* 1)
		)
	)
	(if (eq ?*intPersonajesEnLista* 0) then ;si ya no hay mas preguntas fallo
		(bind ?*intEncontroPersonaje* 0)
		(bind ?*intSeguir* 0)
		(bind ?strNombreEncontrado "")
 	)
)

(deffunction buscarMasProbable()
	(bind ?*intPunteoMayor*  -10000)
	(bind ?*strMasProbable* "")
	(delayed-do-for-all-facts ((?elemento personaje))
		(if (> ?elemento:punteo ?*intPunteoMayor* ) then
			(bind ?*intPunteoMayor* ?elemento:punteo)
			(bind ?*strMasProbable* ?elemento:nombre)
		)	
	)
)

(deffunction pedirRespuesta(?question)
	(printout t ?question crlf)
	(bind ?*strElecccion* (readline))
	(bind ?*strElecccion*  (lowcase ?*strElecccion*))
	(bind ?fueSi (neq ?*strElecccion* "si"))
	(bind ?fueNo (neq ?*strElecccion* "no"))
	(bind ?fueNoLoSe (neq ?*strElecccion* "no lo se"))
	(bind ?fueProbablementeSi (neq ?*strElecccion* "probablemente si"))
	(bind ?fueProbablementeNo (neq ?*strElecccion* "probablemente no"))

	;funciona negado, las variables traen false si fue si,no,no lo se respectivamente entonces si traen false, itera mientras no haya una respuesta valida
	(while (and ?fueSi (and ?fueNo (and ?fueNoLoSe (and ?fueProbablementeSi ?fueProbablementeNo))))
		(printout t "Eleccion invalida,intente de nuevo." crlf)
		(bind ?*strElecccion* (readline))
		(bind ?*strElecccion*  (lowcase ?*strElecccion*))
		(bind ?fueSi (neq ?*strElecccion* "si"))
		(bind ?fueNo (neq ?*strElecccion* "no"))
		(bind ?fueNoLoSe (neq ?*strElecccion* "no lo se"))
		(bind ?fueProbablementeSi (neq ?*strElecccion* "probablemente si"))
		(bind ?fueProbablementeNo (neq ?*strElecccion* "probablemente no"))
	)
)
;funcion que realizara las preguntas
(deffunction hacerPregunta (?question ?indice ?atributo ?valor)
	(pedirRespuesta ?question)
	(switch ?*strElecccion*
		(case "si" then
			;para el si modificar el punteo o probabilidad de los que tienen ese atributo, eliminar de la lista a los que no
			(modificarPunteo ?atributo ?valor ?*strElecccion*)
			(eliminarPersonajeSegunNoAtributo ?atributo ?valor)
		)
		(case "no" then
			;para el no
			;borrarlo de una vez
			(eliminarPersonajeSegunAtributo ?atributo ?valor)
		)
		(case "no lo se" then
			;para el no lo se
			(eliminarPregunta ?indice)
			(if (eq ?*intPreguntasEnLista* 0) then
				(buscarMasProbable)
				(printout  t "Se acabaron las preguntas y no adivine con seguridad,pero lo intentare,su personaje es" ?*strMasProbable* "?" crlf)
				(bind ?*intSeguir* 0)
				(return)
			)	
		)
		(case "probablemente si" then
			;para el probablemente si
			(modificarPunteo ?atributo ?valor ?*strElecccion*)
		)
		(case "probablemente no" then
			;para el probablemente no
			(modificarPunteo ?atributo ?valor ?*strElecccion*)
		)
		(default then
			(printout t "Opcion invalida,se asume :no lo se." crlf)
			(eliminarPregunta ?indice)
		)
	)
	(verificarEncontrado)
)



(deffunction programa() ;funcion que manejara la logica del programa
	(delayed-do-for-all-facts ((?elemento pregunta)) ;recorrera todas las preguntas
		(if(and (eq ?*intEncontroPersonaje* 0)(< ?*intPreguntasRealizadasPrimeraRonda* ?*intPreguntasPrimeraRonda*)) then ;y si el numero de preguntas realizadas es menor a el numero debido de la primera ronda,preguntara
			(hacerPregunta ?elemento:question ?elemento:indice ?elemento:atributo ?elemento:valor)
			(bind ?*intPreguntasRealizadasPrimeraRonda*(+ ?*intPreguntasRealizadasPrimeraRonda* 1))
		)
	)
	(buscarMasProbable)
	(printout t "Dejeme intentarlo, su personaje es  " ?*strMasProbable*  " ?(si)(no)"crlf)
	(bind ?probarOtro (readline))
	(bind ?probarOtro (lowcase ?probarOtro))
	(while (and (neq ?probarOtro "si")(neq ?probarOtro "no"))
		(printout t "Opcion invalida,intente otra vez." crlf)
		(bind ?probarOtro (readline))
		(bind ?probarOtro (lowcase ?probarOtro))
	)
	(if (eq ?probarOtro "si") then
		(bind ?*intEncontroPersonaje* 1)
		(bind ?*strNombreEncontrado* ?*strMasProbable*)
	)
	(if (eq ?probarOtro "no") then
		(bind ?*intEncontroPersonaje* 0)
		(bind ?*strNombreEncontrado* "")
	)
	
	;si termino el primer set, y no encontro respuesta, se va al segundo
	(if (eq ?*intEncontroPersonaje* 0) then
		(printout t "El personaje no fue adivinado en el primer segmento,sigo adivinando(seguir) o me detengo(salir)?" crlf)
		(bind ?seguir (readline))
		(bind ?seguir (lowcase ?seguir))
			(while(and (neq ?seguir "seguir")(neq ?seguir "salir"))
				(printout t "Opcion invalida,intente otra vez." crlf)
				(bind ?seguir (readline))
				(bind ?seguir (lowcase ?seguir))
			)
			(if (eq ?seguir "seguir") then
				(bind ?*intSeguir* 1)
				(if (eq ?*intPersonajesEnLista* 1) then
					(printout t "Lo siento ,no puedo seguir adivinando, solo este personaje coincide con tus respuestas" crlf)
					(bind ?*intEncontroPersonaje* 0)
					(bind ?*intSeguir* 0)
				else
					(printout t "Iniciando segundo segmento de preguntas" crlf) 
					(delayed-do-for-all-facts ((?elemento pregunta));en caso de que se termino el primer set de preguntas, va por el segundo
						(if(and(eq ?*intEncontroPersonaje* 0) (< ?*intPreguntasRealizadasSegundaRonda* ?*intPreguntasSegundaRonda* )) then ;y si el numero de preguntas realizadas es menor a el numero debido de la primera ronda,preguntara
							(hacerPregunta ?elemento:question ?elemento:indice ?elemento:atributo ?elemento:valor)
							(bind ?*intPreguntasRealizadasSegundaRonda*(+ ?*intPreguntasRealizadasSegundaRonda* 1))
						)
					)
					
					(buscarMasProbable)
					(printout t "Dejeme intentarlo, su personaje es  " ?*strMasProbable*  " ?(si)(no)"crlf)
					(bind ?probarOtro (readline))
					(bind ?probarOtro (lowcase ?probarOtro))
					(while (and (neq ?probarOtro "si")(neq ?probarOtro "no"))
						(printout t "Opcion invalida,intente otra vez." crlf)
						(bind ?probarOtro (readline))
						(bind ?probarOtro (lowcase ?probarOtro))
					)
					(if (eq ?probarOtro "si") then
						(bind ?*intEncontroPersonaje* 1)
						(bind ?*strNombreEncontrado* ?*strMasProbable*)
					)
					(if (eq ?probarOtro "no") then
						(bind ?*intEncontroPersonaje* 0)
						(bind ?*strNombreEncontrado* "")
					)
				)
			)
			(if (eq ?seguir "salir") then
				(bind ?*intSeguir* 0)
			)
		
	)
	
	
	(if(eq ?*intEncontroPersonaje* 0) then ;si termino y no lo encontro
		(bind ?*intSeguir* 0)
		(printout t "Que dificil, no pude adivinar el personaje, cual era su nombre?" crlf)
		(bind ?nombrePersonaje (readline))
		(open "noEncontrados.txt" archivoNoEncontrados "a")
		(printout archivoNoEncontrados ?nombrePersonaje crlf)
		(close)
		(printout  t "Ok,he guardado a " ?nombrePersonaje " en un archivo ,sigo intentando adivinar(si) o paro (no)" crlf)
		(bind ?probarOtro (readline))
		(bind ?probarOtro (lowcase ?probarOtro))
			(while (and (neq ?probarOtro "si")(neq ?probarOtro "no"))
				(printout t "Opcion invalida,intente otra vez." crlf)
				(bind ?probarOtro (readline))
				(bind ?probarOtro (lowcase ?probarOtro))
			)
			(if (eq ?probarOtro "si") then
				(reset)
				(bind ?*intResetearFacts* 1)
			)
			(if(eq ?probarOtro "no") then
				(printout t "Saliendo de Akinator a la tortrix" crlf)
				(bind ?*intSeguir* 0)
			)
		
	)
	(if (eq  ?*intEncontroPersonaje* 1) then ;si termino y si lo encontro
		(printout t "Su personaje fue encontrado, es " ?*strNombreEncontrado* crlf)
		(printout t "Es esto correcto?" crlf)
		(bind ?correcto (readline))
		(bind ?correcto  (lowcase ?correcto))
		(while (and (neq ?correcto "si") (neq ?correcto "no"))
			(printout t "Opcion invalida,intente otra vez." crlf)
			(bind ?correcto (readline))
			(bind ?correcto  (lowcase ?correcto))
		)
		(if (eq ?correcto "si") then ;pregunta al usuario si adivino correctamente, este es para el si
			(printout t "Que bien!! lo he adivinado correctamente." crlf)
			(printout t "Desea probar con otro personaje " crlf)
			(bind ?probarOtro (readline))
			(bind ?probarOtro (lowcase ?probarOtro))
			(while (and (neq ?probarOtro "si")(neq ?probarOtro "no"))
				(printout t "Opcion invalida,intente otra vez." crlf)
				(bind ?probarOtro (readline))
				(bind ?probarOtro (lowcase ?probarOtro))
			)
			(if (eq ?probarOtro "si") then
				(reset)
				(bind ?*intResetearFacts* 1)
			)
			(if(eq ?probarOtro "no") then
				(printout t "Saliendo de Akinator a la tortrix" crlf)
				(bind ?*intSeguir* 0)
			)
		)
		(if (eq ?correcto "no") then ;esto es cuando el ususario dice que no era ese personaje
			(printout t "Seguro que no era ese?,sigo adivinando(seguir) o me detengo(salir)?" crlf)
			(bind ?*intEncontroPersonaje* 0)
			(bind ?seguir (readline))
			(bind ?seguir (lowcase ?seguir))
			(while(and (neq ?seguir "seguir")(neq ?seguir "salir"))
				(printout t "Opcion invalida,intente otra vez." crlf)
				(bind ?seguir (readline))
				(bind ?seguir (lowcase ?seguir))
			)
			(if (eq ?seguir "seguir") then
				(bind ?*intSeguir* 1)
				(if (eq ?*intPersonajesEnLista* 1) then
					(printout t "Lo siento ,no puedo seguir adivinando, solo este personaje coincide con tus respuestas" crlf)
					(bind ?*intEncontroPersonaje* 0)
					(bind ?*intSeguir* 0)
				)
			)
			(if (eq ?seguir "salir") then
				(bind ?*intSeguir* 0)
			)
 		)
	)
)

;todas las preparaciones iniciales,cargar datos,mostrar instrucciones,etc
(defrule setup 
	(declare (salience 100));asigna la mas alta prioridad a esta regla para que se ejecute primero
	=>

	(if (eq ?*intEjecutarSetup* 1) then
	(load-facts "proyecto2_datos_200715367.clp")
	(printout t "Proyecto 2,Inteligencia Artificial, Luis Leal 200715367" crlf)
	(printout t "------------------Akinator A la Tortrix----------------" crlf)
	(printout t "Instrucciones:" crlf)
	(printout t "1. Piense en un personaje." crlf)
	(printout t "2. Responda a la serie de preguntas con una de las siguientes opciones:" crlf)
	(printout t "    1. Si." crlf)
	(printout t "    2. No." crlf)
	(printout t "    3. No lo se." crlf)
	(printout t "    4. Probablemente si." crlf)
	(printout t "    5. Probablemente no." crlf)
	(printout t "Presione Enter para continuar" crlf)
	(readline)	
	)
	(bind ?*intEjecutarSetup* 0)
)

(defrule principal
	(declare (salience 99))
	(pregunta (question ?question)(indice ?indice)(atributo ?atributo)(valor ?valor))
    =>
	
	
	(if (eq ?*intSeguir* 1) then
		;(bind ?index(random 1 ?*intNumeroTotalPreguntas*))
		;(hacerPregunta ?question ?indice ?atributo ?valor)
		;(pedirRespuesta)
		(programa)
	)	
)


