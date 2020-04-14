saludar::String -> String
saludar nombre = "hola " ++ nombre

etiquetaSimple::(Int,Int)
etiquetaSimple = (1,2)

funcionSimple::(Int,Int)->(Int,Int)
funcionSimple par = par

funcionSimple2::(Int,Int)->Int
funcionSimple2 par = snd par


funcionSimple3::(Int,Int)->Int
funcionSimple3  ( _ , 0 ) = 0
funcionSimple3  ( 123 , 123 ) = 100000000
funcionSimple3  ( x , y ) = x + y

data Colectivo = UnColectivo {
    nro::Int,
    destino::String,
    origen::String
} deriving Show

data Persona = UnaPersona {
    nombre::String,
    edad:: Int,
    barrio:: String,
    tivoVivienda:: Bool, 
    linea::Char,
    colectivos::[Colectivo]} deriving Show

juan::Persona
juan = UnaPersona "Juan Perez" 20 "boedo" True 'e' [UnColectivo 115 "retiro" "soldati"]

trucho = UnColectivo 99 "casa" "dondeQuiero"

maria::Persona
maria = UnaPersona "Maria Gonzalez" 21 "palermo" False 'd' [trucho, trucho, trucho]
-- cacho::Persona
-- cacho = ("Castaña",60,"montserrat",True,'c')


irAComprar::Persona -> String
irAComprar ( UnaPersona name _ "boedo" _ _ bondis) = saludar name ++ " y no soy de San Lorenzo " ++ origen (head bondis)
irAComprar ( UnaPersona name _ _ False _ _) = saludar name 
irAComprar alguien = irAComprarComun alguien

irAComprarComun::Persona -> String
irAComprarComun alguien = saludar (nombre alguien) ++ " de " ++ barrio alguien

-- comoTeLlamas::Persona -> String
-- comoTeLlamas (nombre,_,_,_,_ ) = nombre

-- enQueBarrioVivis::Persona -> String
-- enQueBarrioVivis (_,_,barrio,_,_ ) = barrio

mudarseVersalles::Persona->Persona
mudarseVersalles persona = mudarse "versalles" persona

mudarse::String -> Persona->Persona
mudarse nuevoBarrio persona  = cumplirAnios (cambiarBarrio nuevoBarrio persona) 

cambiarBarrio::String->Persona->Persona
cambiarBarrio nuevoBarrio (UnaPersona nombre edad _ casa _ _)  =  UnaPersona nombre edad nuevoBarrio casa (buscarLinea nuevoBarrio) []


buscarLinea:: String -> Char
buscarLinea "versalles" = 'a'
buscarLinea "boedo" = 'e'
buscarLinea "almagro" = 'a'
buscarLinea "palermo" = 'd'
buscarLinea _ = 'h'

cumplirAnios:: Persona->Persona
cumplirAnios alguien = cumplirVariosAnios 1 alguien 

cumplirVariosAnios:: Int -> Persona->Persona
cumplirVariosAnios anios alguien = alguien{ edad =  edad alguien +anios}  
--cumplirVariosAnios anios (UnaPersona nombre edad barrio casa linea col) = UnaPersona nombre (edad +anios ) barrio casa linea col 

personas:: [Persona]
personas = [juan, maria]

nombreDelUltimo::[Persona]->String
nombreDelUltimo  vecinos = nombre (last vecinos)


-- elMaximoEstaAlInicio :: [Char] -> Bool
-- elMaximoEstaAlInicio numeros = head numeros == maximum numeros

-- juntamosListas lista1 lista2 = lista1 ++ reverse lista2

-- capicua [] = True
-- capicua [_] = True
-- capicua (cabeza:cola) = cabeza == last cola

-- f (cabeza:cola) = cabeza



