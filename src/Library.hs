module Library where
import PdePreludat

-- Definir mis tipos
type Edad = Number
type Dinero = Number
type Muchedumbre = [Persona]
type Semilla = Number
type PepesInfinitos = [Persona]

-- Definir mis tipos data
data Persona = Persona {
    edad :: Edad,
    dinero :: Dinero
} deriving Show

-- Inicializar un tipo data
pepe = Persona 18 0
claudia = Persona 36 100
esteban = Persona 40 2

muchedumbre :: Muchedumbre
muchedumbre = [pepe, claudia, esteban]

numerosLocos :: [Number]
numerosLocos = [1,2,3]

pepesInfinitos :: PepesInfinitos
pepesInfinitos = repeat pepe

-- Funcion que devuelve un elemento de la variable
edadPersona :: Persona -> Edad
edadPersona persona = edad persona -- elementoDeLaVariable Variable

-- Funcion que usa aplicación parcial
esMayorDe18 :: Edad -> Bool
esMayorDe18 = (> 18) -- esMayorDe18 edad = edad > 18 / esMayorDe18 edad = (> 18)

-- Funcion que usa composición y aplicación parcial
esMayorDeEdad :: Persona -> Bool
esMayorDeEdad = (esMayorDe18 . edadPersona) 

-- Funcion que "modifica" una Persona
darDinero :: Persona -> Persona
darDinero persona = persona {dinero = dinero persona + cuantoDar persona}

-- Funcion que usa guardas
cuantoDar :: Persona -> Dinero
cuantoDar persona
    | esMayorDeEdad persona = 10000
    | otherwise =  100

-- Funcion recursiva
plataDeTodos :: Muchedumbre -> [Dinero]
plataDeTodos [] = [] --Caso borde
plataDeTodos (persona:personas) = dinero persona:plataDeTodos personas

-- Funciones que usan Orden Superior
edades :: Muchedumbre -> [Edad]
edades = map edad 

puedenBeberAlcohol :: Muchedumbre -> Muchedumbre
puedenBeberAlcohol = filter esMayorDeEdad

-- Función que usa Orden Superior y composición
sumaDeEdades :: Muchedumbre -> Number
sumaDeEdades = sum.map edad

-- Función que usa foldl
restaAIzquierdaConSemilla :: Semilla -> [Number] -> Number
restaAIzquierdaConSemilla = foldl (-) 
-- Función que usa foldl1
restaAIzquierdaSinSemilla :: [Number] -> Number
restaAIzquierdaSinSemilla = foldl1 (-) 
-- Función que usa foldr
restaADerechaConSemilla :: Semilla -> [Number] -> Number
restaADerechaConSemilla = foldr (-) 
-- Función que usa foldr1
restaADerechaSinSemilla :: [Number] -> Number
restaADerechaSinSemilla = foldr1 (-) 

-- Lazy Evaluation: Evaluar los argumentos a medida que los voy necesitan
--Si la evaluación de los argumentos es verificada entonces la función converge a un resultado
-- a pesar de diverger al ser infinita

-- Función que isa Lazy Evaluation
pepesInfinitosSonMayoresDe18 :: PepesInfinitos -> Bool
pepesInfinitosSonMayoresDe18 = all esMayorDeEdad -- Devuelve false porque al ser una lista repetida de pepes, y al no ser mayor de edad, no es necesario seguir evaluando