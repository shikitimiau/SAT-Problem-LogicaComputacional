import Graficas
import SAT.MiniSat (solve, Formula(..))
import Data.Map (Map, toList)

import qualified Data.Map as Map

type Color = Int
type Coloracion = [(Vertice, Color)]

-- Genera las variables proposicionales para cada vértice y color
variables :: [Vertice] -> Color -> [[Formula (Vertice, Color)]]
variables vs k = [[Var (i, j) | j <- [1..k]] | i <- vs]

-- Restricción 1: Cada vértice debe tener al menos un color
restriccion1 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion1 vs k = [Some [Var (i, j) | j <- [1..k]] | i <- vs]

-- Restricción 2: Cada vértice debe tener a lo sumo un color
restriccion2 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion2 vs k = [(Not (Var (i, j))) :||: (Not (Var (i, j'))) | i <- vs, j <- [1..k], j' <- [1..k], j /= j']

-- Restricción 3: Vértices adyacentes no pueden tener el mismo color
restriccion3 :: [Arista] -> Color -> [Formula (Vertice, Color)]
restriccion3 es k = [(Not (Var (i, j))) :||: (Not (Var (i', j))) | (i, i') <- es, j <- [1..k]]

{-- Restricción 4: Se deben utilizar todos los colores.
restriccion4 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion4 vs k = --}

-- Definir la función para obtener solo los elementos cuyo segundo valor sea True
filtrarTrue :: Map (Vertice, Color) Bool -> [(Vertice, Color)]
filtrarTrue m = [ (v, c) | ((v, c), True) <- toList m ]

-- Función principal que construye la fórmula y obtiene las k-coloraciones
kColoracion :: Color -> Grafica -> [Coloracion]
kColoracion k g = case solve formula of
    Nothing    -> []  -- No hay solución
    Just sol   -> [filtrarTrue sol]
  where
    vs = vertices g
    es = aristas g
    -- Construcción de la fórmula con todas las restricciones
    formula = All (restriccion1 vs k ++ restriccion2 vs k ++ restriccion3 es k {--++ restriccion4 vs k --})



solucion k (Grafica vs es) = solve (All (restriccion1 vs k ++ restriccion2 vs k ++ restriccion3 es k {--++ restriccion4 vs k --}))

{-- ------------------
% Ejemplos de graficas %
  -------------------- --}
graficaHexagonoTriangulado = Grafica [1,2,3,4,5,6,7] [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,3),(3,4),(4,5),(5,6),(6,7),(7,2)]

ejemploGrafHexagonoTriangulado = solucion 7 graficaHexagonoTriangulado
-- [((1,1),False),((1,2),False),((1,3),True),((2,1),False),((2,2),True),((2,3),False),((3,1),True),((3,2),False),((3,3),False),((4,1),False),((4,2),True),((4,3),False),((5,1),True),((5,2),False),((5,3),False),((6,1),False),((6,2),True),((6,3),False),((7,1),True),((7,2),False),((7,3),False)])

satK3GrafHex = kColoracion 7 graficaHexagonoTriangulado
-- [[(1,3),(2,2),(3,1),(4,2),(5,1),(6,2),(7,1)]]



graficaZ = Grafica [1,2,3,4] [(1,2), (2,3), (3,4)]
ejemploZ = solucion 4 graficaZ
satK2GrafZ = kColoracion 4 graficaZ