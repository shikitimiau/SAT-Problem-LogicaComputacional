import Graficas
import SAT.MiniSat (solve, solve_all, Formula(..))
import Data.Map (Map, toList)

import qualified Data.Map as Map

type Color = Int
type Coloracion = [(Vertice, Color)]

-- Restricción 1: Cada vértice debe tener al menos un color
restriccion1 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion1 vs k = [Some [Var (i, j) | j <- [1..k]] | i <- vs]

-- Restricción 2: Cada vértice debe tener a lo sumo un color
restriccion2 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion2 vs k = [(Not (Var (i, j))) :||: (Not (Var (i, j'))) | i <- vs, j <- [1..k], j' <- [1..k], j /= j']

-- Restricción 3: Vértices adyacentes no pueden tener el mismo color
restriccion3 :: [Arista] -> Color -> [Formula (Vertice, Color)]
restriccion3 es k = [(Not (Var (i, j))) :||: (Not (Var (i', j))) | (i, i') <- es, j <- [1..k]]

-- Restricción 4: Se deben utilizar todos los colores.
restriccion4 :: [Vertice] -> Color -> [Formula (Vertice, Color)]
restriccion4 vs k = [Some [Var (i, j) | i <- vs] | j <- [1..k]]

-- Definir la función para obtener solo los elementos cuyo segundo valor sea True
filtrarTrue :: Map (Vertice, Color) Bool -> [(Vertice, Color)]
filtrarTrue m = [ (v, c) | ((v, c), True) <- toList m ]

-- Función auxiliar para obtener todas las soluciones
kColoracionAll :: Int -> Grafica -> [Map (Vertice, Int) Bool]
kColoracionAll k g = solve_all formula
  where
    vs = vertices g
    es = aristas g
    -- Construcción de la fórmula con todas las restricciones
    formula = All (restriccion1 vs k ++ restriccion2 vs k ++ restriccion3 es k ++ restriccion4 vs k)

-- Función principal que construye la fórmula y obtiene todas las k-coloraciones
kColoracion :: Int -> Grafica -> [Coloracion]
kColoracion k g = case kColoracionAll k g of
    []      -> []  -- No hay soluciones
    sols    -> map filtrarTrue sols


{-- ------------------
% Ejemplos de graficas %
  -------------------- --}
graficaHexagonoTriangulado = Grafica [1,2,3,4,5,6,7] [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,3),(3,4),(4,5),(5,6),(6,7),(7,2)]

satK3GrafHex = kColoracion 3 graficaHexagonoTriangulado
{-
[[(1,3),(2,2),(3,1),(4,2),(5,1),(6,2),(7,1)],
[(1,2),(2,3),(3,1),(4,3),(5,1),(6,3),(7,1)],
[(1,3),(2,1),(3,2),(4,1),(5,2),(6,1),(7,2)],
[(1,1),(2,2),(3,3),(4,2),(5,3),(6,2),(7,3)],
[(1,2),(2,1),(3,3),(4,1),(5,3),(6,1),(7,3)],
[(1,1),(2,3),(3,2),(4,3),(5,2),(6,3),(7,2)]]
-}

graficaZ = Grafica [1,2,3,4] [(1,2), (2,3), (3,4)]

satK2GrafZ = kColoracion 2 graficaZ
{-
[[(1,2),(2,1),(3,2),(4,1)],
[(1,1),(2,2),(3,1),(4,2)]]
-}

satK4GrafZ = kColoracion 4 graficaZ
{-
[[(1,4),(2,3),(3,2),(4,1)],
 [(1,3),(2,4),(3,2),(4,1)],
 [(1,4),(2,2),(3,3),(4,1)],
 [(1,4),(2,3),(3,1),(4,2)],
 [(1,4),(2,1),(3,2),(4,3)],
 [(1,4),(2,2),(3,1),(4,3)],
 [(1,1),(2,3),(3,4),(4,2)],
 [(1,4),(2,1),(3,3),(4,2)],
 [(1,3),(2,2),(3,4),(4,1)],
 [(1,1),(2,2),(3,4),(4,3)],
 [(1,2),(2,4),(3,3),(4,1)],
 [(1,3),(2,4),(3,1),(4,2)],
 [(1,3),(2,2),(3,1),(4,4)],
 [(1,3),(2,1),(3,4),(4,2)],
 [(1,2),(2,3),(3,4),(4,1)],
 [(1,1),(2,4),(3,3),(4,2)],
 [(1,1),(2,4),(3,2),(4,3)],
 [(1,1),(2,2),(3,3),(4,4)],
 [(1,2),(2,4),(3,1),(4,3)],
 [(1,1),(2,3),(3,2),(4,4)],
 [(1,3),(2,1),(3,2),(4,4)],
 [(1,2),(2,1),(3,4),(4,3)],
 [(1,2),(2,3),(3,1),(4,4)],
 [(1,2),(2,1),(3,3),(4,4)]]
-}




grafica12 = Grafica [1,2,3,4] [(1,2),(1,3),(2,3),(3,4)]
satK3grafica12 = kColoracion 3 grafica12
{-
[[(1,3),(2,1),(3,2),(4,1)],
[(1,2),(2,3),(3,1),(4,2)],
[(1,3),(2,1),(3,2),(4,3)],
[(1,3),(2,2),(3,1),(4,3)],
[(1,2),(2,3),(3,1),(4,3)],
[(1,3),(2,2),(3,1),(4,2)],
[(1,1),(2,3),(3,2),(4,1)],
[(1,2),(2,1),(3,3),(4,1)],
[(1,1),(2,3),(3,2),(4,3)],
[(1,1),(2,2),(3,3),(4,1)],
[(1,1),(2,2),(3,3),(4,2)],
[(1,2),(2,1),(3,3),(4,2)]]
-}
