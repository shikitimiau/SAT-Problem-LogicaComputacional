module Graficas (
    Grafica(..),
    Vertice,
    Arista,
    crearGrafica,
    agregarVertice,
    agregarArista,
    vertices,
    aristas,
    vecinos
) where

import Data.List (nub)

-- Vertice | Representado como un valor entero
type Vertice = Int

-- Arista | Es una tupla que asocia dos vértices
type Arista = (Vertice, Vertice)

-- Grafica | Definida como un conjunto de vertices y aristas.
-- En nuestro caso estos conjuntos los representamos como listas, y posteriormente nos aseguraremos que no haya elementos repetidos en estas.
data Grafica = Grafica [Vertice] [Arista] deriving Show

-- crearGrafica | Crear una gráfica vacía
-- Inicializa los vertices y aristas como listas vacias
crearGrafica :: Grafica
crearGrafica = Grafica [] []

-- agregarVertice| Agrega un vértice a una gráfica
-- Nos aseguramos de quitar repeticiones en los vertices
agregarVertice :: Vertice -> Grafica -> Grafica
agregarVertice v (Grafica vs es) = Grafica (nub (v:vs)) es

-- agregarArista | Agrega una arista a una gráfica
-- Nos aseguramos de quitar repeticiones en los vertices y aristas
agregarArista :: Arista -> Grafica -> Grafica
agregarArista e@(v1, v2) (Grafica vs es) = Grafica (nub (v1:v2:vs)) (nub (e:es))

-- vertices | Obtener la lista de vértices de una gráfica
vertices :: Grafica -> [Vertice]
vertices (Grafica vs _) = vs

-- aristas | Obtener la lista de aristas de una gráfica
aristas :: Grafica -> [Arista]
aristas (Grafica _ es) = es

-- vecinos | Obtener los vecinos de un vértice
vecinos :: Vertice -> Grafica -> [Vertice]
vecinos v (Grafica _ es) = [u | (u, w) <- es, w == v] ++ [w | (u, w) <- es, u == v]
