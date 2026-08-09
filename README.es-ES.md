

# Algoritmo de la flor de Edmonds para emparejamiento de peso máximo en grafos no dirigidos

Esta biblioteca implementa el algoritmo de la flor (Blossom) que calcula un emparejamiento de peso máximo en un grafo no dirigido en O(número de nodos ** 3).

Fue portado a partir del código en Python escrito por Joris van Rantwijk, incluido en la biblioteca de grafos NetworkX, y modificado.

## Cómo empezar

Añade las dependencias necesarias a tu proyecto:

```clojure
[ageneau/blossom "0.1.4"]
[aysylu/loom "1.0.2"]
```

## Uso


```clojure
(ns test.blossom
  (:require [blossom.max-weight-matching :as mwm]
            [blossom.matching :as m]
            [loom.graph :as lg]))
 
(def edges [[1 2 2][1 3 -2][2 3 1][2 4 -1][3 4 -6]])
(def g (-> (lg/weighted-graph) (lg/add-edges* edges)))

;; Compute a maximum weigted matching
(def maxw-matching (mwm/max-weight-matching edges))
;; => #{#{1 2}}

;; Compute the maximum-cardinality matching with maximum weight among all
;; maximum-cardinality matchings.
(def maxc-matching (mwm/max-weight-matching edges {:max-cardinality true}))
;; => #{#{4 2} #{1 3}}

;; Compute a maximal matching (not necessarily max weight)
(def max-matching (m/maximal-matching g))
;; => #{#{4 3} #{1 2}}

(m/is-matching? g maxw-matching)
;; => true

(m/is-maximal-matching? g maxw-matching)
;; => false

(m/is-maximal-matching? g maxc-matching)
;; => true

(m/is-maximal-matching? g max-matching)
;; => true


```

## Pruebas unitarias

Algunas pruebas unitarias se generan automáticamente instrumentando el código Python en el que se basa este proyecto, ejecutando las pruebas unitarias de Python y volcando los datos internos a JSON, que luego se comparan con las estructuras de datos equivalentes en Clojure.

Los datos JSON para estas pruebas unitarias se pueden generar ejecutando:

```
make -C python
```

Para ejecutar las pruebas unitarias de Clojure:

```
lein do clean, test
```

Para ejecutar las pruebas unitarias de Clojurescript:

```
lein do clean, doo node node-test once
```


## Licencia

Copyright &copy; NetworkX developers.
Copyright (C) 2004-2018 by
    Aric Hagberg <hagberg@lanl.gov>
    Dan Schult <dschult@colgate.edu>
    Pieter Swart <swart@lanl.gov>

Copyright &copy; 2008 by
    Joris van Rantwijk.

Copyright &copy; 2011 by
    Nicholas Mancuso <nick.mancuso@gmail.com>

Copyright &copy; 2018 Sylvain Ageneau

Todos los derechos reservados.

Este proyecto está licenciado bajo la [Licencia BSD de 3 Cláusulas "Nueva" o "Revisada"][license].

[license]: https://opensource.org/licenses/BSD-3-Clause
