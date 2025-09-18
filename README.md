# Prácticas de Paradigmas de Programación

Este repositorio contiene todas las implementaciones realizadas en la asignatura **Paradigmas de Programación** durante el curso 2024/2025 por *Marcos Zúñiga Ituarte*.  
Todas las prácticas fueron desarrolladas en el lenguaje de programación **OCaml**, que fue el utilizado en la asignatura.  
Aunque el contenido puede consultarse libremente, se recomienda leer este README, donde se explica la [organización del repositorio](#organización) y se recogen los [errores detectados en las prácticas](#errores).


## Organización

- El repositorio se divide en un conjunto de carpetas.  
  - Las primeras se llaman **Entregas** y contienen los archivos completos tal como se entregaron en cada conjunto de prácticas.  
  - Después están las carpetas de **Prácticas**, cada una corresponde a una práctica individual de la asignatura.  
    - Algunas prácticas se dividen en subprácticas (numeradas en las carpetas).  
    - Otras contienen directamente los distintos archivos.

- Además de las prácticas obligatorias, la asignatura incluía dos tareas voluntarias llamadas **Desafíos**.  
  - En estos casos también se incluyen los enunciados.  
  - El **primer desafío (Hanoi)** funcionaba correctamente, pero al aumentar el número de discos se volvía muy lento y podía fallar o no encontrar solución a partir de más de 4 discos.  
  - El **segundo desafío (Solo Chess)** está implementado de forma más “artística”. Aunque no se vio en clase, se utilizó el paradigma de **orientación a objetos en OCaml** para experimentar con él, ofreciendo una implementación desde otra manera de pensar.
- Además, se encontraron algunos archivos que no fue posible clasificar.  
  Estos se han dejado en la raíz del proyecto por si pudieran resultar de utilidad.


## Errores

### Entrega 2
- **P4 (fast_fibto)**  
  - La implementación utilizaba el operador `;`, que no estaba permitido.  
  - Se eliminó la comprobación de errores que debía incluirse.

- **P6 (hanoi)**  
  - La función `move` no respetaba exactamente el formato exigido (por ejemplo, en el movimiento de 1 a 2).  

---

### Entrega 3
- **Folding**  
  - Las funciones `iprod` y `fprod` lanzaban excepción en listas vacías, cuando debían devolver 1 y 1.0 (elemento neutro).  

- **Hanoi Plus**  
  - La función `move` permitía mover desde una torre vacía (ilegal).  
  - `move_sequence` heredaba ese error y además no lanzaba excepción en estado inválido.  

- **Tail**  
  - La función `concat` invertía el orden de las listas internas (ejemplo: `concat [[1];[2;3]]`).  
  - `fold_right` se implementó como `List.fold_right`, que no es terminal.  
  - `sublists` fallaba con lista vacía.

---

### Entrega final
- **Galopada**  
  - Se producía un error al trabajar con una lista vacía de peones cuando el caballo podía ponerse en cualquier posicion del tablero.  

- **stBinTree**  
  - `find_in_depth` y `find_in_depth_opt` podían lanzar excepciones (`Failure "left_b"` o `"right_b"`) por falta de control.  
  - `replace_when` y `cut_below` presentaban el mismo problema.  
  - `from_bin` solo comprobaba las ramas, no el árbol entero.  

- **gTree**  
  - `breadth_find` estaba implementada igual que `find_in_depth`, aunque son recorridos distintos.  
    - Ejemplo: en el árbol `GT (1, [GT (1, [GT (0, [])]); GT (5, [])])` debía devolver `5`, pero devolvía `0`.  
  - En `for_all`, un error de copia/pega hacía que la llamada recursiva en `aux` usara `exists`.  
  - `from_stBin` no controlaba las excepciones de `left_b` o `right_b`.
