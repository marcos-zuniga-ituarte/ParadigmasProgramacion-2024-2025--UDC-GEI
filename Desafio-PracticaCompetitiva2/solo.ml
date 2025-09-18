(*Es muy destacable el uso de clases debido a la capacidad de estas en ocaml: entre
iteraciones se usan punteros, y este problema requiere de encontrar la solucion
futura para encontrar la actual, y esto permite que en las direcciones de memoria
de los objetos futura, se tengan en cuenta de forma sencilla en la actual*)

class virtual figura (desc : string) =
  object (self)
    (* Atributos de la clase padre figura*)
    val mutable posicion : char * int = (desc.[1], int_of_char desc.[2] - int_of_char '0')
    val mutable tipo : char = desc.[0]
    val mutable numMovs : int = 1

    (* Métodos de la clase figura*)

    (* Validar si una posición está dentro del tablero, muy importante al crear la instancia de la figura*)
    method private posicion_valida (desc : string) =
      if String.length desc <> 3 then
        raise (Failure "Error al contruir en figura, descripción debe tener exactamente 3 caracteres");
      let pos_char = desc.[1] in
      let pos_int = int_of_char desc.[2] - int_of_char '0' in
      if pos_char < 'a' || pos_char > 'h' || pos_int < 1 || pos_int > 8 then
        raise (Failure "Posición fuera del tablero")

    (* Constructor para verificar entrada válida *)
    initializer
      self#posicion_valida desc

    (* Obtener la posición actual en la que se encuentra la figura*)
    method getPosition : char * int = posicion

    (* Actualizar la posición, usado para revertir los cambios en caso de que
    un camino no sea posible, ese es el motivo por el que resta uno a numMovs *)
    method setPosition (nueva_posicion : char * int) : unit =
      let (columna, fila) = nueva_posicion in
      if columna < 'a' || columna > 'h' || fila < 1 || fila > 8 then
        failwith "Posición fuera del tablero"
      else (
        posicion <- nueva_posicion;
        if numMovs > 0 then numMovs <- numMovs - 1 (* Restar uno a numMovs si es mayor que 0 *)
      )

    (* Verificar si hay obstáculos en el camino hacia el destino a comer, esto es importante para la reina (q),
    el alfil (b), y la torre (r) *)
    method private hay_obstaculos (destino : char * int) (lista_figuras : figura list) : bool =
      let (pos_x, pos_y) = posicion in
      let (dest_x, dest_y) = destino in
      let rec avanzar (x, y) paso_x paso_y =
        if (x, y) = destino then false
        else if List.exists (fun f -> f#getPosition = (x, y)) lista_figuras then true
        else
          let nuevo_x = char_of_int (int_of_char x + paso_x) in
          let nuevo_y = y + paso_y in
          if nuevo_x < 'a' || nuevo_x > 'h' || nuevo_y < 1 || nuevo_y > 8 then false
          else avanzar (nuevo_x, nuevo_y) paso_x paso_y
      in
      match tipo with
      | 'b' -> 
          if abs (int_of_char pos_x - int_of_char dest_x) <> abs (pos_y - dest_y) then false
          else
            let paso_x = if pos_x < dest_x then 1 else -1 in
            let paso_y = if pos_y < dest_y then 1 else -1 in
            avanzar (char_of_int (int_of_char pos_x + paso_x), pos_y + paso_y) paso_x paso_y
      | 'r' -> 
          if pos_x <> dest_x && pos_y <> dest_y then false
          else
            let paso_x = if pos_x = dest_x then 0 else if pos_x < dest_x then 1 else -1 in
            let paso_y = if pos_y = dest_y then 0 else if pos_y < dest_y then 1 else -1 in
            avanzar (char_of_int (int_of_char pos_x + paso_x), pos_y + paso_y) paso_x paso_y
      | 'q' ->
          if pos_x = dest_x || pos_y = dest_y || abs (int_of_char pos_x - int_of_char dest_x) = abs (pos_y - dest_y) then
            let paso_x = if pos_x = dest_x then 0 else if pos_x < dest_x then 1 else -1 in
            let paso_y = if pos_y = dest_y then 0 else if pos_y < dest_y then 1 else -1 in
            avanzar (char_of_int (int_of_char pos_x + paso_x), pos_y + paso_y) paso_x paso_y
          else false
      | _ -> false

    (* Verificar si puede comer la figura actual a otra figura, si puede entonces
    devuelve true ademas de actualizar su posicion a la de la figura comida *)
    method come (otra : figura) (lista_figuras : figura list) : bool =
      if numMovs = 3 then false
      else if otra#getType = 'k' then false (* No se puede comer al rey *)
      else
        let pos_otra = otra#getPosition in
        (* Verificar si el camino esta limpio para las figuras de alfil (b), reina (q) o torre (r) *)
        if (tipo = 'b' || tipo = 'q' || tipo = 'r') && self#hay_obstaculos pos_otra lista_figuras
          then false
        else if List.exists (fun pos -> pos = pos_otra) self#getPossibleDestinations then (
          posicion <- pos_otra;
          numMovs <- numMovs + 1;
          true
        ) else
          false

    (* Método virtual para obtener posibles destinos, lo deben sobreescribir las clases hijas *)
    method virtual getPossibleDestinations : (char * int) list

    (* Obtener el tipo de la figura *)
    method getType : char = tipo
  end
;;

(* Clase hija: peon *)
class peon (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let posibles = [
        (char_of_int (int_of_char pos_char - 1), pos_int + 1);
        (char_of_int (int_of_char pos_char + 1), pos_int + 1);
      ] in
      (* Filtrar casillas fuera del tablero (8*8) *)
      List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      ) posibles
  end
;;

(* Clase hija: caballo *)
class caballo (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let posibles = [
        (char_of_int (int_of_char pos_char - 2), pos_int + 1);
        (char_of_int (int_of_char pos_char - 2), pos_int - 1);
        (char_of_int (int_of_char pos_char + 2), pos_int + 1);
        (char_of_int (int_of_char pos_char + 2), pos_int - 1);
        (char_of_int (int_of_char pos_char - 1), pos_int + 2);
        (char_of_int (int_of_char pos_char - 1), pos_int - 2);
        (char_of_int (int_of_char pos_char + 1), pos_int + 2);
        (char_of_int (int_of_char pos_char + 1), pos_int - 2);
      ] in
      (* Filtrar casillas fuera del tablero (8*8) *)
      List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      ) posibles
  end
;;

(* Clase hija: rey *)
class rey (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let posibles = [
        (char_of_int (int_of_char pos_char - 1), pos_int);
        (char_of_int (int_of_char pos_char + 1), pos_int);
        (pos_char, pos_int + 1);
        (pos_char, pos_int - 1);
        (char_of_int (int_of_char pos_char - 1), pos_int + 1);
        (char_of_int (int_of_char pos_char - 1), pos_int - 1);
        (char_of_int (int_of_char pos_char + 1), pos_int + 1);
        (char_of_int (int_of_char pos_char + 1), pos_int - 1);
      ] in
      (* Filtrar casillas fuera del tablero (8*8) *)
      List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      ) posibles
  end
;;

(* Clase hija: dama *)
class dama (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let direccionesTransformations = [
        (-1, 0); (1, 0);
        (0, -1); (0, 1);
        (-1, -1); (1, 1);
        (-1, 1); (1, -1);
      ] in
      List.concat (
        List.map (fun (dx, dy) ->
          List.init 8 (fun i ->
            (char_of_int (int_of_char pos_char + (i + 1) * dx),
             pos_int + (i + 1) * dy)
          )
        ) direccionesTransformations
      )
      (* Filtrar casillas fuera del tablero (8*8) *)
      |> List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      )
  end
;;

(* Clase hija: alfil *)
class alfil (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let direcciones = [
        (-1, -1); (1, 1);
        (-1, 1); (1, -1)
      ] in
      List.concat (
        List.map (fun (dx, dy) ->
          List.init 8 (fun i ->
            (char_of_int (int_of_char pos_char + (i + 1) * dx),
             pos_int + (i + 1) * dy)
          )
        ) direcciones
      )
      (* Filtrar casillas fuera del tablero (8*8) *)
      |> List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      )
  end
;;

(* Clase hija: torre *)
class torre (desc : string) =
  object (self)
    inherit figura desc

    (* Redefinir posibles destinos *)
    method getPossibleDestinations : (char * int) list =
      let (pos_char, pos_int) = self#getPosition in
      let direcciones = [
        (-1, 0); (1, 0);
        (0, -1); (0, 1)
      ] in
      List.concat (
        List.map (fun (dx, dy) ->
          List.init 8 (fun i ->
            (char_of_int (int_of_char pos_char + (i + 1) * dx),
             pos_int + (i + 1) * dy)
          )
        ) direcciones
      )
      (* Filtrar casillas fuera del tablero (8*8) *)
      |> List.filter (fun (c, i) ->
        c >= 'a' && c <= 'h' && i >= 1 && i <= 8
      )
  end
;;

(* funcion para crear la figura con su correspondiente constructor,
el contructor ya validara que sea una posicion valida *)
let crear_figura (desc : string) =
  match desc.[0] with
  | 'k' -> (new rey desc :> figura)
  | 'q' -> (new dama desc :> figura)
  | 'b' -> (new alfil desc :> figura)
  | 'n' -> (new caballo desc :> figura)
  | 'r' -> (new torre desc :> figura)
  | 'p' -> (new peon desc :> figura)
  | _ -> raise (Failure "Crear figura, no existe el tipo descrito")
;;

(* funcion para crear la lista de objetos de figuras dada la lista de strings de las figuras *)
let crear_lista_figuras (descripciones : string list) =
  List.map crear_figura descripciones
;;

(* funcion para eliminar un objeto de una lista, muy destacable que la comprobacion de si 2
instanciashabria que saber como se hacen sin embargo, a esta funcion se la va a llamar siempre
con la misma lista de objetos sin cambiar sus direcciones en memoria, y ademas el objeto a
eliminar siempre va a estar actualizado en la lista debido a la naturaleza de ocaml de como
funcionan las clases, por ello nos da igual si la comparacion se hace por identidad o por referencia *)
let rec remove elemento =function
    [] -> []
  | h::t -> if h = elemento then t else h::remove elemento t
;;

(* funcion para calcular la solucion, pero recibe una lista de objetos y devuelve una lista
de movimientos que habra que transformar agrupandolos en pares, devuelve la lista vacia si no encontro solucion *)
let rec solo_chess_aux listaObjetos= 
    let rec aux1 = function (* bucle1 que recorre la lista de figuras *)
        [] -> []
      | h1::t1 ->   let rec aux2 =  function (* bucle2 que recorre la lista de figuras *)
                          [] -> []
                        | h2::t2 -> 
                                  let posicionIni=h1#getPosition in
                                  if h1#come h2 listaObjetos (* si se come pues se come *)
                                  then  let solucion1 = solo_chess_aux ((remove h2 listaObjetos)) in (* se calcula la solucion en el resto eliminando el objeto comido *)
                                        if solucion1 = [] && List.length listaObjetos <> 2
                                        then let _ = h1#setPosition posicionIni in aux2 t2 (* si no se encuentra solucion entonces se revierten los cambios realizados en el objeto, el setPosition tambien resta 1 al numMovs, ademas se itera con el resto de elementos para continuar con la busqueda *)
                                        else posicionIni::h2#getPosition::solucion1 (* si se encontro solucion entonces se concatena la solucion *)
                                  else  aux2 (t2) (* si no se cumplen ninguna de las anteriores entonces se itera con el resto de elementos para buscar la solucion *)
                    in  let solucion=aux2 (remove h1 listaObjetos) in
                        if solucion=[] then aux1 t1 else solucion (* si la solucion es vacia se itera con el resto de elementos, sino la devuelve *)
    in if List.length listaObjetos = 1 then [] else aux1 listaObjetos (* caso base de la recursividad, si se llega a un elemento devuelve la lista vacia porque no se requieren hacer movimientos, sino se itera con los elementos *)
;;

(* Convertir un numero a carácter *)
let int_to_char n = char_of_int (n + int_of_char '0')

(* Convertir pares en la lista a cadenas, para trandformar lo generado por
solo_chess_aux en el formato esperado *)
let convertir_pares lista =
  let rec aux = function
    | [] -> []  (* Caso base: lista vacía *)
    | (c1, n1) :: (c2, n2) :: t ->
        (* Concatenar el par como cadena *)
        let par = String.make 1 c1 ^ String.make 1 (int_to_char n1) ^
                  String.make 1 c2 ^ String.make 1 (int_to_char n2) in
        par :: aux t  (* Agregar el resultado y continuar con el resto *)
    | _ -> raise (Failure "convertir_pares, la lista debe tener un número par de elementos")
  in
  aux lista
;;

(* funcion principal, que transforma lo recibido a objetos, busca la solucion, y
si la encuentra la transforma en el formato esperado, sino lanza la excepcion Not_found *)
let solo_chess listaString= 
    let objetos = crear_lista_figuras listaString in
    let solucion = solo_chess_aux objetos in
    if solucion = [] then raise Not_found else convertir_pares solucion
;;