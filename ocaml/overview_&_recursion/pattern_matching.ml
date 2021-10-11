(* Funzione per invertire il valore di un booleano tramite pattern matching *)
let invert x =
    match x with
        | true -> false
        | false -> true;;

(* Versione compatta della medesima funzione con pattern matching *)
let invert´ = function 
    true -> false | false -> true;;