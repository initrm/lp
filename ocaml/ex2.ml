exception UnknownSourceScale

type temperature = { scale: string; value: float }

let to_celsius temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (5. /. 4.)
    | { scale = s; value = v } when String.equal s "fahrenheit" -> (v +. (-. 32.)) *. (5. /. 9.)
    | { scale = s; value = v } when String.equal s "kelvin" -> v +. (-. 273.15)
    | { scale = s; value = v } when String.equal s "rankine" -> (v +. (-. 491.67)) *. (5. /. 9.)
    | { scale = s; value = v } when String.equal s "delisle" -> 100. -. v *. (2. /. 3.)
    | { scale = s; value = v } when String.equal s "newton" -> v *. (100. /. 33.)
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (40. /. 21.)
    | _ -> raise UnknownSourceScale
  in { scale = "celsius"; value = (get_val temperature) };;

let to_fahrenheit temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (9. /. 4.) +. 32.
    | { scale = s; value = v } when String.equal s "celsius" -> v *. (9. /. 5.) +. 32.
    | { scale = s; value = v } when String.equal s "kelvin" -> v *. (9. /. 5.) -. 459.67
    | { scale = s; value = v } when String.equal s "rankine" -> v -. 459.67
    | { scale = s; value = v } when String.equal s "delisle" -> 212. -. v *. (6. /. 5.)
    | { scale = s; value = v } when String.equal s "newton" -> v *. (60. /. 11.) +. 32.
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (24. /. 7.) +. 32.
    | _ -> raise UnknownSourceScale
  in { scale = "fahrenheit"; value = (get_val temperature) };;

let to_kelvin temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (5. /. 4.) +. 273.15
    | { scale = s; value = v } when String.equal s "celsius" -> v +. 273.15
    | { scale = s; value = v } when String.equal s "fahrenheit" -> v +. (459.67) *. (5. /. 9.)
    | { scale = s; value = v } when String.equal s "rankine" -> v *. (5. /. 9.)
    | { scale = s; value = v } when String.equal s "delisle" -> 373.15 +. (-. v) *. (2. /. 3.)
    | { scale = s; value = v } when String.equal s "newton" -> v *. (100. /. 33.) +. 273.15
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (40. /. 21.) +. 273.15
    | _ -> raise UnknownSourceScale
  in { scale = "kelvin"; value = (get_val temperature) };;

let to_rankine temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (9. /. 4.) +. 491.67
    | { scale = s; value = v } when String.equal s "celsius" -> (v +. 273.15) *. (9. /. 5.)
    | { scale = s; value = v } when String.equal s "fahrenheit" -> v +. 459.67
    | { scale = s; value = v } when String.equal s "kelvin" -> v *. (9. /. 5.)
    | { scale = s; value = v } when String.equal s "delisle" -> 671.67 +. (-. v) *. (6. /. 5.)
    | { scale = s; value = v } when String.equal s "newton" -> v *. (60. /. 11.) +. 491.67
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (24. /. 7.) +. 491.67
    | _ -> raise UnknownSourceScale
  in { scale = "rankine"; value = (get_val temperature) };;

let to_delisle temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "reaumur" -> (80. +. (-. v)) *. (15. /. 8.)
    | { scale = s; value = v } when String.equal s "celsius" -> (100. +. (-. v)) *. (3. /. 2.)
    | { scale = s; value = v } when String.equal s "fahrenheit" -> (212. +. (-. v)) *. (5. /. 6.)
    | { scale = s; value = v } when String.equal s "kelvin" -> (373.15 +. (-. v)) *. (3. /. 2.)
    | { scale = s; value = v } when String.equal s "rankine" -> (671.67 +. (-. v)) *. (5. /. 6.)
    | { scale = s; value = v } when String.equal s "newton" -> (33. +. (-. v)) *. (50. /. 11.)
    | { scale = s; value = v } when String.equal s "romer" -> (60. +. (-. v)) *. (20. /. 7.)
    | _ -> raise UnknownSourceScale
  in { scale = "delisle"; value = (get_val temperature) };;

let to_newton temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "celsius" -> v *. (33. /. 100.)
    | { scale = s; value = v } when String.equal s "fahrenheit" -> (v +. (-. 32.)) *. (11. /. 60.)
    | { scale = s; value = v } when String.equal s "kelvin" -> (v +.  (-. 273.15)) *. (33. /. 100.)
    | { scale = s; value = v } when String.equal s "rankine" -> (v +. (-. 491.67)) *. (11. /. 60.)
    | { scale = s; value = v } when String.equal s "delisle" -> 33. +. (-. v) *. (11. /. 50.)
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (33. /. 80.)
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (22. /. 35.)
    | _ -> raise UnknownSourceScale
  in { scale = "newton"; value = (get_val temperature) };;

let to_reaumur temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "celsius" -> v *. (4. /. 5.)
    | { scale = s; value = v } when String.equal s "fahrenheit" -> (v +. (-. 32.)) +. (4. /. 9.)
    | { scale = s; value = v } when String.equal s "kelvin" -> (v +.  (-. 273.5)) *. (4. /. 5.)
    | { scale = s; value = v } when String.equal s "rankine" -> (v +. (-. 491.67)) *. (4. /. 9.)
    | { scale = s; value = v } when String.equal s "delisle" -> 80. +. ((-. v) *. (8. /. 15.))
    | { scale = s; value = v } when String.equal s "newton" -> v *. (80. /. 33.)
    | { scale = s; value = v } when String.equal s "romer" -> (v +. (-. 7.5)) *. (32. /. 21.)
    | _ -> raise UnknownSourceScale
  in { scale = "reaumur"; value = (get_val temperature) };;

let to_romer temperature = 
  let get_val = function
    | { scale = s; value = v } when String.equal s "celsius" -> v *. (21. /. 40.) +. 7.5
    | { scale = s; value = v } when String.equal s "fahrenheit" -> (v +. (-. 32.)) *. (7. /. 24.) +. 7.5
    | { scale = s; value = v } when String.equal s "kelvin" -> (v +.  (-. 273.15)) *. (21. /. 40.) +. 7.5
    | { scale = s; value = v } when String.equal s "rankine" -> (v +. (-. 491.67)) *. (7. /. 24.) +. 7.5
    | { scale = s; value = v } when String.equal s "delisle" -> 60. +. ((-. v) *. (7. /. 20.))
    | { scale = s; value = v } when String.equal s "newton" -> v *. (35. /. 22.) +. 7.5
    | { scale = s; value = v } when String.equal s "reaumur" -> v *. (21. /. 32.) +. 7.5
    | _ -> raise UnknownSourceScale
  in { scale = "romer"; value = (get_val temperature) };;

(* returns the given temperature converted in each of the 8 temperature scales  *)
let conversion_table scale value = 
  let temp = { scale = scale; value = value } 
  in let conv = function
    | { scale = s; _ } when String.equal s "celsius" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "fahrenheit" -> [
            (to_celsius temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "kelvin" -> [
            (to_fahrenheit temp); 
            (to_celsius temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "rankine" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_celsius temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "delisle" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_celsius temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "newton" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_celsius temp); 
            (to_reaumur temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "reaumur" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_celsius temp);
            (to_romer temp)
          ]
    | { scale = s; _ } when String.equal s "romer" -> [
            (to_fahrenheit temp); 
            (to_kelvin temp); 
            (to_rankine temp); 
            (to_delisle temp); 
            (to_newton temp); 
            (to_reaumur temp);
            (to_celsius temp)
          ]
    | _ -> raise UnknownSourceScale
  in conv temp;;

let rec print_temp_table = function
  | [] -> ()
  | hd::tl -> print_endline (hd.scale^" "^(Printf.sprintf "%.5f" hd.value)); print_temp_table tl;;