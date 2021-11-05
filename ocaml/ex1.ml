let alkaline_earth_metals = [56; 4; 12; 88; 38; 20] and noble_gases = [2; 10; 18; 36; 54; 86];;

(* finds the biggest element inside the list *)
let rec highest_aem ?(c_max=0) = function
  [] -> c_max
  | hd::tl -> if c_max < hd then highest_aem ~c_max:hd tl else highest_aem ~c_max:c_max tl;;

(* sorts the list (insertion sort) *)
let rec sorted_insert element = function
    [] -> [element]
    | hd::tl -> if hd > element then element::hd::tl else hd::(sorted_insert element tl);;

let rec sort = function
    [] -> []
    | hd::tl -> sorted_insert hd (sort tl);;

(* merges two lists togheter *)
let rec merge list1 list2 = match list1, list2 with
  [], [] -> []
  | hd1::tl1, hd2::tl2 -> hd1::hd2::(merge tl1 tl2)
  | hd1::tl1, [] -> list1
  | [], hd2::tl2 -> list2;;

(* since the exercise says "put into a list the ATOMIC NUMBERS" and not an element which has atomic number
and name, this function helps, given an atomic number, to retrieve its element name *)
let atomic_number_to_name = function
    4 -> "beryllium"
  | 12 -> "magnesium"
  | 20 -> "calcium" 
  | 38 -> "strontium"
  | 56 -> "barium"
  | 88 -> "radium"
  | 2 -> "helium"
  | 10 -> "neon"
  | 18 -> "argon"
  | 36 -> "krypton"
  | 54 -> "xenon"
  | 86 -> "radon"
  | _ -> "not in this exercise";;

(* prints the elements name given a list of atomic numbers *)
let rec print_elements = function 
  [] -> ()
  | hd::tl -> print_string ((atomic_number_to_name hd)^" "); print_elements tl;;

(* merges the given lists, sorts the merged list and prints the name of the elements *)
let sorted_merge_and_print list1 list2 = print_elements(sort (merge list1 list2));;