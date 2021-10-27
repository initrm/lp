(*
Exercise 5: Playing around with Strings.

Define the following functions/operators on strings:

    is_palindrome: string → bool that checks if the string is palindrome, a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...
    operator (-): string → string → string that subtracts the letters in a string from the letters in another string, e.g., "Walter Cazzola"-"abcwxyz" will give "Wlter Col" note that the operator - is case sensitive
    anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram of one or more of the strings in the dictionary
*)

(* operator - (minus) for strings (case sensitive) *)

let rec remove_char ?(c_str="") ?(idx=0) str c  = 
  match idx with
    _ when idx == (String.length str) -> c_str
    | _ -> 
      if (str.[idx]) == c then remove_char ~c_str:c_str ~idx:(idx+1) str c 
      else remove_char ~c_str:(c_str^(String.sub str idx 1)) ~idx:(idx+1) str c;;

let rec minus str1 str2 = 
  match String.length str2 with
    0 -> str1
    | _ -> minus (remove_char str1 str2.[0]) (String.sub str2 1 ((String.length str2) + (-1)));;

let (-) str1 str2 = minus str1 str2;;

(* finds all the symbols which are not alpha into the strings *)
let rec find_non_alpha_chars ?(symbols="") str =
  match String.length str with
    0 -> symbols
    | _ -> 
      if (str.[0] >= 'a' && str.[0] <= 'z') || (str.[0] >= 'A' && str.[0] <= 'Z') 
        then find_non_alpha_chars ~symbols:symbols (String.sub str 1 ((String.length str) + (-1)))  
      else find_non_alpha_chars ~symbols:(symbols^(String.sub str 0 1)) (String.sub str 1 ((String.length str) + (-1))) 

(* checks if a given string is palindrome *)

let rec check_palindrome str = 
  match (String.length str) with
    0 -> true
    | 1 -> true
    | _ -> 
      if (str.[0]) == (str.[(String.length str) + (- 1)]) then check_palindrome (String.sub str 1 ((String.length str) + (-2)))
      else false;;

let rec is_palindrome str = check_palindrome (String.lowercase_ascii (str - (find_non_alpha_chars str)))

(* checks if any of the strings in the dictionary is an anagram of the given string *)

let single_string_anagram str1 str2 = 
  match String.length str1, String.length str2 with
    0, 0 -> true
    | len1, len2 when len1 != len2 -> false
    | len1, len2 -> 
      let rec verify_char_freq str1 str2 = match String.length str2 with
        0 -> if (String.length str1) == 0 then true else false
        | _ -> verify_char_freq (remove_char str1 str2.[0]) (String.sub str2 1 ((String.length str2) + (-1)))
      in verify_char_freq str1 str2;;

let rec anagram str dictionary = 
  match dictionary with
    [] -> false
    | hd::tl -> if (single_string_anagram str hd) then true else anagram str tl;;