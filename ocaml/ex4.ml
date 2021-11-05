exception EmptyString

type word_frequency = { word: string; mutable frequency: int }
type text_frequency = word_frequency list

(* if a word_frequency record with the word equal to the provided one, increments the frequency of that record,
otherwise, inserts a new word_frequency record with the provided word and frequency set to 1.
returns the modified text_frequency list *)
let rec increment_frequency word frequency = match frequency with
  | [] -> { word=word; frequency=1 }::[]
  | hd::tl when hd.word = word -> { word=word; frequency=hd.frequency+1 }::tl
  | hd::tl -> hd::(increment_frequency word tl);;

(* reads the whole file *)
let read_file filename = 
  let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in s;;

(* extracts the first word (or symbol) that encounters in the provided text, skips spaces *)
let extract_word text =
  let rec ext_w str = match String.length str with
    (* empty string provided *)
    | 0 when String.equal text str -> raise EmptyString
    (* ends the string while reading a word, returns the word read until here *)
    | 0 -> ""
    (* encounter a space as first char, skips it and read the next word *)
    | _ when str.[0] = ' ' && String.equal str text -> ext_w (String.sub str 1 ((String.length str) + (-1)))
    (* encounters a space while reading a word, returns the word read until the space *)
    | _ when str.[0] = ' ' -> ""
    (* encounters a char, continue with the word reading *)
    | _ when (str.[0] >= 'a' && str.[0] <= 'z') || (str.[0] >= 'A' && str.[0] <= 'Z') -> (String.make 1 str.[0])^(ext_w (String.sub str 1 ((String.length str) + (-1))))
    (* encounters a symbol as first char, returns that symbol as a word *)
    | _ when String.equal str text -> String.make 1 str.[0]
    (* encounters a symbol while reading a word, returns the word read until the symbol *)
    | _ -> ""
  in ext_w text;;

(* returns a word_frequency list with the frequency of each word in the provided text *)
let calc_word_freq text = 
  let rec calc str freq = match String.length str with
    | 0 -> freq
    | _ when str.[0] = ' ' -> calc (String.sub str 1 ((String.length str) + (-1))) freq
    | _ -> 
      let w = extract_word str 
      in let updated_text = String.sub str (String.length w) ((String.length str) + (- (String.length w)))
      in calc updated_text (increment_frequency (String.lowercase_ascii w) freq)
  in calc text [];;