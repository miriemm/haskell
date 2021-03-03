(*
-----------------------
-- Miriem Omer
-- 29.11.2020
-----------------------
*)

(* for this exercise, I used SOSML to write and run the code *)

(* empty list *)
val it = []: 'a list;

(* function to calculate the length of a list of characters *)
fun length(lst: char list): int =
  case lst of
    [] => 0
  | h::t => 1 + length(t) ;

(* function to get the minimum of a list -> it returns "int option" *)
fun minimum(xs)=
	case xs of
		[] => NONE
	  | (head::[]) => SOME head
	  | (head::neck::rest) =>	if head < neck
					then minimum (head::rest)
					else minimum (neck::rest);

(* I made a function valOf to transform "int option" to "int" *)
fun valOf (opt: 'a option) : 'a =
    case opt of
        NONE => raise Fail "option is NONE"
      | SOME a => a;

(* helper function that will be used in "levenshtein" function *)
(* I implemented this using the Algorithm for Levenshtein distance *)
fun levenshteinHelper (l1: char list) (l2: char list) :int =
(* if the first list is empty, it returns the nr of elem from the second list *)
  if l1 = it then length l2
(* if the second list is empty, it returns the nr of elements from the first list *)
  else if l2= it then length l1
  else
    case (l1,l2) of
      ([],[]) => 0 (* if both lists are empty, it returns 0 *)
  | (x::xs,y::ys) => if x = y then levenshteinHelper xs ys
else valOf (minimum [(levenshteinHelper xs l2) + 1, (levenshteinHelper l1 ys) + 1 , (levenshteinHelper xs ys) + 1] );


(* using String.explode we transform a string into a list of characters *)
(* I used the helper function from above on these lists of characters *)
fun levenshtein (x : string) (y : string): int =
     levenshteinHelper (String.explode x) (String.explode y);


(* Here, I tested if it works correctly *)

levenshtein "kitten" "sitting";
levenshtein "haskell" "sml";
