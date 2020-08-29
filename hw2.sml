(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1 *)
(* a *)
fun get_option opt =
	case opt of
		NONE => []
	| SOME lst => lst 

fun all_except_option (s, s_list) = (* don't need to add types since func args are resolved using pattern matching *)
	case s_list of
		[] => NONE
	| x::xs => if same_string (s, x)
						 then SOME xs 
						 else SOME (x::(get_option (all_except_option (s, xs))))

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


