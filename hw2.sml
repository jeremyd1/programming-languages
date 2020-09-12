(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1 *)
(* a *)
fun all_except_option (s, s_list) = 	
	(* return NONE if the string is not in the list, else return SOME lst
	* where lst is identical to the argument list except the string
  * is not in it *)
	case s_list of
		[] => NONE
	| x::xs => if same_string (s, x)
						 then SOME xs 
						 else case all_except_option(s, xs) of
										NONE => NONE
									| SOME lst => SOME (x::lst) (* only add to list after recursive call *)  

(* b *)
fun get_substitutions1 (s_list_list, s) =
	(* return all strings in lists that have s, but s itself should not be in the result *)
	case s_list_list of
		[] => []
	| a::b => case all_except_option(s, a) of
							NONE => get_substitutions1 (b, s)	
						| SOME lst => lst @ get_substitutions1(b, s) 
			
(* c *)
fun get_substitutions2 (s_list_list, s) =
	(* part b but tail-recursive *)
	let fun helper (acc, s_list_list, s) =
		case s_list_list of
			[] => acc
		| a::b => case all_except_option (s, a) of
								NONE => helper (acc, b, s)
							| SOME lst => helper (acc @ lst, b, s) 
	in
		helper ([], s_list_list, s)
	end

(* d *)
fun similar_names (s_list_list, {first=first, middle=middle, last=last}) =
	(* takes a string list list of substitutions, a full name, and returns a list of
	* full names. Result is all the full names you can produce by substituting for the
	* first name. *)
	let val subs = get_substitutions2 (s_list_list, first)
		fun helper(subs) =
			case subs of
				[] => []			
			| x::xs => {first=x, middle=middle, last=last}::helper(xs)	

	in
		{first=first, middle=middle, last=last}::helper(subs) 
	end
		

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color c =
	(* return the color of card c *)
	case c of
		(Clubs, _) => Black (* QUESTION: how does a pattern differentiate between a var and an exact string? *)
  | (Spades, _) => Black
	| (Diamonds, _) => Red
	| (Hearts, _) => Red

(* b *)
fun card_value c =
	case c of
	  (_, Jack) => 10 
	| (_, Queen) => 10
	| (_, King) => 10
	| (_, Ace) => 11
	| (_, Num i) => i 
	
(* c *)
fun remove_card (cs, c, e) =
	(* return a list of all the caards in cs except c 
	* if c is in the list more than once, remove only the
	* first one
	* if c is not in the list, return an exception *)
	let fun helper(cs, c, found, e) =
		case cs of
			[] => if found then [] else raise e
		|	c1::cs' => if found orelse c1 <> c 
								then c1::helper (cs', c, found, e) 
								else helper (cs', c, true, e)
	in helper (cs, c, false, e)
	end

(* d *)
fun all_same_color cs =
	(* return true if all the cards are the same color *)
	let fun helper (cs, prev) =
		case cs of
			[] => true
		| c::cs' => if (card_color c) = (card_color prev)
								then helper (cs', c)
								else false
	in case cs of
			[] => true
		| c::cs' => helper (cs', c) 
	end

(* e *)
fun sum_cards cs =
	(* return the sum of all the card values - must be tail recursive *)
	let fun helper (cs, sum) =
		case cs of
			[] => sum
		| c::cs' => helper (cs', (card_value c) + sum)
	in helper(cs, 0)
	end	

(* f *)
fun score (cs, g) =
	let val sum =	sum_cards cs
		val preliminary_score = if sum > g then 3 * (sum - g) else (g - sum)
	in if all_same_color cs
		then preliminary_score div 2
		else preliminary_score
	end

(* g *)
fun officiate (cards, moves, goal) =
	let fun state(cards, moves, held_cards, goal) =
		if sum_cards held_cards > goal
		then score (held_cards, goal)
		else
			case moves of
			  [] => score (held_cards, goal)
			| move::moves' => (case move of
                                Draw => (case cards of
										  [] => score (held_cards, goal)
										| card::cards' => state (cards', moves', card::held_cards, goal))
                              | Discard card => state (cards, moves', remove_card (held_cards, card, IllegalMove), goal))
	in state(cards, moves, [], goal)
	end
