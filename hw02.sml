(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(*1.a*)
(*help function to check if string in list*)
fun contains(s: string, strings:string list) =
    case strings of
        [] => false
	    | x::local_list => if same_string(s, x) then true else contains(s, local_list)

(*help function to remove string from list*)
fun remove_from (s: string, strings:string list) = 
    case strings of
        [] => []
	    | x::local_list => if same_string(s, x) then remove_from(s, local_list) else x::remove_from(s, local_list)

(*main task*)
fun all_except_option (s:string, strings:string list) = 
    if not (contains(s, strings)) then NONE else SOME (remove_from(s, strings))

(*test for 1.a*)
val test_1a_1 = all_except_option("b", ["a", "b", "c", "d", "b", "n"])
(*expected ["a", "c", "d", "n"]*)


(*1.b*)
(*help function that concat arrays*)
fun concat_arr (xl, yl) =
    case xl of
        [] => yl
	    | x::xl' => x::concat_arr(xl', yl)

(*take subarrays that contains 's' but exclude 's' and concat in one array*)
fun get_substitutions1(ssl: string list list, s:string) = 
    case ssl of
        [] => []
	    | x::ssl' =>
	        let
	            val t = get_substitutions1(ssl', s)
	        in
	            if not(contains(s, x)) then t else concat_arr(remove_from(s, x), t)
	        end

(*test for 1.b*)
val test_1b_1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
(*expected array of ["Fredrick","Freddie","F"]*)
val test_1b_2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(*expected array of ["Jeffrey","Geoff","Jeffrey"]*)

(*1.c*)
(*same as 1.b but uses local function*)
fun get_substitutions2 (ssl: string list list, s:string) = 
  let
    fun local_func (ssl: string list list, s: string, accu: string list) =
      case ssl of
        [] => accu
	| x::ssl' =>
	   let
	       val t = if not(contains(s, x)) then [] else remove_from(s, x)
	   in
	       local_func(ssl', s, concat_arr(accu, t))
	   end
  in
     local_func(ssl, s, [])
  end

(*tests for 1.c*)
val test_1c_1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
(*expected array of ["Fredrick","Freddie","F"]*)
val test_1c_2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(*expected array of ["Jeffrey","Geoff","Jeffrey"]*)

(*1.d*)
fun similar_names (ssl: string list list, {first=f, middle=m, last=z}) = 
  let
   fun fn_it (sl:string list) = 
     case sl of
        [] => []
	    | x::sl' => {first=x, last=z, middle=m}::fn_it(sl')
   in
    {first=f, last=z, middle=m}::fn_it(get_substitutions2(ssl, f))
   end

val test_1d_1 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})







(*task 2 starts here*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*2.a*)
fun card_color (s:suit, r:rank) = 
  case s of 
    Clubs => Black 
    | Diamonds => Red 
    | Hearts => Red 
    | Spades => Black

(*2.b*)
fun card_value (s:suit, r:rank) = 
  case r of 
    Ace => 11
    | Num i => i
    | _ => 10

(*2.c*)
fun remove_card (cards: card list, c: card, e: exn) = 
    case cards of 
      [] => raise e
      | x::cards' => if x = c then cards' else x::remove_card(cards', c, e)

(*2.d*)
fun all_same_color (cards: card list) = 
    case cards of 
      [] => true
      | c::[] => true
      | c1::(c2::cards') => card_color(c1) = card_color(c2) andalso all_same_color(c2::cards')

(*2.e*)
fun sum_cards (cards: card list) = 
  let
   fun local_func (cards: card list, accu) = 
    case cards of 
      [] => accu
      | c::cards' => local_func(cards', card_value(c)+accu)
  in
    local_func(cards, 0)
  end

(*2.f*)
fun score (cards: card list, goal) = 
   let
      val sum = sum_cards(cards)
      val pre = if sum > goal then 3 * (sum-goal) else goal-sum
   in
      if all_same_color(cards) then pre div 2 else pre
   end

(*2.g*)
fun officiate (cards: card list, ds: move list, goal) = 
    let
      fun local_func (cards: card list, ds: move list, accu_cards: card list) =
        case (cards, ds) of
	        (_, []) => score(accu_cards, goal)
	        | (_, (Discard c)::ds') => local_func(cards, ds', remove_card(accu_cards, c, IllegalMove))
          | ([], Draw::ds') => score(accu_cards, goal)
          | (c::cards', Draw::ds') => 
                if sum_cards(c::accu_cards) > goal 
                then score(c::accu_cards, goal) 
                else local_func(cards', ds', c::accu_cards)		   
    in
       local_func(cards, ds, [])
    end
