(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, sl) =
    case sl of
        [] => NONE
      | s'::sl' => if same_string(s', s) 
            then SOME sl'
            else case all_except_option(s, sl') of
                NONE => NONE
              | SOME sl'' => SOME (s'::sl'')

fun get_substitutions1(sll, s) =
    case sll of
        [] => []
      | sl::sll' => case all_except_option(s, sl) of
            NONE => get_substitutions1(sll', s)
          | SOME sl' => sl' @ get_substitutions1(sll', s)

fun get_substitutions2(sll, s) =
    let fun aux(sll, acc) =
        case sll of
            [] => acc
          | sl::sll' => case all_except_option(s, sl) of
                NONE => aux(sll', acc)
              | SOME sl' => aux(sll', acc @ sl')
    in
        aux(sll, [])
    end

fun similar_names(sll, full_name) =
    let val {first=first_name, middle=middle_name, last=last_name} = full_name
    fun aux(sl) =
        case sl of
            [] => []
          | s::sl' => {first=s, middle=middle_name, last=last_name}::aux(sl')
    in
        full_name::aux(get_substitutions2(sll, first_name))
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
fun card_color(c) =
    case c of
        (Hearts, _) => Red
      | (Diamonds, _) => Red
      | (Clubs, _) => Black
      | (Spades, _) => Black

fun card_value(c) =
    case c of
        (_, Num v) => v
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
        [] => raise e
      | c':cs' => if c'=c then cs' else c'::remove_card(cs')

fun all_






