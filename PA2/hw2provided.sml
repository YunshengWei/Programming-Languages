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
      | c'::cs' => if c'=c then cs' else c'::remove_card(cs', c, e)

fun all_same_color(cs) =
    case cs of
        [] => true
      | c::[] => true
      | c::c'::cs' => card_color(c)=card_color(c') andalso all_same_color(c'::cs')

fun sum_cards(cs) =
    let fun aux(cs, acc) =
        case cs of
            [] => acc
          | c::cs' => aux(cs', card_value(c) + acc)
    in
        aux(cs, 0)
    end

fun score(cs, goal) =
    let val s =  sum_cards(cs)
        val ps = if s > goal then 3 * (s - goal) else goal - s
    in
        if all_same_color(cs) then ps div 2 else ps
    end

fun officiate(cl, ml, goal) =
    let fun aux(cl, hl, ml, s) =
        if s > goal
        then score(hl, goal)
        else case ml of
            [] => score(hl, goal)
          | Draw::ml' => (case cl of
                [] => score(hl, goal)
              | c::cl' => aux(cl', c::hl, ml', s + card_value(c)))
          | (Discard c)::ml' => aux(cl, remove_card(hl, c, IllegalMove), ml', s - card_value(c))
    in
        aux(cl, [], ml, 0)
    end

fun score_challenge(cs, goal) =
    let val all_same_color = all_same_color(cs)
        val num_aces = List.length(List.filter (fn (s, r) => r = Ace) cs)
        fun least_score(num_aces, sum) =
            if sum > goal
            then if num_aces = 0
                then 3 * (sum - goal)
                else Int.min(3 * (sum - goal), least_score(num_aces - 1, sum - 10))
            else goal - sum
        val ls = least_score(num_aces, sum_cards(cs))
    in
        if all_same_color then ls div 2 else ls
    end

fun officiate_challenge(cl, ml, goal) =
    let fun card_value2(c) =
        case c of
            (_, Num v) => v
          | (_, Ace) => 1
          | _ => 10
        fun aux(cl, hl, ml, s) =
        if s > goal
        then score_challenge(hl, goal)
        else case ml of
            [] => score_challenge(hl, goal)
          | Draw::ml' => (case cl of
                [] => score_challenge(hl, goal)
              | c::cl' => aux(cl', c::hl, ml', s + card_value2(c)))
          | (Discard c)::ml' => aux(cl, remove_card(hl, c, IllegalMove), ml', s - card_value(c))
    in
        aux(cl, [], ml, 0)
    end

fun careful_player(card_list, goal) =
    let fun can_discard(left, right) =
        case right of
            [] => NONE
          | c::right' => if score(left@right', goal) = 0
                then SOME c
                else can_discard(c::left, right')
        fun aux(card_list, held_list, sum) =
            if score(held_list, goal) = 0
            then []
            else
                case card_list of
                    [] => (if goal > sum + 10
                        then [Draw]
                        else case can_discard([], held_list) of
                            NONE => [Draw]
                          | SOME c => [(Discard c)])
                  | c::cl => (if goal > sum + 10
                        then Draw::aux(cl, c::held_list, sum+card_value(c))
                        else case can_discard([c], held_list) of
                            NONE => Draw ::aux(cl, c::held_list, sum+card_value(c))
                          | SOME c => [(Discard c), Draw])
    in
        aux(card_list, [], 0)
    end