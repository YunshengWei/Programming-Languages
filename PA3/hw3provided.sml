(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* problem 1 *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) ""

val longest_string2 =
    List.foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) ""

fun longest_string_helper f =
    List.foldl (fn (s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* problem 2 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => (case f(x) of
            NONE => first_answer f xs'
          | SOME v => v)

fun all_answers f xs =
    let fun aux(xs, acc) =
        case xs of
            [] => SOME acc
          | x::xs' => case f(x) of
                NONE => NONE
              | SOME v => aux(xs', acc @ v)
    in
        aux(xs, [])
    end

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) String.size

fun count_some_var(s, p)=
    g (fn x => 0)  (fn x => if x = s then 1 else 0) p

fun check_pat(p) =
    let fun get_all_variables(p) =
        case p of
            Variable x => [x]
          | TupleP ps => List.foldl (fn (p, l) => get_all_variables(p) @ l) [] ps
          | ConstructorP(_, p) => get_all_variables(p)
          | _ => []
        fun has_repeats(xs) =
        case xs of
            [] => false
          | x::xs' => List.exists (fn x' => x = x') xs' orelse has_repeats(xs')
    in
        not (has_repeats(get_all_variables(p)))
    end

fun match(v, p) =
    case (p, v) of
        (Wildcard, _) => SOME []
      | (Variable s, v) => SOME [(s, v)]
      | (UnitP, Unit) => SOME []
      | (ConstP i, Const i') => SOME []
      | (TupleP ps, Tuple vs) => 
            if List.length ps <> List.length vs
            then NONE
            else all_answers match (ListPair.zip(vs, ps))
      | (ConstructorP (s, p), Constructor (s', v)) =>
            if s = s' then match(v, p) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE

(* Challenge Problems *)
fun typecheck_patterns(sstl, pl) =
    let 
        fun f s =
            List.find (fn (x, _, _) => x = s) sstl
        (* if pattern p and type t match, return SOME lowest common anster, else NONE *)
        fun match (p, t) =
            case p of
                Wildcard => SOME t
              | Variable _ => SOME t
              | UnitP => if t = Anything orelse t = UnitT then SOME UnitT else NONE
              | ConstP _ => if t = Anything orelse t = IntT then SOME IntT else NONE
              | TupleP ps => (case t of
                    TupleT ts => if List.length ps <> List.length ts
                        then NONE
                        else (case (all_answers (fn x => case (match x) of
                            NONE => NONE
                          | SOME t => SOME [t]) (ListPair.zip (ps, ts))) of
                            NONE => NONE
                          | SOME ts' => SOME (TupleT ts'))
                  | Anything => (case (all_answers (fn x => case (match x) of
                            NONE => NONE
                          | SOME t => SOME [t]) (List.map (fn x => (x, Anything)) ps)) of
                        NONE => NONE
                      | SOME ts' => SOME (TupleT ts'))
                  | _ => NONE)
              | ConstructorP (s, p) => case f s of
                    NONE => NONE
                  | SOME (_, x, t') => (case match(p, t') of
                        NONE => NONE
                      | SOME _ => (case t of
                            Datatype x' => if x = x' then SOME (Datatype x) else NONE
                          | Anything => SOME (Datatype x)
                          | _ => NONE))
        fun g (pl, lca) =
            case pl of
                [] => SOME lca
              | p::pl' => (case match (p, lca) of
                    NONE => NONE
                  | SOME t => g (pl', t))
    in
        g(pl, Anything)
    end










