(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A";

val test6 = rev_string "abc" = "cba";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1;

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val test13a = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", Variable "x")]) = SOME (Datatype "bar1");

val test13g = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", ConstructorP ("foo2", UnitP))]) = SOME (Datatype "bar1");

val test13h = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", ConstructorP ("foo2", ConstP 1))]) = NONE;

val test13i = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", ConstructorP ("foo2", Wildcard))]) = SOME (Datatype "bar1");

val test13b = typecheck_patterns([("foo1","bar1",IntT),("foo2","bar2",UnitT)],
                              [ConstructorP("foo1", ConstP 1),
                               ConstructorP("foo2",UnitP)])
           = NONE;

val test13c = typecheck_patterns([("foo1","bar1",IntT),("foo2","bar2",UnitT)],
                   [ConstructorP("foo1", ConstP 1), ConstructorP("foo2",UnitP)]) = NONE;

val test13d = typecheck_patterns([],
        [TupleP [Variable("x"), Variable("y")], TupleP [Wildcard, Wildcard]])
        = SOME (TupleT [Anything, Anything])

val test13e = typecheck_patterns([],
        [TupleP [Wildcard, Wildcard], TupleP [Wildcard, TupleP [Wildcard, Wildcard]]])
        = SOME (TupleT [Anything, TupleT [Anything, Anything]])

val test13f = typecheck_patterns([("foo1","bar1",IntT),("foo2","bar2",UnitT)],
                              [ConstructorP("foo3", ConstP 1)])
           = NONE;
