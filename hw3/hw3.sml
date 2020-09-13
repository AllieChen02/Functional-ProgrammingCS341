(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

val slst = ["Facebook", "Linkedinlinkedin","amazon","GOOGLE","zooM","aaaaaaaalinkedin"]
(* problem 1  string list -> string list *)
fun only_lowercase slst = 
    List.filter(fn (s) => Char.isLower(String.sub(s, 0))) slst
val res1 = only_lowercase(slst)

(* problem 2 string list -> string *)
fun longest_string1 slst = 
    List.foldl(fn (s, acc) => if String.size(s) > String.size(acc) then s else acc) "" slst

fun longest_string1_1 slst = 
    let
      fun helper(lst, acc) = 
        case lst of 
                [] => acc
            |   x :: xs' => if String.size(x) > String.size(acc) then helper(xs', x) else helper(xs', acc)
    in
      helper(slst, "")
    end
val res2 = longest_string1 slst
val res2_1 = longest_string1_1 slst

(* problem 3  string list -> string *)
fun longest_string2 slst = List.foldl(fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" slst
val res3 = longest_string2 slst

(* problem 4  (int * int -> bool) -> string list -> string *)
val longest_string_helper = fn f => fn slst => List.foldl(fn (x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" slst
val longest_string3 = longest_string_helper (fn (x,y) => x > y) slst
val longest_string4 = longest_string_helper (fn (x,y) => x >= y) slst

(* problem 5 string list -> string *)
val longest_lowercase = 
    (longest_string_helper( fn(x,y) => x > y)) o (List.filter(fn (s) => Char.isLower(String.sub(s, 0))))
val res5 = longest_lowercase slst

(* problem 6 from right to left *)
val caps_no_X_string  = 
    String.implode o List.map Char.toUpper o List.filter (fn c => c <> #"x" andalso c <> #"X") o String.explode;

val res6 = caps_no_X_string "aBxXXxDdx"  

(* problem 7 *)
fun first_answer(f, xs) = 
    case xs of 
        x :: xs' => (case f x of
                        SOME v => v
                        |   _ => first_answer(f, xs'))
        |  _ => raise NoAnswer

(* problem 8 *)
fun all_answer(f, xs) = 
    let 
        val res = List.map f xs
        fun aux (xs, acc) =
            case xs of
                [] => acc
            |   SOME v :: xs' => aux(xs', SOME (v @ valOf acc))
            |   _ => NONE
    in
	    aux(res, SOME [])
    end;

(* problem 9 -a *)
val count_wildcards = g (fn _ => 1) (fn s => 0);

val xs = TupleP [WildcardP, UnitP, ConstructorP("nouse", WildcardP), TupleP [WildcardP, VariableP "var"]];
val count_wild_and_variable_lengths = count_wildcards xs 