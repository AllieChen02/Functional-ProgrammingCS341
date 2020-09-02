(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report
use "parsed_large_police.sml";

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array") *)


(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

(* histogram and histogram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

exception SortIsBroken
(* problem 1 *)
fun make_silly_json(i : int) = 
  let
    fun helper(n) = 
      if n = 1
      then []
      else Object [("n", Num(int_to_real(n-1))), ("b", True)] :: make_silly_json(n-1)
  in 
    helper(i)
  end
val test1 = make_silly_json(10);

(* problem 2 *)
fun assoc (k, xs) = 
  case xs of
    [] => NONE
  | (k1,v1) :: t => if k=k1 then SOME v1 else assoc(k, t)
val L2 = [("Facebook", 1857), ("Linkedin", 1890), ("Google", 1894), ("Amazon", 1875)];
val k1 = "ellychen"
val k2 = "Linkedin";
val p2 = assoc(k2, L2);

(* problem 3 
   takes a json (call it j) and a string (call it f) and returns a json option.
*)
fun dot(j, f) = 
  case j of 
    	Object(lst) => assoc(f, lst)
    | _ => NONE;

(* problem 4 
  takes a json, and returns a string list
*)
val j1 = String "Pearl";
val j2 = Object([("Facebook", Num(int_to_real(89))), ("Linkedin", Num(int_to_real(79))), ("Google", Num(int_to_real(37))), ("Amazon", Num(int_to_real(40)))]);
fun one_fields(j) = 
  case j of 
    Object(lst) => 
      let
        fun helper(lst, acc) =
          case lst of 
              [] => acc
            | (field, _) :: t => helper(t, field :: acc)
      in
        helper(lst, [])
      end   
  | _ => []
val p4 = one_fields(j2);


(* problem 5 
    take a string list, and return a bool
*)
fun no_repeats(slst) = 
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2
    val sorted_xs = ListMergeSort.sort compare_strings slst (* sorted first *)
    fun helper(lst) = 
      case lst of 
        [] => true
      | h :: t => 
        case t of 
            [] => true
          | h2 :: _ => if h=h2 then false else helper(t)
  in
    helper(sorted_xs)    
  end
val s1 = ["Anya", "Elly", "Emma", "Tony"];
val s2 = ["Anya", "Elly", "Emma", "Tony", "Elly"];
val p5 = no_repeats(s1);
val p5 = no_repeats(s2);


fun two_fields(j) = 
  case j of 
      Object(lst) => 
        let fun aux(l, acc) = 
            case l of
                [] => acc
              | (_,v) :: t => aux(t, v :: acc)
        in
          aux(lst, [])
        end
(* problem 6, takes a json and return a bool *)
fun recursive_no_field_repeats(j) = 
    case j of
      Object(lst) => no_repeats(one_fields(j)) andalso recursive_no_field_repeats(Array(two_fields(j)))
    | Array(jlst) => (
        case jlst of  
            [] => true
          | h :: t => recursive_no_field_repeats(h) andalso recursive_no_field_repeats(Array(t)))
    | _ => true 


(* string list -> (string, int) list *)
fun count_occurrences(xs, ex) = 
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2
    fun helper(xs', acc, curr, count) = 
      case xs' of 
          [] => (curr, count) :: acc
        | h :: t =>          
            case strcmp(h, curr) of 
                LESS => raise ex
              | EQUAL => helper(t, acc, curr, count+1)
              | GREATER => helper(t, (curr, count) :: acc, h, 1)
         
  in
    (* sort first *)
    let
      val sorted_xs = ListMergeSort.sort compare_strings xs 
    in
      case sorted_xs of 
        [] => []
      | x :: xs' => helper(xs', [], x, 1)
    end
  end

val L = ["Fisher", "Neyman", "Pearson", "Pearson", "Williams"];
exception NotSortedInAscendingOrder;
val res = count_occurrences(L, NotSortedInAscendingOrder);

(* problem 8 
   string * (json list) -> string list
*)
fun string_values_for_field(k, jlst) = 
  case jlst of 
      [] => []
    | h :: t => (
      case dot(h, k) of 
          SOME(String(s)) => s :: string_values_for_field(k, t)
        | _ => string_values_for_field(k, t)
    )
val j8 = [j2, Object([("Pearson", String "Karl"), ("Fisher", String "Ronald"), ("Conway", String "John"), ("Neyman", String "Jerzy"), ("Pearson", String "Egon")])];
val res = string_values_for_field("Pearson", j8);

(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)


fun histogram (xs : string list) : (string * int) list = (* string list -> string * int list *)
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs 
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))


(**** PUT PROBLEMS 9-11 HERE ****)
(* prbolem 9 *)
fun filter_field_value(k, v , jlst) =
  case jlst of 
      [] => []
    | h :: t => (
      case dot(h, k) of
          SOME(String(s)) => 
            if s=v
            then h :: filter_field_value(k, v, t)
            else filter_field_value(k, v, t)
        | _ => filter_field_value(k , v, t)
    )
val j9 = [j2, Object([("Pearson", String "Karl"), ("Fisher", String "Ronald"), ("Conway", String "John"), ("Neyman", String "Jerzy"), ("Pearson", String "Egon")])]
val res2 = filter_field_value("Pearson", "Karl", j9)

(* problem 10 and 11 *)
(* val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", large_incident_reports_list);
val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list); *)

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)


(* 
  convert json to string 
  string * string list -> string
*)
fun concat_with(seperator, slst) = 
  let
    fun helper(acc, tlst) =
      case tlst of  
          [] => acc
        | h :: t => helper(acc ^ seperator ^ h, t)
  in
    case slst of
        [] => ""
      | h :: t => helper(h, t)
  end
  val test16 = concat_with (" ", ["Nice", "to", "meet", "you", "!"]);


fun quote_string(s) = 
  "\"" ^ s ^ "\"";
val test17 = quote_string("helloworld")

fun real_to_string_for_json(r) = 
  if real_is_negative(r) 
  then "-" ^ real_to_string(real_abs(r)) 
  else real_to_string(r);


fun json_to_string (j) = 
  let
    fun object_to_string(arr) =
      case arr of 
          [] => []
        | (k,v) :: t => (quote_string(k) ^ " : " ^ json_to_string(v)) :: object_to_string(t)
    fun array_to_string(arr) =
      case arr of
          [] => []
        | h :: t => json_to_string(h) :: array_to_string(t)
  in
    case j of 
      Num r => real_to_string_for_json (r)
	  | String s => quote_string(s)
	  | False => "false"
	  | True => "true"
	  | Null => "null"
	  | Object lst => "{" ^ concat_with(", ", object_to_string(lst)) ^ "}"
	  | Array lst => "[" ^ concat_with(", ", array_to_string(lst)) ^ "]"
  end 
val y = json_to_string(json_obj);

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

