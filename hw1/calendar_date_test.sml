use "calendar_date.sml";

val test1 = is_older ((1,2,2020),(2,3,2020)) = true

val test2 = number_in_month([(28, 2, 2012),(24,12,2013)],2) = 1

val test3 = number_in_months([(26, 3, 2000),(27, 2, 2012),(28, 2, 2012),(24,12,2013)],[2,3]) = 3

val test4 = dates_in_month ([(28, 2, 2012),(24,12,2013), (27, 2, 2012)],2) = [(28, 2, 2012), (27, 2, 2012)]

val test5 = dates_in_months([(26, 3, 2000), (24,12,2013),(27, 2, 2012)],[2,3]) = [(27, 2, 2012), (26, 3, 2000)]  (* order ? *)

val test6 = get_nth(["hello", "world", "good", "morning"], 3) = "good"

val test7_1 = date_to_string((26, 3, 2000)) = "March-26-2000"
val test7_2 = date_to_string((11, 12, 2000)) = "December-11-2000"

val test8_1 = number_before_reaching_sum(23, [2,5,4,6,9,15,16]) = 4
val test8_2 = number_before_reaching_sum(5, [2,5,4,6,9,15,16]) = 1

val test9_1 = what_month(31) = 1
val test9_2 = what_month(32) = 2
val test9_3 = what_month(364) = 12

val test10_1 = month_range(31,32) = [1,2]
val test10_2 = month_range(31,33) = [1,2,2]

val test11_1 = oldest([(26, 3, 2000), (24,12,2013),(27, 2, 2012)]) = SOME (26, 3, 2000)

val test12_1 = cumulative_sum([12,27,13]) = [12,39,52]

(* challenge problems *)
val test13_1 = number_in_months_challenge([(26, 3, 2000),(27, 2, 2012),(28, 2, 2012),(24,12,2013)],[2,3]) = 3
val test14_1 = dates_in_months_challenge([(26, 3, 2000), (24,12,2013),(27, 2, 2012)],[2,3]) = [(27, 2, 2012), (26, 3, 2000)]
val test15_1 = reasonable_date(31, 7, 2000)
val test15_2 = reasonable_date(29, 3, 1996)
val test15_3 = reasonable_date(31, 1, 1997)