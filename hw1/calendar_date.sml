fun is_older( x: int * int * int, y : int * int * int) = 
    let
      val x_day = #1 x;
      val x_month = #2 x;
      val x_year = #3 x;
      val y_day = #1 y;
      val y_month = #2 y;
      val y_year = #3 y;
    in
      if x_year < y_year then true
      else if x_year = y_year andalso x_month < y_month then true
      else if x_year = y_year andalso x_month = y_month andalso x_day < y_day then true
      else false
    end


fun number_in_month( dates_list : (int * int * int) list, month : int) = 
    if null dates_list
    then 0
    else 
        let
          val m = #2 (hd dates_list)
        in
          if m = month
          then 1 + number_in_month( (tl dates_list), month )
          else number_in_month((tl dates_list), month)
        end

fun number_in_months( dates_list : (int * int * int) list, months : int list ) = 
    if null months
    then 0
    else number_in_month(dates_list, hd months) + number_in_months(dates_list, tl months)

fun dates_in_month(dates_list : (int * int * int ) list, month : int ) = 
    if null dates_list 
    then []
    else 
        let
          val m = #2 (hd dates_list)
        in
          if m = month
          then hd dates_list :: dates_in_month((tl dates_list), month)
          else dates_in_month((tl dates_list), month)
        end

fun dates_in_months(dates_list : (int * int * int) list,  months : int list) = 
    if null months
    then []
    else dates_in_month(dates_list , hd months) @ dates_in_months(dates_list , tl months)  (* SML’s list-append operator (@) *)

fun get_nth(list : string list, n : int) = 
    if n=1
    then hd list
    else get_nth(tl list, n-1)

fun date_to_string(date : int * int * int) = 
    let
      val map = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      val day = #1 date
      val month = #2 date
      val year = #3 date
    in
      get_nth(map, month)^"-"^Int.toString(day)^"-"^Int.toString(year)
    end

fun number_before_reaching_sum(sum : int, nums : int list) =
    if sum <= 0
    then ~1  (* use -1 because we want to get the largest sum which is less than target*)
    else
        let
          val first = hd nums
        in
          1 + number_before_reaching_sum(sum-first, tl nums)
        end

fun what_month(day : int) = 
    let
      val days_list = [31,28,31,30,31,30,31,31,30,31,30,31] 
    in
      1 + number_before_reaching_sum(day, days_list)
    end

fun month_range(day1 : int , day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else 
        let
          val temp = oldest(tl dates)
        in
          if isSome temp andalso is_older(valOf temp, hd dates)
          then temp
          else SOME(hd dates)
        end
        
fun cumulative_sum(nums : int list) = 
    if null nums
    then []
    else 
        let
            fun helper(cursum : int , nums : int list) = 
                if null nums
                then []
                else hd nums + cursum :: helper(hd nums + cursum, tl nums)
        in
          helper(0, nums)
        end

fun number_in_months_challenge()
        


