(* Homework 1 *)
 
(* 1 *)
fun is_older (date1: int*int*int, date2: int*int*int) =
  let val date1_in_days = (#1 date1) * 365 + (#2 date1) * 30 + #3 date1 
      val date2_in_days = (#1 date2) * 365 + (#2 date2) * 30 + #3 date2 
  in 
      date1_in_days < date2_in_days
  end 

 
(* 2 *)
fun number_in_month (dates: (int*int*int) list, month: int) = 
  if null dates
  then 0
  else 
    if (#2 (hd dates)) = month
    then 1 + number_in_month (tl dates, month) 
    else number_in_month (tl dates, month)

(* 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) = 
  if null months
  then 0
  else
    (* recurisvely split problem into subproblems of (dates, month_i). 
    * since no date can have 2 different months, there is no overlap *)
    number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* 4 *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else
    if (#2 (hd dates)) = month
    then hd dates::dates_in_month (tl dates, month) 
    else dates_in_month (tl dates, month)

(* 5 *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
  (* need helper to make sure order of dates is preserved *)
  let fun is_date_in_months (date: (int*int*int), months: int list) =
      (* returns true if date is in any month in the list of months or false
      * otherwise. Could be converted to returning an Optional instead. *)
        if null months
        then false 
        else
          if #2 date = hd months
          then true 
          else is_date_in_months(date, tl months)
  in
    if null dates
    then []
    else
      if is_date_in_months (hd dates, months)
      then hd dates::dates_in_months(tl dates, months)
      else dates_in_months(tl dates, months)
  end

(* 6 *)
fun get_nth (strings: string list, n: int) =
  (* return the nth element of the list *)
  if n = 1
  then hd strings
  else
    get_nth (tl strings, n-1)

(* 7 *)
fun date_to_string (date: (int*int*int)) =
  let val months = ["January", "February", "March", "April", "May", "June","July", "August", "September", "October", "November", "December"]
      val year = #1 date
      val month = get_nth (months, #2 date)
      val day = #3 date
  in
      month ^ " " ^ Int.toString day ^ "," ^ " " ^ Int.toString year
  end

(* 8 *)
fun number_before_reaching_sum (sum: int, values: int list) =
 (* return an int n s.t. the first n elements of the list add to less than the
 * sum and the first n+1 elements of the list add to sum or more *) 
 if hd values >= sum (* assumes that values is non-empty *)
 then 0 
 else
   1 + number_before_reaching_sum (sum - hd values, tl values)

(* 9 *)
fun what_month (day: int) =
  let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    1 + number_before_reaching_sum(day, month_days)
  end

(* 10 *)
fun month_range (day1: int, day2: int) =
  (* return a list of months between day1 and day2 *)
  if day1 > day2
  then []
  else
    what_month day1::month_range(day1+1, day2)

(* 11 *)
fun oldest(dates: (int*int*int) list) =
  (* return an optional containing the oldest date *)
  if null dates
  then NONE
  else
    let
      val oldest_date = oldest (tl dates)
    in
      if isSome oldest_date
      then
        if is_older (hd dates, valOf oldest_date)
        then SOME (hd dates)
        else
          oldest_date
      else
       SOME (hd dates) 
    end



