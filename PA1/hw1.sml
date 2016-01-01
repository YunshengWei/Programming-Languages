type date = int * int * int

fun is_older(d1 : date, d2 : date) =
    let val x1 = (#1 d1) * 365 + (#2 d1) * 31 + #3 d1
        val x2 = (#1 d2) * 365 + (#2 d2) * 31 + #3 d2
    in
        if x1 < x2
        then true
        else false
    end

fun number_in_month(ds : date list, month : int) =
    if null ds
    then 0
    else (if #2 (hd ds) = month then 1 else 0) + number_in_month(tl ds, month)

fun number_in_months(ds : date list, ms : int list) =
    if null ms
    then 0
    else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month(ds : date list, month : int) =
    if null ds
    then []
    else
        let val d = hd ds
        in
            if #2 d = month then d :: dates_in_month(tl ds, month) else dates_in_month(tl ds, month)
        end

fun dates_in_months(ds : date list, ms : int list) =
    if null ms
    then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth(ss : string list, n : int) =
    if n = 1
    then hd ss
    else get_nth(tl ss, n - 1)

fun date_to_string(d : date) =
    let val ss = ["January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December"]
    in
        get_nth(ss, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

fun number_before_reaching_sum(sum : int, ns : int list) =
    if hd ns >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd ns, tl ns)

fun what_month(day : int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days) + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(ds : date list) =
    if null ds
    then NONE
    else
        let fun oldest_non_empty(ds : date list) =
            if null(tl ds)
            then hd ds
            else let val tl_ans = oldest_non_empty(tl ds)
                in
                    if is_older(hd ds, tl_ans)
                    then hd ds
                    else tl_ans
                end
        in
            SOME(oldest_non_empty(ds))
        end


