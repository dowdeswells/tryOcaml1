
module M = TryOcaml1.Infinite

module TD = Timedesc.Date

let start = (TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok
let endDate = TD.add ~days:9 start

let ten_days = M.days_range start endDate

let print_all () = ten_days 
  |> Seq.map (M.print_date) 
  |> List.of_seq
  |> String.concat ","
  |> print_endline

let print_date () =
  Alcotest.(check string) "print start date" "2025-01-01"  (M.print_date start)

let dates = [
  Alcotest.test_case "Print Date" `Quick print_date;
  Alcotest.test_case "Print Int 2 Digits" `Quick print_all;
]
