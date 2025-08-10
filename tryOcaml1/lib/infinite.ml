type ints_seq = int Seq.t

let rec ints_from (n : int) : ints_seq = fun () -> Seq.Cons (n, ints_from (n + 1))

let natural_numbers : ints_seq = ints_from 0
let first_five = Seq.take 5 natural_numbers |> List.of_seq

module TD = Timedesc.Date

type date = TD.t

type date_seq = date Seq.t


let print_date (d:date) = Printf.sprintf "%d-%02d-%02d" (TD.year d) (TD.month d) (TD.day d)


let rec weeks_from (n : date) : date_seq = fun () -> Seq.Cons (n, weeks_from (TD.add ~days:7 n))

let rec days_from (n : date) : date_seq = fun () -> Seq.Cons (n, days_from (TD.add ~days:1 n))

let days_range (start:date) (endDate:date) =
  days_from start
  |> Seq.take_while (fun a -> TD.le a endDate)



