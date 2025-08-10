

let add x y = x + y
let sub x y = x - y

module TD = Timedesc

type housePriceInput  = {
  buyPrice: float;
  sellPrice: float;
  settlementDate: TD.Date.t;
  saleDate: TD.Date.t;
  weeklyRent: float;
  monthlyOutgoings: float;
}

type flow = {
  date: TD.Date.t;
  amount: float;
}

let exampleHousePriceInput = {
  buyPrice = 500000.0;
  sellPrice = 600000.0;
  settlementDate = (TD.Date.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok;
  saleDate = (TD.Date.Ymd.make ~year:2035 ~month:1 ~day:1) |> Result.get_ok;
  weeklyRent = 500.0;
  monthlyOutgoings = 2000.0;
}



let add_1_week (date: TD.Date.t) : TD.Date.t =
  TD.Date.add ~days:7 date ;;

let print_date (date: TD.Date.t) : string =
  Format.asprintf "%a" TD.Date.pp_rfc9110 date ;;

let list_weekly_anniversary start_date end_date =
  let rec generate_payments current_date payments =
    if TD.Date.compare current_date end_date > 0 then
      payments
    else
      let next_anniversary = add_1_week current_date 
      in
      let payments' = current_date :: payments in
      generate_payments next_anniversary payments'
  in
  let payments = generate_payments start_date [] in
  List.rev payments  ;;

exception Ill_typed

type value =
  | Int of int
  | Bool of bool

type expr =
  | Value of value
  | Eq of expr * expr
  | Plus of expr * expr
  | If of expr * expr * expr

let rec eval expr =
  match expr with
  | Value v -> v
  | If (c, t, e) ->
    (match eval c with
     | Bool b -> if b then eval t else eval e
     | Int _ -> raise Ill_typed)
  | Eq (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Int (f1 + f2))  


let three = `Integer 3
let four = `Floater 4.0

let nan = `Not_a_number

let stuff = [three, four, nan]

let pp_stuff l =
  l |> List.map 
    (fun a -> match a with
              | `Integer a' -> Format.asprintf "%d" a'
              | `Floater b' -> Format.asprintf "%d" b'
              | `Not_a_number -> "Not a number"
              
    )
let is_positive = function
  | `Integer x -> Ok (x > 0)
  | `Floater x -> Ok (x > 0.)
  | `Not_a_number -> Error "not a number";;

let basic_color_to_int = function
  | `Black -> 0 | `Red     -> 1 | `Green -> 2 | `Yellow -> 3
  | `Blue  -> 4 | `Magenta -> 5 | `Cyan  -> 6 | `White  -> 7;;



let color_to_int = function
  | `Basic (basic_color,weight) ->
    let base = match weight with `Bold -> 8 | `Regular -> 0 in
    base + basic_color_to_int basic_color
  | `RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | `Gray i -> 232 + i;;


let extended_color_to_int = function
  | `RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
  | (`Basic _ | `RGB _ | `Gray _) as color -> color_to_int color;;


let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin";;

let show_dirs (path:string) = 
  String.split_on_char ':' path
  |> List.sort_uniq String.compare
  |> List.iter print_endline
  























