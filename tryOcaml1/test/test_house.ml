module M = TryOcaml1.HouseTransactions 
module HT = TryOcaml1.HouseTypes
module X = TryOcaml1.Xirr

(* transaction as printable, equal and so to testable *)
let pp_transaction fmt (t:HT.transaction) =
  Fmt.pf fmt "{ %s }" (M.pp_transaction t)
let equal_transaction (u1:HT.transaction) (u2:HT.transaction) =
  u1.date = u2.date && u1.amount = u2.amount
let transaction_testable = Alcotest.testable pp_transaction equal_transaction

(* Tests *)
let print_for_ten_years () = 
 let settlementDate = (M.TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok in
  let a = HT.{
    buyPrice = 500000.0;
    loanAmount = 100000.0;
    interestRate = 7.50;
    sellPrice = 600000.0;
    settlementDate = settlementDate;
    saleDate = (TD.Ymd.make ~year:2035 ~month:1 ~day:1) |> Result.get_ok;
    weeklyRent = 500.0;
    monthlyOutgoings = 500.0;
  } in
  let acc = (M.makeTransactions a) in
  let txns = acc.transactions  in  
  
  let output = txns
    |> List.take 10 
    |> List.map M.pp_transaction
    |> String.concat "\r\n"
  in 
   Printf.sprintf "%s \r\nbalance: %f" output acc.balance |> print_endline 

let print_for_one_month () = 
  let settlementDate = (M.TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok in
  let a = HT.{
    buyPrice = 500000.0;
    loanAmount = -100000.0;
    interestRate = 365.0;
    sellPrice = 600000.0;
    settlementDate = settlementDate;
    saleDate = (TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok;
    weeklyRent = 500.0;
    monthlyOutgoings = 500.0;
  } in
  let acc = (M.makeTransactions a) in
  let interestPayments = acc.transactions  in
  Alcotest.(check int) "no of payments" 3  (List.length interestPayments);
  Alcotest.(check transaction_testable) 
    "first transaction" 
    HT.{date=settlementDate; amount=600000.0; desc="Sell Property"}  
    (List.hd interestPayments);
  Alcotest.(check (Alcotest.float epsilon_float)) "balance" 700500.0  (acc.balance)




let calc_xirr () = 

  let convertDate (d:Timedesc.Date.t):Timedesc.t =
    Timedesc.of_date_and_time d (Timedesc.Time.make_exn ~hour:0 ~minute:0 ~second:0 ()) ~tz:Timedesc.Time_zone.utc
    |> Result.get_ok

  in let to_cashFlow (t:HT.transaction):X.cash_flow = {date=(convertDate t.date); amount=t.amount}  
  and settlementDate = (M.TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok in

  let a = HT.{
    buyPrice = 500000.0;
    loanAmount = 100000.0;
    interestRate = 7.50;
    sellPrice = 600000.0;
    settlementDate = settlementDate;
    saleDate = (TD.Ymd.make ~year:2035 ~month:1 ~day:1) |> Result.get_ok;
    weeklyRent = 500.0;
    monthlyOutgoings = 500.0;
  } in
  let acc = (M.makeTransactions a) in 
  let cashFlows = List.map to_cashFlow acc.transactions in
  let r = TryOcaml1.Xirr.xirr cashFlows in
  (match r with
  | Some xirr ->  Printf.sprintf "XIRR: %f" xirr
  | None -> "Didnt work")
  |> print_endline



let houseTests = [
  Alcotest.test_case "Do One Month" `Quick print_for_one_month;
  Alcotest.test_case "10 Years" `Quick print_for_ten_years;
  Alcotest.test_case "Print Xirr" `Quick calc_xirr;
] 
