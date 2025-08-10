module M = TryOcaml1.HouseTransactions 
module HT = TryOcaml1.HouseTypes

(* open TryOcaml1.House *)
let exampleHousePriceInput = HT.{
  buyPrice = 500000.0;
  loanAmount = 100000.0;
  interestRate = 365.0;
  sellPrice = 600000.0;
  settlementDate = (TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok;
  saleDate = (TD.Ymd.make ~year:2025 ~month:1 ~day:1) |> Result.get_ok;
  weeklyRent = 500.0;
  monthlyOutgoings = 2000.0;
} 

let pp_transaction fmt (t:HT.transaction) =
  Fmt.pf fmt "{ %s }" (M.pp_transaction t)
  
let equal_transaction (u1:HT.transaction) (u2:HT.transaction) =
  u1.date = u2.date && u1.amount = u2.amount

(* Step 3: Combine them into a testable value *)
let transaction_testable = Alcotest.testable pp_transaction equal_transaction

let applyAll (house:HT.housePriceInput) =
  M.makeTransactions house

let print_all () = 
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
  let acc = (applyAll a) in
  let txns = acc.transactions  in  
  
  let output = txns
    |> List.take 10 
    |> List.map M.pp_transaction
    |> String.concat "\r\n"
  in 
   Printf.sprintf "%s \r\nbalance: %f" output acc.balance |> print_endline 

let print_all2 () = 
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
  let acc = (applyAll a) in
  let interestPayments = acc.transactions  in
  Alcotest.(check int) "no of payments" 1  (List.length interestPayments);
  Alcotest.(check transaction_testable) 
    "first transaction" 
    HT.{date=settlementDate; amount=500.0; desc="Rent"}  
    (List.hd interestPayments);
  Alcotest.(check (Alcotest.float epsilon_float)) "balance" 100500.0  (acc.balance)



let houseTests = [
  Alcotest.test_case "Show Interest Payments" `Quick print_all2;
  Alcotest.test_case "Print Int 2 Digits" `Quick print_all;
] 
