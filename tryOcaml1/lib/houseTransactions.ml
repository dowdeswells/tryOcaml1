
module HT = HouseTypes
module TD = Timedesc.Date

type date = TD.t


type transactionAccumulator = {
  house: HT.housePriceInput;
  dailyInterestRate: float;
  accrewedInterest: float;
  balance: float;
  transactions: HT.transaction List.t;
}

type date_seq = date Seq.t

let pp_transaction (t:HT.transaction) = Printf.sprintf "%s %f %s" (Infinite.print_date t.date) t.amount t.desc

let interest  (acc:HT.account):float = 
  if acc.balance < 0.0 
  then 0.0 
  else acc.accrewedInterest +. acc.dailyInterestRate *. acc.balance 

let interestDay (settlementDate:date) (day:date) = 
  (TD.day day) = (TD.day settlementDate)


let applyTransaction acc (txn:HT.transaction) = 
  { acc with balance= acc.balance +. txn.amount; transactions= txn::acc.transactions}

let calcOutgoings (acc:transactionAccumulator) (d:date):transactionAccumulator =
  let amount = -. acc.house.monthlyOutgoings in
  if (TD.day d) = (TD.day acc.house.settlementDate) && not (TD.equal d acc.house.settlementDate)
    then 
      applyTransaction acc {date=d; amount=amount; desc="Outgoings"} 
    else 
      acc


let calcInterest (acc:transactionAccumulator) (d:date):transactionAccumulator =
  let amount = 
    if acc.balance >= 0.0 
    then 0.0 
    else acc.accrewedInterest +. acc.dailyInterestRate *. acc.balance in

  if (TD.day d) = (TD.day acc.house.settlementDate) && not (TD.equal d acc.house.settlementDate)
    then 
      applyTransaction {acc with accrewedInterest=0.0;} {date=d; amount=amount; desc="Interest"}
    else 
      {acc with accrewedInterest=amount;}

let calcRent (acc:transactionAccumulator) (d:date):transactionAccumulator =
  let rentDay = TD.weekday acc.house.settlementDate
  and thisDay = TD.weekday d in
  if rentDay = thisDay
    then applyTransaction acc {date=d; amount=acc.house.weeklyRent; desc="Rent"}
    else acc  

let calcAll (acc:transactionAccumulator) (d:date):transactionAccumulator =
  let acc' = calcRent acc d in
  let acc'' = calcOutgoings acc' d in
  calcInterest acc'' d

let rec days_from (n : date) : date_seq = fun () -> Seq.Cons (n, days_from (TD.add ~days:1 n))

let days_range (start:date) (endDate:date) =
  days_from start
  |> Seq.take_while (fun a -> TD.le a endDate)


let makeTransactions (house:HT.housePriceInput):transactionAccumulator =
  let acc={
    house=house;  
    dailyInterestRate=house.interestRate /. (365.0 *. 100.0);
    accrewedInterest=0.0;
    balance=0.0 -. house.loanAmount;
    transactions = [{date=house.settlementDate; amount= -. house.buyPrice; desc="Buy property"}];
    } 
  and days = days_range house.settlementDate house.saleDate in
  let acc' = Seq.fold_left calcAll acc days in
  applyTransaction acc' {date=house.saleDate; amount=house.sellPrice; desc="Sell Property"}
  


