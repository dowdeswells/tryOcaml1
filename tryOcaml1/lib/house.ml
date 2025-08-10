module TD = Timedesc.Date

type date = TD.t

type housePriceInput  = {
  buyPrice: float;
  sellPrice: float;
  loanAmount: float;
  interestRate: float;
  settlementDate: date;
  saleDate: date;
  weeklyRent: float;
  monthlyOutgoings: float;
}

type transaction = {
  date: date;
  amount: float;
}

type account = {
  dailyInterestRate: float;
  accrewedInterest: float;
  balance: float;
  transactions: transaction List.t;
}

type accountRepo = {
  getAccount: unit -> account;
  writeAccount: account -> unit;
}

type streamMergeState = {
  streams: (transaction Seq.t) Array.t;
}

let pp_transaction t = Printf.sprintf "%s %f" (Infinite.print_date t.date) t.amount 

let interestAtDay (repo:accountRepo) (house:housePriceInput) (d:date) =
  let account = repo.getAccount () in
  let amount = if account.balance < 0.0 
    then 0.0 
    else 
      let interest = account.accrewedInterest +. account.dailyInterestRate *. account.balance in
      if (TD.day d) = (TD.day house.settlementDate)
        then 
          let account' = {account with accrewedInterest=0.0;} in 
          repo.writeAccount account';
          interest
        else 
          let account' = {account with accrewedInterest=interest;} in 
          repo.writeAccount account';
          0.0
  in
  {date=d; amount=amount;}  


let buildInterest (repo:accountRepo) (house:housePriceInput) = 
  let open Infinite in
  let days = days_range house.settlementDate house.saleDate in
  let interestTransactions =
    days
    |> Seq.map (interestAtDay repo house)
    |> Seq.filter (fun t -> t.amount > 0.0)
    |> Seq.map (fun t -> 
                  let account' = repo.getAccount() in
                  repo.writeAccount {account' with balance=account'.balance +. t.amount};
                  t)
  in
  interestTransactions



let buildRent (house:housePriceInput) =
  let open Infinite in
  let rentDay = TD.weekday house.settlementDate in
  let days = days_range house.settlementDate house.saleDate in
  let rentTransactions = days
  |> Seq.filter (fun a -> TD.weekday a = rentDay)
  |> Seq.map (fun a -> {date=a; amount=house.weeklyRent;}) in
  rentTransactions




