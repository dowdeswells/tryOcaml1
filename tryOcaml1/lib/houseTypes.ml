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
  desc: string;
}

type account = {
  dailyInterestRate: float;
  accrewedInterest: float;
  balance: float;
  transactions: transaction List.t;
}

