type cash_flow = {
  amount : float;
  date : Timedesc.t;
}

type cash_flows = cash_flow list


let calcDaysBetween (d1 : Timedesc.t) (d2 : Timedesc.t) : float =
  Timedesc.Date.diff_days (Timedesc.date d1) (Timedesc.date d2)
  |> abs
  |> float_of_int


let xnpv (cash_flows : cash_flows) (rate : float) : float =
  match cash_flows with
  | [] -> 0.0
  | first :: _ ->
      let d1 = first.date in
      let days_per_year = 365.25 in (* Using 365.25 for leap years *)
      let sum_present_values current_flows acc =
        let accum acc cf =
          let days = calcDaysBetween d1 cf.date in
          let time_in_years = days /. days_per_year in
          acc +. (cf.amount /. ((1. +. rate) ** time_in_years)) 
        in
        List.fold_left accum acc current_flows
      in
      sum_present_values cash_flows 0.0

let xnpv_derivative (cash_flows : cash_flows) (rate : float) : float =
  match cash_flows with
  | [] -> 0.0
  | first :: _ ->
      let d1 = first.date in
      let days_per_year = 365.25 in
      let rec sum_derivatives current_flows acc =
        match current_flows with
        | [] -> acc
        | cf :: tail ->
            let days = calcDaysBetween d1 cf.date
            in
            let time_in_years = days /. days_per_year in
            let derivative_term =
              (-. time_in_years *. cf.amount) /. ((1. +. rate) ** (time_in_years +. 1.))
            in
            sum_derivatives tail (acc +. derivative_term)
      in
      sum_derivatives cash_flows 0.0      

let solve_with_newton_raphson
    (f : float -> float)
    (f' : float -> float)
    (guess : float)
    (tolerance : float)
    (max_iterations : int)
  : float option =
  let rec iterate current_guess iterations =
    if iterations = 0 then None
    else
      let fx = f current_guess  in
      let f_prime_x = f' current_guess in
      if abs_float f_prime_x < 1e-10 then None (* Avoid division by zero *)
      else
        let next_guess = current_guess -. (fx /. f_prime_x) in
        if abs_float (next_guess -. current_guess) < tolerance then
          Some next_guess
        else
          iterate next_guess (iterations - 1)
  in
  iterate guess max_iterations      

let xirr ?(guess = 0.1) ?(tolerance = 1e-6) ?(max_iterations = 100) (cash_flows : cash_flows)
  : float option =
  let sorted_cash_flows =
    List.sort (fun cf1 cf2 -> Timedesc.compare_struct cf1.date cf2.date) cash_flows
  in
  if List.length sorted_cash_flows < 2 then None
  else
    let _ = List.hd sorted_cash_flows in
    let has_positive = List.exists (fun cf -> cf.amount > 0.0) sorted_cash_flows in
    let has_negative = List.exists (fun cf -> cf.amount < 0.0) sorted_cash_flows in

    (* XIRR requires at least one positive and one negative cash flow *)
    if not (has_positive && has_negative) then None
    else
      solve_with_newton_raphson
        (xnpv sorted_cash_flows)
        (xnpv_derivative sorted_cash_flows)
        guess
        tolerance
        max_iterations  