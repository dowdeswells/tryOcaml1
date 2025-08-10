open TryOcaml1.First

let pp_value fmt (v:value) =
  match v with
  | Int i -> Fmt.pf fmt "{ %d }" i
  | Bool b -> Fmt.pf fmt "{ %s }" (if b then "true" else "false")

let equal_value (u1:value) (u2:value) =
  match (u1,u2) with
  | (Bool _, Int _) -> false
  | (Int _, Bool _) -> false
  | (Bool b1, Bool b2) -> b1 = b2
  | (Int i1, Int i2) -> i1 = i2

let value_testable = Alcotest.testable pp_value equal_value

let aaa =
  let i x = Value (Int x)
  (* and b x = Value (Bool x) *)
  and (+:) x y = Plus (x,y)
in
eval (i 3 +: i 6)

let bbb ?(sep="Hello") label1 label2  = 
  show_dirs path;
  label1 ^ sep ^ label2

let p = Alcotest.(check string)


let print_gadt () = 
  let _ = bbb "a" "b" ~sep:"-" in
  Alcotest.(check value_testable) "first transaction" aaa  aaa
  (* p "2nd transaction" bbb "Hello" *)

let tests = [
  Alcotest.test_case "GADT" `Quick print_gadt;
]