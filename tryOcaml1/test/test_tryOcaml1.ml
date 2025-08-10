


(* Define your test suite structure *)
let () =
  Alcotest.run ~verbose:true "ALL the Tests" [
    "First", FirstTest.tests;
    "Dates", Test_infinite.dates;
    "Houses", Test_house.houseTests;
    (* You could add other test groups here *)
  ]