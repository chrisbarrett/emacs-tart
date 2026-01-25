(** Tart test suite *)

let test_version () =
  Alcotest.(check string) "version defined" "0.1.0" Tart.version

let () =
  Alcotest.run "tart"
    [ ("basics", [ Alcotest.test_case "version" `Quick test_version ]) ]
