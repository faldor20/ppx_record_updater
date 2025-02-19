type test_other = { a : int; b : string } [@@deriving show, record_updater ~derive: show]

type test = { a : int; b : string; [@no_update] c : test_other [@updater] }
[@@deriving show, record_updater]

let tester =
  test_apply_update
    { a = Some 2; c = Some { a = Some 100; b = Some "next" } }
    { a = 1; b = "hi"; c = { a = 2; b = "bye" } }

let _ =
  print_endline (show_test tester);
  show_test_other_update_t {a=None;b=Some "hi"}
