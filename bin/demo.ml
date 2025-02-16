type test_other = { a : int; b : string } [@@deriving show, record_updater]

type test = { a : int; b : string; c : test_other [@update] }
[@@deriving show, record_updater]

(* type test_updater = { a : int option; b : string option } *)

(* type test_updater={ *)
(* a:int option *)
(* } *)

(* let b={a=1} *)
(* let c= b.a *)
(* let d=  Updater.{a=Some b.a} *)
(* let updater (og:test) updater : test = *)
(* let a=(updater.a |> Option.value ~default:og.a) in *)
(* { *)
(* a  ; *)
(* b = updater.b |> Option.value ~default:og.b; *)
(* } *)
(* ;; *)

let tester =
  test_apply_update
    { a = Some 2; b = None; c = Some { a = Some 100; b = Some "next" } }
    { a = 1; b = "hi"; c = { a = 2; b = "bye" } }

let _ = print_endline (show_test tester)

(* let update (og:test) (update:Updater.t)= *)
(* match update.a   with *)
(* |Some newA-> *)
(* {og with a=newA} *)
(* |None-> og *)
