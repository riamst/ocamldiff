open Core
open Ocamldiff

let () =
  let l1 = {|Dear shitbag,
you
suck
big
doodoo

sincerely,
g|} in
  let l2 = {|Dear goodguy,
you
rock
big
W

sincerely,
w|} in
  Diff.diff l1 l2 |> print_endline
;;
