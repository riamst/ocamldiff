open Core

let to_cost_table n o =
  let init_tbl_val = 0 in
  let o_len = List.length o in
  let n_len = List.length n in
  let open Array in
  let table = make_matrix ~dimy:(o_len + 1) ~dimx:(n_len + 1) init_tbl_val in
  for i = 1 to n_len do
    for j = 1 to o_len do
      let cell_val =
        if String.equal (List.nth_exn n (i - 1)) (List.nth_exn o (j - 1)) then
          1 + table.(i - 1).(j - 1)
        else max table.(i - 1).(j) table.(i).(j - 1)
      in
      table.(i).(j) <- cell_val
    done
  done;
  table
;;

let diff ~old_s:o ~new_s:n =
  let string_eq a b i j =
    if i < 0 || j < 0 then false
    else String.equal (List.nth_exn a i) (List.nth_exn b j)
  in
  let o = String.split_lines o in
  let n = String.split_lines n in
  let c = to_cost_table o n in
  let open List in
  let rec pdiff c x y i j =
    if i >= 0 && j >= 0 && string_eq x y (i - 1) (j - 1) then
      let diff = "  " ^ nth_exn x (i - 1) in
      diff :: pdiff c x y (i - 1) (j - 1)
    else if j > 0 && (i = 0 || c.(i).(j - 1) >= c.(i - 1).(j)) then
      let diff = "+ " ^ nth_exn y (j - 1) in
      diff :: pdiff c x y i (j - 1)
    else if i > 0 && (j = 0 || c.(i).(j - 1) < c.(i - 1).(j)) then
      let diff = "- " ^ nth_exn x (i - 1) in
      diff :: pdiff c x y (i - 1) j
    else []
  in
  pdiff c o n (length o) (length n)
  |> fold ~f:(fun acc e -> e ^ "\n" ^ acc) ~init:""
;;

let diffs = to_cost_table
