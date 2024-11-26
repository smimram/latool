open Common

(** Check the contents of a LaTeX file and return corrections in markdown format. *)
let check ?(stdout=true) ?(limit=20) s =
  let l = String.split_on_char '\n' s |> ref in
  let t0 = Unix.time () in
  let lines = List.length !l in
  let ans = ref "" in
  let buf = ref "" in
  while !l <> [] do
    buf := "";
    while String.length !buf < limit && !l <> [] do
      if !buf <> "" then buf := !buf ^ "\n";
      buf := !buf ^ List.hd !l;
      l := List.tl !l
    done;
    let buf = !buf in
    let buf = Printf.sprintf "```\n%s\n```\n" buf in
    if stdout then print_endline buf;
    if !Settings.show_progress then
      let l = List.length !l in
      let dt = Unix.time () -. t0 in
      Printf.printf "progress: %d%% (%d minutes remaining)\r%!"
        (100 - l * 100 / lines)
        (dt *. float l /. (60. *. float (lines - l)) |> int_of_float);
    let corrections = Llama.spellcheck buf in
    if stdout then Printf.printf "%s\n\n%!" corrections;
    let chunk = Printf.sprintf "%s\n%s\n\n" buf corrections in
    ans := !ans ^ chunk
  done;
  !ans
