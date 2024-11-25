(** Check the contents of a LaTeX file and return corrections in markdown format. *)
let check ?(stdout=true) ?(limit=2000) s =
  (* Printf.printf "checking: '%s'\n" s; *)
  let l = String.split_on_char '\n' s |> ref in
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
    let corrections = Llama.spellcheck buf in
    if stdout then Printf.printf "%s\n\n%!" corrections;
    let chunk = Printf.sprintf "%s\n%s\n\n" buf corrections in
    ans := !ans ^ chunk
  done;
  !ans
