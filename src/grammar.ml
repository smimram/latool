(** Check the contents of a LaTeX file and return corrections in markdown format. *)
let check ?(stdout=true) ?(limit=20) s =
  let l = String.split_on_char '\n' s |> ref in
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
    Printf.printf "progress: %d%%\r%!" (100 - List.length !l * 100 / lines);
    let corrections = Llama.spellcheck buf in
    if stdout then Printf.printf "%s\n\n%!" corrections;
    let chunk = Printf.sprintf "%s\n%s\n\n" buf corrections in
    ans := !ans ^ chunk
  done;
  !ans
