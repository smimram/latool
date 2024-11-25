(** Check the contents of a LaTeX file and return corrections in markdown format. *)
let check s =
  let l = String.split_on_char '\n' s |> ref in
  let ans = ref "" in
  let buf = ref "" in
  while !l <> [] do
    while String.length !buf < 2000 && !l <> [] do
      if !buf <> "" then buf := !buf ^ "\n";
      buf := !buf ^ List.hd !l;
      l := List.tl !l
    done;
    buf := "";
    print_endline !buf;
    ans := "```\n" ^ !buf ^ "\n```\n";
    let corrections = Llama.spellcheck !buf in
    print_endline corrections;
    ans := !ans ^ corrections
  done;
  !ans
