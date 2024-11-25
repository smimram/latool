open Common

let () =
  let outfile = ref None in
  let fnames = ref [] in
  let expand = ref false in
  let grammar = ref false in
  let remove_comments = ref false in
  Arg.parse
    (Arg.align
       [
         ("--expand", Arg.Set expand, " Replace \\input by their content.");
         ("--grammar", Arg.Unit (fun () -> grammar := true; expand := true; remove_comments := true), " Perform grammar check using LLM.");
         ("-o", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--output", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--remove-comments", Arg.Set remove_comments, " Remove comments.");
       ]
    )
    (fun s -> fnames := s :: !fnames)
    "latool [options] files";
  if !fnames = [] then (print_endline "Please provide a file name."; exit 1);
  let oc =
    match !outfile with
    | Some fname -> open_out fname
    | None -> stdout
  in
  let rec process =
    let re_comment = Str.regexp "%[^\n]*$" in
    let re_input = Str.regexp "\\\\input{\\([^}]*\\)}" in
    fun fname ->
      let s = File.contents fname in
      let s =
        if not !remove_comments then s
        else Str.global_replace re_comment "" s
      in
      let s =
        if not !expand then s else
          Str.global_substitute re_input (fun s ->
              let i = Str.matched_group 1 s in
              let i = if not (Sys.file_exists i) then i ^ ".tex" else i in
              if not (Sys.file_exists i) then
                (
                  Printf.eprintf "Could not find file: %s\n%!" i;
                  "NOT_FOUND"
                )
              else
                process i
            ) s
      in
      s
  in
  let grammar s =
    if not !grammar then s else
      let s = Str.global_replace (Str.regexp "~") " " s in
      let s = Str.global_replace (Str.regexp {|\\cite{[^}]*}|}) "" s in
      let s = Str.global_replace (Str.regexp " ,") "," s in
      let s = Str.global_replace (Str.regexp {|\\documentclass.*$|}) "" s in
      let s = Str.global_replace (Str.regexp {|\\usepackage.*$|}) "" s in
      let s = Str.global_replace (Str.regexp {|\\usetikzlibrary.*$|}) "" s in
      let s = Str.global_replace (Str.regexp {|\\\(re\)?newcommand.*$|}) "" s in
      let s = Str.global_substitute (Str.regexp {|\\title{\([^}]*\)}|}) (fun s -> Str.matched_group 1 s ^ "\n") s in
      let s = Str.global_replace (Str.regexp {|\\author{[^}]*}|}) "" s in
      let s = Str.global_replace (Str.regexp {|\\tableofcontents|}) "" s in
      let s = Str.global_substitute (Str.regexp {|\\section{\([^}]*\)}|}) (fun s -> Str.matched_group 1 s ^ "\n") s in
      let s = Str.global_substitute (Str.regexp {|\\subsection{\([^}]*\)}|}) (fun s -> Str.matched_group 1 s ^ "\n") s in
      (* let s = Str.global_replace (Str.regexp {|\\\[.*\\\]|}) "" s in *)
      let s = Str.global_replace (Str.regexp "\\\\\\[\\(.\\|\n\\)*\\\\\\]") "" s in
      let s = Str.global_replace (Str.regexp "\\\\begin{align*}\\(.\\|\n\\)*\\\\end{align*}") "" s in
      let s = Str.global_replace (Str.regexp {|\\begin{[^}]*}|}) "" s in
      let s = Str.global_replace (Str.regexp {|\\end{[^}]*}|}) "" s in
      Grammar.check s
  in
  if !expand then
    List.iter (fun fname -> process fname |> grammar |> output_string oc) !fnames
