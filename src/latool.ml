open Common

let () =
  let outfile = ref None in
  let fnames = ref [] in
  let expand = ref false in
  let remove_comments = ref false in
  Arg.parse
    (Arg.align
       [
         ("--expand", Arg.Set expand, " Replace \\input by their content.");
         ("-o", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--output", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--remove-comments", Arg.Set remove_comments, " Remove comments.");
       ]
    )
    (fun s -> fnames := s :: !fnames)
    "latool [options] files";
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
  if !expand then
    List.iter (fun fname -> process fname |> output_string oc) !fnames
