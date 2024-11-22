open Common

let () =
  let outfile = ref None in
  let fnames = ref [] in
  let expand = ref false in
  Arg.parse
    (Arg.align
       [
         ("--expand", Arg.Set expand, " Replace \\input by their content.");
         ("-o", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--output", Arg.String (fun s -> outfile := Some s), " Output file.");
       ]
    )
    (fun s -> fnames := s :: !fnames)
    "latool [options] files";
  let oc =
    match !outfile with
    | Some fname -> open_out fname
    | None -> stdout
  in
  if !expand then
    let re = Str.regexp "\\\\input{\\([^}]*\\)}" in
    List.iter
      (fun fname ->
         let s =
           File.contents fname |>
           Str.global_substitute re (fun s ->
               let i = Str.matched_group 1 s in
               let i = if not (Sys.file_exists i) then i ^ ".tex" else i in
               if not (Sys.file_exists i) then
                 (
                   Printf.eprintf "Could not find file: %s\n%!" i;
                   "NOT_FOUND"
                 )
               else
                 File.contents i
             )
         in
         output_string oc s
      ) !fnames
