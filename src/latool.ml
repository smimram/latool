open Common

let () =
  let outfile = ref None in
  let fnames = ref [] in
  let expand = ref false in
  Arg.parse
    (Arg.align
       [
         ("--expand", Arg.Set expand, " Replace \\input by their content.")
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
    let re = Str.regexp "\\input{\\([^}]*\\)}" in
    List.iter
      (fun fname ->
         let s =
           File.contents fname |>
           Str.global_substitute re (fun s ->
               assert (Str.string_match re s 0);
               let i = Str.matched_group 1 s in
               Printf.printf "Replace %s\n%!" i;
               "REPLACED"
             )
         in
         output_string oc s
      ) !fnames
