open Common

let () =
  let outfile = ref None in
  let fnames = ref [] in
  let expand = ref false in
  let grammar = ref false in
  let remove_comments = ref false in
  let remove_markup = ref false in
  Arg.parse
    (Arg.align
       [
         ("--expand", Arg.Set expand, " Replace \\input by their content.");
         ("--grammar", Arg.Unit (fun () -> grammar := true; expand := true; remove_comments := true; remove_markup := true), " Perform grammar check using LLM.");
         ("-o", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--output", Arg.String (fun s -> outfile := Some s), " Output file.");
         ("--remove-comments", Arg.Set remove_comments, " Remove comments.");
         ("--remove-markup", Arg.Set remove_markup, " Remove markup (roughly).");
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
    let re_comment = Re.Posix.re ~opts:[`Newline] {|%.*$|} |> Re.compile in
    let re_input = Re.Posix.re {|\\input\{([^}]*)}|} |> Re.compile in
    fun fname ->
      let s = File.contents fname in
      let s =
        if not !remove_comments then s else
          Re.remove re_comment s
      in
      let s =
        if not !expand then s else
          Re.replace re_input ~f:(fun g ->
              let i = Re.Group.get g 1 in
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
  let postprocess s =
    let s =
      if not !remove_markup then s else
        let remove re = Re.remove (Re.Posix.re ~opts:[`Newline] re |> Re.compile) in
        let remove' re = Re.remove (Re.Posix.re re |> Re.shortest |> Re.compile) in
        let replace re r = Re.replace (Re.Posix.re ~opts:[`Newline] re |> Re.compile) ~f:(fun _ -> r) in
        let replace1 re = Re.replace (Re.Posix.re ~opts:[`Newline] re |> Re.compile) ~f:(fun g -> Re.Group.get g 1) in
        s
        |> remove {|\\documentclass.*$|}
        |> remove {|\\usepackage.*$|}
        |> remove {|\\usetikzlibrary.*$|}
        |> remove {|\\(re)?newcommand.*$|}
        |> replace1 {|\\title\{([^}]*)}|}
        |> remove {|\\author\{([^}]*)}|}
        |> Re.replace (Re.Posix.re {|\\keywords\{([^}]*)}|} |> Re.compile) ~f:(fun g -> "Keywords: " ^ Re.Group.get g 1)
        |> remove {|\\address\{([^}]*)}|}
        |> remove {|\\email\{([^}]*)}|}
        |> remove {|\\maketitle|}
        |> remove {|\\tableofcontents|}
        |> remove' {|\\\[.*\\]|}
        |> remove' {|\\begin\{align\*}.*\\end\{align\*}|}
        |> Re.replace (Re.Posix.re {|\\section\{([^}]*)}|} |> Re.compile) ~f:(fun g -> "# " ^ Re.Group.get g 1 ^ "\n")
        |> Re.replace (Re.Posix.re {|\\subsection\{([^}]*)}|} |> Re.compile) ~f:(fun g -> "## " ^ Re.Group.get g 1 ^ "\n")
        |> Re.replace (Re.Posix.re {|\\subsubsection\{([^}]*)}|} |> Re.compile) ~f:(fun g -> "### " ^ Re.Group.get g 1 ^ "\n")
        |> remove {|\\begin\{[^}]*}|}
        |> remove {|\\end\{[^}]*}|}
        |> replace {|\\item(\[[^]]*])?|} "-"
        |> remove {|\\q?quad|}
        |> remove {|\\label\{[^}]*}|}
        |> remove {|\\cite(\[[^]]*])?\{[^}]*}|}
        |> replace1 {|\\emph\{([^}]*)}|}
        |> remove {|\\noindent$|}
        |> remove {|\\bibliography.*$|}
        |> remove {|\\\\|}
        |> replace "~" " "
        |> Re.replace (Re.str "[ ]+" |> Re.compile) ~f:(fun _ -> " ")
        |> Re.replace (Re.str " ," |> Re.compile) ~f:(fun _ -> ",")
        |> Re.replace (Re.str {| +\.|} |> Re.compile) ~f:(fun _ -> ".")
        |> remove {|^ *|}
        |> Re.replace (Re.Posix.re "\n\n+" |> Re.compile) ~f:(fun _ -> "\n\n")
    in
    let s = if not !grammar then s else Grammar.check s in
    s
  in
  List.iter (fun fname -> process fname |> postprocess |> output_string oc) !fnames
