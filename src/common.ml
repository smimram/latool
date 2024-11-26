module Settings = struct
  let show_progress = ref true
end

module File = struct
  let contents fname =
    let ic = open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
end

module JSON = struct
  let to_assoc = function
    | `Assoc l -> l
    | _ -> assert false

  let assoc k json = List.assoc k (to_assoc json)

  let to_list = function
    | `List l -> l
    | _ -> assert false

  let to_string = function
    | `String s -> s
    | _ -> assert false
end

module Re = struct
  include Re

  let remove re s = replace re ~f:(fun _ -> "") s
end
