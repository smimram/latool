module File = struct
  let contents fname =
    let ic = open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
end
