open Common

let completion ?(system="You are a helpful assistant.") user =
  let get =
    let open Lwt in
    let open Cohttp in
    let open Cohttp_lwt_unix in
    let uri = Uri.of_string "http://localhost:8080/v1/chat/completions" in
    let (q : Yojson.t) =
      `Assoc [
        "model", `String "gpt-4o";
        "messages", `List [
          `Assoc
            [
              "role", `String "system";
              "content", `String system
            ];
          `Assoc
            [
              "role", `String "user";
              "content", `String user
            ]
        ]
      ]
    in
    let q = Yojson.to_string q in
    Client.post ~body:(`String q) uri >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    if code <> 200 then failwith "Unexpected llama.cpp code: %d" code;
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (* body |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string *)
    body
    |> Yojson.Basic.from_string
    |> JSON.assoc "choices"
    |> JSON.to_list
    |> List.hd
    |> JSON.assoc "message"
    |> JSON.assoc "content"
    |> JSON.to_string
  in
  Lwt_main.run get

let spellcheck text =
  try
  if text = "" then "" else
    (* let system = "You are an editor and proofreader. I will provide you with text in LaTeX format that needs to be checked for spelling and grammar errors. Your task is to carefully review the text and correct any mistakes, ensuring that the corrected text is free of errors and maintains the original meaning. You will only show a list of corrections to make (first original and then corrected)." in *)
    let system = "Correct spelling and grammar errors. You show a list of corrections." in
    completion ~system text
  with
  | Unix.Unix_error(Unix.ECONNREFUSED, _, _) -> failwith "Could not connect to llama.cpp server. Did you launch one?"
