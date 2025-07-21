let tokenize text =
  let split_re = Str.regexp "[^a-zA-Z0-9_]+" in
  let digit_re = Str.regexp "^[0-9]+$" in
  text |> String.lowercase_ascii |> Str.split split_re
  |> List.filter (fun s -> String.length s > 0)
  |> List.filter (fun w -> not (Str.string_match digit_re w 0))

let build_vocab tokens =
  let tbl = Hashtbl.create 10000 in
  let idx = ref 0 in
  List.iter
    (fun w ->
      if not (Hashtbl.mem tbl w) then (
        Hashtbl.add tbl w !idx ;
        incr idx))
    tokens ;
  (tbl, !idx)

let build_vocab_with_freq tokens min_vocab_freq =
  let tbl = Hashtbl.create 10000 in
  let freq_tbl = Hashtbl.create 10000 in
  let idx = ref 0 in
  List.iter
    (fun w ->
      let c = try Hashtbl.find freq_tbl w with Not_found -> 0 in
      Hashtbl.replace freq_tbl w (c + 1))
    tokens ;
  let sorted =
    Hashtbl.fold
      (fun w c acc -> if c >= min_vocab_freq then (w, c) :: acc else acc)
      freq_tbl []
    |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
  in
  List.iter
    (fun (w, _) ->
      try
        let _ = Hashtbl.find tbl w in
        ()
      with Not_found ->
        Hashtbl.add tbl w !idx ;
        incr idx)
    sorted ;
  (tbl, !idx, sorted)

let generate_pairs_seq tokens window_size vocab =
  let arr = Array.of_list tokens in
  let len = Array.length arr in
  let open Seq in
  let rec outer i () =
    if i >= len then Nil
    else
      let input_word = arr.(i) in
      match Hashtbl.find_opt vocab input_word with
      | None -> outer (i + 1) ()
      | Some input_idx ->
        let left = max 0 (i - window_size) in
        let right = min (len - 1) (i + window_size) in
        let rec inner j () =
          if j > right then outer (i + 1) ()
          else if i = j then inner (j + 1) ()
          else
            let context_word = arr.(j) in
            match Hashtbl.find_opt vocab context_word with
            | None -> inner (j + 1) ()
            | Some context_idx -> Cons ((input_idx, context_idx), inner (j + 1))
        in
        inner left ()
  in
  outer 0

let generate_pairs tokens window_size vocab =
  generate_pairs_seq tokens window_size vocab

let tokenize = tokenize
let build_vocab = build_vocab
let generate_pairs = generate_pairs

let build_negative_sampling_table tokens vocab table_size =
  let word_freq = Hashtbl.create 100 in
  List.iter
    (fun w ->
      match Hashtbl.find_opt vocab w with
      | None -> ()
      | Some _ ->
        let c = try Hashtbl.find word_freq w with Not_found -> 0 in
        Hashtbl.replace word_freq w (c + 1))
    tokens ;
  let power = 0.75 in
  let total =
    Hashtbl.fold (fun _ c acc -> acc +. (float c ** power)) word_freq 0.0
  in
  let vocab_size = Hashtbl.length vocab in
  let table = Array.make table_size 0 in
  let i = ref 0 in
  Hashtbl.iter
    (fun w idx ->
      let freq = try Hashtbl.find word_freq w with Not_found -> 0 in
      let p = (float freq ** power) /. total in
      let n = int_of_float (p *. float table_size) in
      for _ = 1 to n do
        if !i < table_size then (
          table.(!i) <- idx ;
          incr i)
      done)
    vocab ;
  while !i < table_size do
    table.(!i) <- Random.int vocab_size ;
    incr i
  done ;
  table

let sample_negative table n target_idx =
  let table_len = Array.length table in
  let seen = Hashtbl.create n in
  let result = Array.make n 0 in
  let count = ref 0 in

  while !count < n do
    let idx = table.(Random.int table_len) in
    if idx <> target_idx && not (Hashtbl.mem seen idx) then (
      Hashtbl.add seen idx () ;
      result.(!count) <- idx ;
      incr count)
  done ;
  Array.to_list result

let save_vocab_freq ~filename ~sorted_vocab =
  let oc = open_out filename in
  Printf.fprintf oc "word,frequency\n" ;
  List.iter (fun (w, c) -> Printf.fprintf oc "%s,%d\n" w c) sorted_vocab ;
  close_out oc

let preprocess_file ~input_txt ~output_txt =
  let ic = open_in input_txt in
  let oc = open_out output_txt in
  try
    while true do
      let line = input_line ic in
      let tokens = tokenize line in
      if tokens <> [] then Printf.fprintf oc "%s\n" (String.concat " " tokens)
    done
  with End_of_file ->
    close_in ic ;
    close_out oc

module Text_utils = struct
  let tokenize = tokenize
  let build_vocab = build_vocab
  let generate_pairs = generate_pairs
  let build_negative_sampling_table = build_negative_sampling_table
  let sample_negative = sample_negative
  let build_vocab_with_freq = build_vocab_with_freq
  let save_vocab_freq = save_vocab_freq
  let preprocess_file = preprocess_file
end
