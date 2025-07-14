let split_file ~filename ~chunk_size_mb ~out_dir =
  let chunk_size = chunk_size_mb * 1024 * 1024 in
  let file_size =
    let stats = Unix.stat filename in
    stats.Unix.st_size
  in
  Printf.printf "[DEBUG] Input file size: %d bytes\n" file_size;
  flush Stdlib.stdout;
  if not (Sys.file_exists out_dir && Sys.is_directory out_dir) then Sys.mkdir out_dir 0o755;
  let ic = open_in_bin filename in
  let rec loop idx acc =
    let buf = Buffer.create (chunk_size + 128) in
    let bytes = Bytes.create chunk_size in
    let read_bytes = input ic bytes 0 chunk_size in
    if idx mod 1000 = 0 then
      Printf.printf "[DEBUG] Chunk %d: read %d MB\n" idx ((idx / 1000) * 64);
    flush Stdlib.stdout;
    if read_bytes = 0 then (close_in ic; List.rev acc)
    else (
      Buffer.add_subbytes buf bytes 0 read_bytes;
      let rec read_to_space () =
        match input_char ic with
        | exception End_of_file -> ()
        | c -> Buffer.add_char buf c;
          if c = ' ' then () else read_to_space ()
      in
      read_to_space ();
      let chunk_name = Printf.sprintf "%s/chunk_%d" out_dir idx in
      let oc = open_out_bin chunk_name in
      Buffer.output_buffer oc buf;
      close_out oc;
      loop (idx+1) (chunk_name :: acc)
    )
  in
  loop 0 []

let read_chunks ~filename ~chunk_size_mb ~f =
  let out_dir = "data/train/read_chunks" in
  let chunk_files = split_file ~filename ~chunk_size_mb ~out_dir in
  let processed_chunks = ref [] in
  let process_chunk i chunk =
    if i < 1000 then (
      let ic = open_in_bin chunk in
      let buf = Buffer.create (1024 * 1024) in
      (try while true do Buffer.add_channel buf ic 4096 done with End_of_file -> ());
      close_in ic;
      f (Buffer.contents buf);
      processed_chunks := chunk :: !processed_chunks
    )
  in
  List.iteri process_chunk chunk_files;
  !processed_chunks

let cleanup_chunks chunk_files =
  List.iter (fun chunk ->
    try Sys.remove chunk with _ -> ()
  ) chunk_files;
  try Unix.rmdir "data/train/read_chunks" with _ -> ()
