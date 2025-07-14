let parallel_map f lst n_workers =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  let results = Array.make len None in
  let next = Atomic.make 0 in
  let worker () =
    let rec loop () =
      let i = Atomic.fetch_and_add next 1 in
      if i < len then (
        results.(i) <- Some (f arr.(i));
        loop ()
      )
    in loop ()
  in
  let domains = Array.init n_workers (fun _ -> Domain.spawn worker) in
  Array.iter Domain.join domains;
  Array.to_list (Array.map Option.get results)

let () =
  (* convert from XML to txt *)
  let input_xml = "data/train/data.xml" in
  let output_txt = "data/train/data.txt" in
  Printf.printf "[INFO] Converting %s to %s by stripping XML tags...\n" input_xml output_txt;
  flush stdout;
  Preprocess.Xml_utils.xml_to_txt ~input_xml ~output_txt;
  Printf.printf "[INFO] Conversion complete.\n";
  flush stdout;

  (* tokenize and build vocab *)
  let chunk_size_mb = 100 in
  let all_tokens = ref [] in
  let chunk_files = ref [] in
  Printf.printf "[INFO] Splitting and processing file in %dMB chunks (using only first chunk)...\n" chunk_size_mb;
  flush stdout;
  chunk_files := Io.File_utils.read_chunks ~filename:output_txt ~chunk_size_mb:chunk_size_mb ~f:(fun chunk_text ->
    flush stdout;
    let tokens = Preprocess.Text_utils.tokenize chunk_text in
    flush stdout;
    let n_workers = 10 in
    flush stdout;
    let chunked_tokens =
      let chunk_size = max 1 (List.length tokens / n_workers) in
      let rec split acc l =
        if List.length l <= chunk_size then List.rev (l :: acc)
        else let rec take_drop n l acc =
          match n, l with
          | 0, _ | _, [] -> (List.rev acc, l)
          | n, x::xs -> take_drop (n-1) xs (x::acc)
        in
        let hd, tl = take_drop chunk_size l [] in
        split (hd :: acc) tl
      in split [] tokens |> List.rev
    in
    flush stdout;
    let processed = parallel_map (fun tks -> tks) chunked_tokens n_workers in
    all_tokens := List.flatten processed @ !all_tokens
  );
  let tokens = List.rev !all_tokens in
  Printf.printf "[INFO] Total tokens from all chunks: %d\n" (List.length tokens);
  flush stdout;
  Printf.printf "[INFO] Building vocabulary in parallel...\n";
  flush stdout;
  let vocab_tbl, vocab_size, sorted_vocab = Preprocess.Text_utils.build_vocab_with_freq tokens in
  Printf.printf "[INFO] Vocabulary built. Vocab size: %d\n" vocab_size;
  flush stdout;

  (* train embedding *)
  let embed_dim = 150 in
  let window_size = 15 in
  let neg_table_size = 1_000_000 in
  let neg_samples = 5 in
  let epochs = 2 in
  Printf.printf "[INFO] Creating SkipGram model (vocab_size=%d, embed_dim=%d)...\n" vocab_size embed_dim;
  flush stdout;
  let model = Wordvec.SkipGram.create ~vocab_size ~embed_dim in
  Printf.printf "[INFO] Generating training pairs...\n";
  flush stdout;
  let pairs = Preprocess.Text_utils.generate_pairs tokens window_size vocab_tbl in
  Printf.printf "[INFO] Training pairs generated: %d\n" (Seq.length pairs);
  flush stdout;
  Printf.printf "[INFO] Building negative sampling table...\n";
  flush stdout;
  let neg_table = Preprocess.Text_utils.build_negative_sampling_table tokens vocab_tbl neg_table_size in
  Printf.printf "[INFO] Negative sampling table built (size=%d, neg_samples=%d)\n" neg_table_size neg_samples;
  flush stdout;

  Printf.printf "[INFO] Starting training...\n";
  flush stdout;
  Wordvec.SkipGram.train model pairs ~neg_table ~neg_samples ~epochs:epochs;
  Printf.printf "[INFO] Training complete.\n";
  Printf.printf "Trained word2vec model with vocab_size=%d, embed_dim=%d\n" vocab_size embed_dim;

  Printf.printf "[INFO] Saving vocabulary frequencies...\n";
  Preprocess.Text_utils.save_vocab_freq ~filename:"data/artifacts/vocab_freq.csv" ~sorted_vocab;
  Printf.printf "[INFO] Saving embeddings...\n";
  Wordvec.SkipGram.save ~filename:"data/artifacts/embeddings.csv" ~model ~embed_dim ~sorted_vocab ~vocab_tbl;

  Printf.printf "[INFO] Cleaning up chunk files...\n";
  Io.File_utils.cleanup_chunks !chunk_files;
  Printf.printf "[INFO] Cleanup complete.\n";
  flush stdout