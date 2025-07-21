open Wordvec

let usage_msg = "train [options]"
let input_file = ref "data/train/simplewikipedia.txt"
let embed_dim = ref 150
let window_size = ref 15
let neg_samples = ref 5
let epochs = ref 1
let chunk_size_mb = ref 100
let n_workers = ref 10
let min_vocab_freq = ref 10
let output_vocab = ref "data/artifacts/vocab_freq.csv"
let output_embeddings = ref "data/artifacts/embeddings.csv"
let neg_table_size = ref 1_000_000

let speclist =
  [
    ("--input", Arg.Set_string input_file, " Input file path (.xml or .txt)");
    ("--dim", Arg.Set_int embed_dim, Printf.sprintf " Embedding dimension (default: %d)" !embed_dim);
    ("--window-size", Arg.Set_int window_size, Printf.sprintf " Context window size (default: %d)" !window_size);
    ("--epochs", Arg.Set_int epochs, Printf.sprintf " Number of training epochs (default: %d)" !epochs);
    ("--neg-samples", Arg.Set_int neg_samples, Printf.sprintf " Number of negative samples (default: %d)" !neg_samples);
    ("--neg-table-size", Arg.Set_int neg_table_size, Printf.sprintf " Negative sampling table size (default: %d)" !neg_table_size);
    ("--chunk-size", Arg.Set_int chunk_size_mb, Printf.sprintf " Chunk size in MB for processing (default: %d)" !chunk_size_mb);
    ("--workers", Arg.Set_int n_workers, Printf.sprintf " Number of parallel workers (default: %d)" !n_workers);
    ("--min-freq", Arg.Set_int min_vocab_freq, Printf.sprintf " Minimum vocabulary frequency (default: %d)" !min_vocab_freq);
    ("--vocab-output", Arg.Set_string output_vocab, Printf.sprintf " Output vocabulary file (default: %s)" !output_vocab);
    ("--embed-output", Arg.Set_string output_embeddings, Printf.sprintf " Output embeddings file (default: %s)" !output_embeddings);
  ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg ;
  
  Printf.printf "[INFO] Training Configuration:\n" ;
  Printf.printf "  Input file: %s\n" !input_file ;
  Printf.printf "  Embedding dimension: %d\n" !embed_dim ;
  Printf.printf "  Window size: %d\n" !window_size ;
  Printf.printf "  Negative samples: %d\n" !neg_samples ;
  Printf.printf "  Epochs: %d\n" !epochs ;
  Printf.printf "  Chunk size: %dMB\n" !chunk_size_mb ;
  Printf.printf "  Workers: %d\n" !n_workers ;
  Printf.printf "  Min vocab frequency: %d\n" !min_vocab_freq ;
  Printf.printf "  Negative table size: %d\n" !neg_table_size ;
  Printf.printf "  Vocab output: %s\n" !output_vocab ;
  Printf.printf "  Embeddings output: %s\n" !output_embeddings ;
  flush stdout ;

  let all_tokens = ref [] in
  let chunk_files = ref [] in
  Printf.printf
    "[INFO] Splitting and processing file in %dMB chunks (using only first \
     chunk)...\n"
    !chunk_size_mb ;
  flush stdout ;
  chunk_files :=
    Io.File_utils.read_chunks ~filename:!input_file ~chunk_size_mb:!chunk_size_mb
      ~f:(fun chunk_text ->
        flush stdout ;
        let tokens = Preprocess.Text_utils.tokenize chunk_text in
        flush stdout ;
        flush stdout ;
        let chunked_tokens =
          let chunk_size = max 1 (List.length tokens / !n_workers) in
          let rec split acc l =
            if List.length l <= chunk_size then List.rev (l :: acc)
            else
              let rec take_drop n l acc =
                match (n, l) with
                | 0, _ | _, [] -> (List.rev acc, l)
                | n, x :: xs -> take_drop (n - 1) xs (x :: acc)
              in
              let hd, tl = take_drop chunk_size l [] in
              split (hd :: acc) tl
          in
          split [] tokens |> List.rev
        in
        flush stdout ;
        let processed =
          Core.Parallel.parallel_map (fun tks -> tks) chunked_tokens !n_workers
        in
        all_tokens := List.flatten processed @ !all_tokens) ;
  let tokens = List.rev !all_tokens in
  Printf.printf "[INFO] Total tokens from all chunks: %d\n" (List.length tokens) ;
  flush stdout ;
  Printf.printf "[INFO] Building vocabulary in parallel...\n" ;
  flush stdout ;
  let vocab_tbl, vocab_size, sorted_vocab =
    Preprocess.Text_utils.build_vocab_with_freq tokens !min_vocab_freq
  in
  Printf.printf "[INFO] Vocabulary built. Vocab size: %d\n" vocab_size ;
  flush stdout ;

  Printf.printf
    "[INFO] Creating SkipGram model (vocab_size=%d, embed_dim=%d)...\n"
    vocab_size !embed_dim ;
  flush stdout ;
  let model = Wordvec.SkipGram.create ~vocab_size ~embed_dim:!embed_dim in
  Printf.printf "[INFO] Generating training pairs...\n" ;
  flush stdout ;
  let pairs =
    Preprocess.Text_utils.generate_pairs tokens !window_size vocab_tbl
  in
  Printf.printf "[INFO] Training pairs generated: %d\n" (Seq.length pairs) ;
  flush stdout ;
  Printf.printf "[INFO] Building negative sampling table...\n" ;
  flush stdout ;
  let neg_table =
    Preprocess.Text_utils.build_negative_sampling_table tokens vocab_tbl
      !neg_table_size
  in
  Printf.printf
    "[INFO] Negative sampling table built (size=%d, neg_samples=%d)\n"
    !neg_table_size !neg_samples ;
  flush stdout ;

  Printf.printf "[INFO] Starting training (epochs=%d)...\n" !epochs ;
  flush stdout ;
  let training_start_time = Unix.gettimeofday () in
  Wordvec.SkipGram.train model pairs ~neg_table ~neg_samples:!neg_samples ~epochs:!epochs ;
  let training_end_time = Unix.gettimeofday () in
  let total_training_time = training_end_time -. training_start_time in
  Printf.printf "[INFO] Training complete in %.2f seconds.\n" total_training_time ;
  Printf.printf "Trained word2vec model with vocab_size=%d, embed_dim=%d\n"
    vocab_size !embed_dim ;

  Printf.printf "[INFO] Saving vocabulary frequencies...\n" ;
  Preprocess.Text_utils.save_vocab_freq
    ~filename:!output_vocab ~sorted_vocab ;
  Printf.printf "[INFO] Saving embeddings...\n" ;
  Wordvec.SkipGram.save ~filename:!output_embeddings ~model
    ~embed_dim:!embed_dim ~sorted_vocab ~vocab_tbl ;

  Printf.printf "[INFO] Cleaning up chunk files...\n" ;
  Io.File_utils.cleanup_chunks !chunk_files ;
  Printf.printf "[INFO] Cleanup complete.\n" ;
  flush stdout
