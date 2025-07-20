open Preprocess.Text_utils

module type SkipGramSig = sig
  type t

  val create : vocab_size:int -> embed_dim:int -> t

  val train :
    t ->
    (int * int) Seq.t ->
    neg_table:int array ->
    neg_samples:int ->
    epochs:int ->
    ?batch_size:int ->
    unit ->
    unit

  val get_embedding : t -> int -> float array

  val save :
    filename:string ->
    model:t ->
    embed_dim:int ->
    sorted_vocab:(string * 'a) list ->
    vocab_tbl:(string, int) Hashtbl.t ->
    unit

  val load : filename:string -> string array * float array array
end

module SkipGram : SkipGramSig = struct
  type t = {
    w_in : float array array;
    w_out : float array array;
    embed_dim : int;
  }

  let create ~vocab_size ~embed_dim =
    {
      w_in =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ -> Random.float 1.0));
      w_out =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ -> Random.float 1.0));
      embed_dim;
    }

  let sigmoid x = 1.0 /. (1.0 +. exp (-.x))

  let train model pairs ~neg_table ~neg_samples ~epochs ?(batch_size = 1000) ()
      =
    let lr = 0.01 in
    let vocab_size = Array.length model.w_in in
    let embed_dim = model.embed_dim in
    let log_interval = 1_000_000 in

    Printf.printf "[INFO] Counting total training pairs...\n" ;
    flush stdout ;
    let total_pairs_count = Seq.fold_left (fun acc _ -> acc + 1) 0 pairs in
    Printf.printf "[INFO] Total training pairs: %d\n" total_pairs_count ;
    flush stdout ;

    for epoch = 1 to epochs do
      let batch = ref [] in
      let batch_count = ref 0 in
      let total_pairs = ref 0 in
      let epoch_start_time = Unix.gettimeofday () in
      let last_log_time = ref epoch_start_time in

      let grad_w_in = Array.make_matrix vocab_size embed_dim 0.0 in
      let grad_w_out = Array.make_matrix vocab_size embed_dim 0.0 in
      let counts_in = Array.make vocab_size 0 in
      let counts_out = Array.make vocab_size 0 in

      let apply_batch () =
        for idx = 0 to vocab_size - 1 do
          if counts_in.(idx) > 0 then (
            let count = float_of_int counts_in.(idx) in
            for i = 0 to embed_dim - 1 do
              let avg_grad = grad_w_in.(idx).(i) /. count in
              model.w_in.(idx).(i) <- model.w_in.(idx).(i) -. (lr *. avg_grad) ;
              grad_w_in.(idx).(i) <- 0.0
            done ;
            counts_in.(idx) <- 0) ;
          if counts_out.(idx) > 0 then (
            let count = float_of_int counts_out.(idx) in
            for i = 0 to embed_dim - 1 do
              let avg_grad = grad_w_out.(idx).(i) /. count in
              model.w_out.(idx).(i) <- model.w_out.(idx).(i) -. (lr *. avg_grad) ;
              grad_w_out.(idx).(i) <- 0.0
            done ;
            counts_out.(idx) <- 0)
        done
      in

      Seq.iter
        (fun (input_idx, context_idx) ->
          batch := (input_idx, context_idx) :: !batch ;
          incr batch_count ;
          incr total_pairs ;

          if !total_pairs mod log_interval = 0 then (
            let current_time = Unix.gettimeofday () in
            let elapsed_since_last = current_time -. !last_log_time in
            let pairs_per_sec =
              float_of_int log_interval /. elapsed_since_last
            in
            let percentage =
              float_of_int !total_pairs
              /. float_of_int total_pairs_count
              *. 100.0
            in
            Printf.printf
              "[Epoch %d/%d] Processed %d pairs (%.1f%%, %.1f pairs/sec, \
               batch_size=%d)\n"
              epoch epochs !total_pairs percentage pairs_per_sec batch_size ;
            flush stdout ;
            last_log_time := current_time) ;

          let input_vec = model.w_in.(input_idx) in

          let process_pair output_idx label =
            let output_vec = model.w_out.(output_idx) in
            let dot = ref 0.0 in
            for i = 0 to embed_dim - 1 do
              dot := !dot +. (input_vec.(i) *. output_vec.(i))
            done ;
            let pred = sigmoid !dot in
            let error = pred -. label in

            for i = 0 to embed_dim - 1 do
              let grad_in = error *. output_vec.(i) in
              let grad_out = error *. input_vec.(i) in
              grad_w_in.(input_idx).(i) <- grad_w_in.(input_idx).(i) +. grad_in ;
              grad_w_out.(output_idx).(i) <-
                grad_w_out.(output_idx).(i) +. grad_out
            done ;
            counts_in.(input_idx) <- counts_in.(input_idx) + 1 ;
            counts_out.(output_idx) <- counts_out.(output_idx) + 1
          in

          process_pair context_idx 1.0 ;

          let negs =
            Text_utils.sample_negative neg_table neg_samples context_idx
          in
          List.iter (fun neg_idx -> process_pair neg_idx 0.0) negs ;

          if !batch_count >= batch_size then (
            apply_batch () ;
            batch := [] ;
            batch_count := 0))
        pairs ;

      if !batch_count > 0 then apply_batch () ;

      let epoch_end_time = Unix.gettimeofday () in
      let epoch_duration = epoch_end_time -. epoch_start_time in
      let avg_pairs_per_sec = float_of_int !total_pairs /. epoch_duration in
      let final_percentage =
        float_of_int !total_pairs /. float_of_int total_pairs_count *. 100.0
      in
      Printf.printf
        "[Epoch %d/%d] finished (processed %d pairs, %.1f%%, %.2fs, avg %.1f \
         pairs/sec, batch_size=%d)\n"
        epoch epochs !total_pairs final_percentage epoch_duration
        avg_pairs_per_sec batch_size ;
      flush stdout
    done

  let get_embedding model idx = Array.copy model.w_in.(idx)

  let save ~filename ~model ~embed_dim ~sorted_vocab ~vocab_tbl =
    let oc = open_out filename in
    Printf.fprintf oc "word" ;
    for d = 0 to embed_dim - 1 do
      Printf.fprintf oc ",dim%d" d
    done ;
    Printf.fprintf oc "\n" ;
    List.iter
      (fun (word, _) ->
        let idx = Hashtbl.find vocab_tbl word in
        let emb = get_embedding model idx in
        Printf.fprintf oc "%s" word ;
        Array.iter (fun x -> Printf.fprintf oc ",%f" x) emb ;
        Printf.fprintf oc "\n")
      sorted_vocab ;
    close_out oc

  let load ~filename =
    let ic = open_in filename in
    let header = input_line ic in
    let dims = List.length (String.split_on_char ',' header) - 1 in
    let words = ref [] in
    let vectors = ref [] in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char ',' line with
         | word :: floats when List.length floats = dims ->
           let vec =
             Array.init dims (fun i -> float_of_string (List.nth floats i))
           in
           words := word :: !words ;
           vectors := vec :: !vectors
         | _ -> ()
       done
     with End_of_file -> close_in ic) ;
    (Array.of_list (List.rev !words), Array.of_list (List.rev !vectors))
end
