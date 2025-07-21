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
    let xavier_bound_in =
      sqrt (6.0 /. (float_of_int vocab_size +. float_of_int embed_dim))
    in
    let xavier_bound_out =
      sqrt (6.0 /. (float_of_int embed_dim +. float_of_int vocab_size))
    in
    {
      w_in =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ ->
                Random.float (2.0 *. xavier_bound_in) -. xavier_bound_in));
      w_out =
        Array.init vocab_size (fun _ ->
            Array.init embed_dim (fun _ ->
                Random.float (2.0 *. xavier_bound_out) -. xavier_bound_out));
      embed_dim;
    }

  let sigmoid x = 1.0 /. (1.0 +. exp (-.x))

  let sgd_step model input_idx context_idx neg_table neg_samples lr =
    let input_vec = model.w_in.(input_idx) in
    let update output_idx label =
      let output_vec = model.w_out.(output_idx) in
      let dot = ref 0.0 in
      for i = 0 to model.embed_dim - 1 do
        dot := !dot +. (input_vec.(i) *. output_vec.(i))
      done ;
      let pred = sigmoid !dot in
      let error = pred -. label in
      for i = 0 to model.embed_dim - 1 do
        let grad_in = error *. output_vec.(i) in
        let grad_out = error *. input_vec.(i) in
        model.w_in.(input_idx).(i) <-
          model.w_in.(input_idx).(i) -. (lr *. grad_in) ;
        model.w_out.(output_idx).(i) <-
          model.w_out.(output_idx).(i) -. (lr *. grad_out)
      done
    in
    update context_idx 1.0 ;
    let negs = Text_utils.sample_negative neg_table neg_samples context_idx in
    List.iter (fun neg_idx -> update neg_idx 0.0) negs

  let train model pairs ~neg_table ~neg_samples ~epochs =
    let lr = 0.01 in
    let log_interval = 1_000_000 in

    Printf.printf "[INFO] Counting total training pairs...\n" ;
    flush stdout ;
    let total_pairs_count = Seq.fold_left (fun acc _ -> acc + 1) 0 pairs in
    Printf.printf "[INFO] Total training pairs: %d\n" total_pairs_count ;
    flush stdout ;

    for epoch = 1 to epochs do
      let processed_pairs = ref 0 in
      let last_log_time = ref (Unix.gettimeofday ()) in
      let epoch_start_time = Unix.gettimeofday () in

      Seq.iter
        (fun (input_idx, context_idx) ->
          sgd_step model input_idx context_idx neg_table neg_samples lr ;
          incr processed_pairs ;

          if !processed_pairs mod log_interval = 0 then (
            let current_time = Unix.gettimeofday () in
            let elapsed_since_last = current_time -. !last_log_time in
            let pairs_per_sec =
              float_of_int log_interval /. elapsed_since_last
            in
            let percentage =
              float_of_int !processed_pairs
              /. float_of_int total_pairs_count
              *. 100.0
            in
            Printf.printf
              "[Epoch %d/%d] Processed %d pairs (%.1f%%, %.1f pairs/sec)\n"
              epoch epochs !processed_pairs percentage pairs_per_sec ;
            flush stdout ;
            last_log_time := current_time))
        pairs ;

      let epoch_end_time = Unix.gettimeofday () in
      let epoch_duration = epoch_end_time -. epoch_start_time in
      let avg_pairs_per_sec = float_of_int !processed_pairs /. epoch_duration in
      let final_percentage =
        float_of_int !processed_pairs /. float_of_int total_pairs_count *. 100.0
      in
      Printf.printf
        "[Epoch %d/%d] finished (processed %d pairs, %.1f%%, %.2fs, avg %.1f \
         pairs/sec)\n"
        epoch epochs !processed_pairs final_percentage epoch_duration
        avg_pairs_per_sec ;
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
